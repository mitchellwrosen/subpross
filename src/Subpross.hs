module Subpross
  ( withProcess,
    Source,
    Event (..),
  )
where

import Control.Concurrent.STM.TQueue
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad.STM (STM, atomically)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (for_)
import Data.Function
import Data.Functor (void)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (catMaybes)
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Ki
import System.Exit (ExitCode)
import System.IO (Handle, hClose)
import qualified System.Posix.Process as Posix (getProcessGroupIDOf)
import qualified System.Posix.Signals as Posix
import qualified System.Process as Process
import qualified System.Process.Internals as Process

type RunningProcess =
  (Maybe Handle, Maybe Handle, Maybe Handle, Process.ProcessHandle)

type Source a =
  (Maybe a -> IO ()) -> (Maybe Void -> IO ())

data Event a
  = Stdout a
  | Stderr a
  | Exit ExitCode
  deriving stock (Eq, Generic, Show)

withProcess :: Source ByteString -> Process.CreateProcess -> (Source (Event ByteString) -> IO r) -> IO r
withProcess stdin createProcess k =
  bracket (Process.createProcess createProcess) cleanupProcess \process -> do
    (readEvent, writeEvent) <- newQueue
    Ki.scoped \scope -> do
      Ki.fork_ scope (handleRunningProcess stdin process (atomically . writeEvent))
      k \callback _ ->
        fix \again -> do
          event <- atomically readEvent
          callback (Just event)
          case event of
            Stdout _ -> again
            Stderr _ -> again
            Exit _ -> callback Nothing

cleanupProcess :: RunningProcess -> IO ()
cleanupProcess (maybeStdin, maybeStdout, maybeStderr, process) =
  case catMaybes [maybeStdin, maybeStdout, maybeStderr] of
    [] -> terminate
    handles -> terminate `finally` closeHandles1 handles
  where
    terminate :: IO ()
    terminate = do
      Process.withProcessHandle process \case
        Process.ClosedHandle _ -> pure ()
        Process.OpenExtHandle {} -> undefined -- bug "OpenExtHandle is Windows-only" []
        Process.OpenHandle pid -> do
          pgid <- Posix.getProcessGroupIDOf pid
          Posix.signalProcessGroup Posix.sigTERM pgid
      void (Process.waitForProcess process)

-- | Close a non-empty list of handles.
closeHandles1 :: [Handle] -> IO ()
closeHandles1 = \case
  [] -> undefined
  [x] -> hClose x
  x : xs -> hClose x `finally` closeHandles1 xs

handleRunningProcess :: Source ByteString -> RunningProcess -> (Event ByteString -> IO ()) -> IO ()
handleRunningProcess stdin (maybeStdin, maybeStdout, maybeStderr, process) handleEvent =
  Ki.scoped \scope -> do
    for_ maybeStdin \handle -> Ki.fork_ scope (handleInput handle)
    for_ maybeStdout \handle -> Ki.fork_ scope (handleOutput handle (handleEvent . Stdout))
    for_ maybeStderr \handle -> Ki.fork_ scope (handleOutput handle (handleEvent . Stderr))
    exitCode <- Process.waitForProcess process
    Ki.wait scope -- intentional! wait for handle threads to flush
    handleEvent (Exit exitCode)
  where
    handleInput :: Handle -> IO ()
    handleInput handle = do
      alive <- newIORef True
      drain stdin \bytes ->
        readIORef alive >>= \case
          False -> pure ()
          True ->
            try (ByteString.hPut handle bytes) >>= \case
              Left (_ :: IOException) -> writeIORef alive False
              Right () -> pure ()
      hClose handle
    handleOutput :: Handle -> (ByteString -> IO ()) -> IO ()
    handleOutput handle action = do
      fix \again -> do
        chunk <- ByteString.hGetSome handle 4096
        if ByteString.null chunk
          then pure ()
          else do
            action chunk
            again
      hClose handle

drain :: Source a -> (a -> IO ()) -> IO ()
drain source callback =
  source (maybe (pure ()) callback) Nothing

newQueue :: IO (STM a, a -> STM ())
newQueue = do
  queue <- newTQueueIO
  pure (readTQueue queue, writeTQueue queue)
