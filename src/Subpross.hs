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
import Data.Coerce (coerce)
import Data.Function
import Data.Functor (void)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import qualified Ki
import System.Exit (ExitCode)
import System.IO (Handle)
import qualified System.IO as IO
import qualified System.Posix.Process as Posix (getProcessGroupIDOf)
import qualified System.Posix.Signals as Posix
import qualified System.Process as Process
import qualified System.Process.Internals as Process
import Prelude hiding (read)

type Source a =
  (Maybe a -> IO ()) -> (Maybe Void -> IO ())

-- TODO cwd, env
data Config = Config
  { name :: Text,
    arguments :: [Text],
    foreground :: Bool
  }

data Event a
  = Stdout a
  | Stderr a
  | Exit ExitCode
  deriving stock (Eq, Generic, Show)

withProcess :: Config -> Source ByteString -> (Source (Event ByteString) -> IO r) -> IO r
withProcess Config {arguments, foreground, name} stdin k =
  withPipe \stdinR stdinW ->
    withPipe \stdoutR stdoutW ->
      withPipe \stderrR stderrW -> do
        let createProcess :: Process.CreateProcess
            createProcess =
              Process.CreateProcess
                { Process.child_group = Nothing,
                  Process.child_user = Nothing,
                  Process.close_fds = True,
                  Process.cmdspec = Process.RawCommand (Text.unpack name) (map Text.unpack arguments),
                  Process.create_group = not foreground,
                  Process.create_new_console = False,
                  Process.cwd = Nothing,
                  Process.delegate_ctlc = foreground,
                  Process.detach_console = False,
                  Process.env = Nothing,
                  Process.new_session = False,
                  Process.std_err = Process.UseHandle (unH stderrW),
                  Process.std_in = Process.UseHandle (unH stdinR),
                  Process.std_out = Process.UseHandle (unH stdoutW),
                  Process.use_process_jobs = False
                }
        bracket
          ( do
              (_, _, _, process) <- Process.createProcess createProcess
              pure process
          )
          terminateProcess
          \process -> do
            (readEvent, writeEvent) <- newQueue
            Ki.scoped \scope -> do
              Ki.fork_ scope (handleProcess stdinW stdoutR stderrR process stdin (atomically . writeEvent))
              k \callback _ ->
                fix \again -> do
                  event <- atomically readEvent
                  callback (Just event)
                  case event of
                    Stdout _ -> again
                    Stderr _ -> again
                    Exit _ -> callback Nothing

terminateProcess :: Process.ProcessHandle -> IO ()
terminateProcess process = do
  Process.withProcessHandle process \case
    Process.ClosedHandle _ -> pure ()
    Process.OpenExtHandle {} -> undefined -- bug "OpenExtHandle is Windows-only" []
    Process.OpenHandle pid -> do
      pgid <- Posix.getProcessGroupIDOf pid
      Posix.signalProcessGroup Posix.sigTERM pgid
  void (Process.waitForProcess process)

handleProcess ::
  H "w" ->
  H "r" ->
  H "r" ->
  Process.ProcessHandle ->
  Source ByteString ->
  (Event ByteString -> IO ()) ->
  IO ()
handleProcess stdinW stdoutR stderrR process stdin handleEvent =
  Ki.scoped \scope -> do
    Ki.fork_ scope (handleInput stdinW)
    Ki.fork_ scope (handleOutput stdoutR (handleEvent . Stdout))
    Ki.fork_ scope (handleOutput stderrR (handleEvent . Stderr))
    exitCode <- Process.waitForProcess process
    Ki.wait scope -- intentional! wait for handle threads to flush
    handleEvent (Exit exitCode)
  where
    handleInput :: H "w" -> IO ()
    handleInput handle = do
      alive <- newIORef True
      drain stdin \bytes ->
        readIORef alive >>= \case
          False -> pure ()
          True ->
            try (hWrite handle bytes) >>= \case
              Left (_ :: IOException) -> writeIORef alive False
              Right () -> pure ()
      hClose handle
    handleOutput :: H "r" -> (ByteString -> IO ()) -> IO ()
    handleOutput handle action =
      fix \again -> do
        chunk <- hRead handle
        if ByteString.null chunk
          then hClose handle
          else do
            action chunk
            again

-- | Drain a 'Source' by executing a callback on each item.
drain :: Source a -> (a -> IO ()) -> IO ()
drain source callback =
  source (maybe (pure ()) callback) Nothing

newQueue :: IO (STM a, a -> STM ())
newQueue = do
  queue <- newTQueueIO
  pure (readTQueue queue, writeTQueue queue)

------------------------------------------------------------------------------------------------------------------------
-- Handle utils

newtype H (s :: Symbol) = H
  {unH :: Handle}

hClose :: H s -> IO ()
hClose =
  coerce IO.hClose

hRead :: H "r" -> IO ByteString
hRead (H handle) =
  ByteString.hGetSome handle 4096

hWrite :: H "w" -> ByteString -> IO ()
hWrite =
  coerce ByteString.hPut

withPipe :: (H "r" -> H "w" -> IO r) -> IO r
withPipe action =
  bracket
    (coerce Process.createPipe)
    (\(read, write) -> hClose read `finally` hClose write)
    (uncurry action)
