{-# LANGUAGE UnboxedTuples #-}

module Subpross
  ( withProcess,
    Spec (..),
    Event (..),
  )
where

import Control.Concurrent.STM.TQueue
import Control.Exception (IOException, bracket, try)
import Control.Monad.STM (atomically)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Function
import Data.Functor (void)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Event
import H
import qualified Ki
import Pipe (Pipe)
import qualified Pipe
import qualified System.Posix.Process as Posix (getProcessGroupIDOf)
import qualified System.Posix.Signals as Posix
import qualified System.Posix.Types as Posix (CPid)
import qualified System.Process as Process
import qualified System.Process.Internals as Process
import Prelude hiding (lines)

-- TODO cwd, env
data Spec = Spec
  { name :: Text,
    arguments :: [Text],
    foreground :: Bool
  }

withProcess ::
  Spec ->
  Pipe Void ByteString ->
  (Pipe Void (Event ByteString ByteString) -> IO r) ->
  IO r
withProcess Spec {arguments, foreground, name} stdin k =
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
          (fourth <$> Process.createProcess createProcess)
          terminateProcess
          (handleProcess stdin k stdinW stdoutR stderrR)
  where
    fourth :: (a, b, c, d) -> d
    fourth (_, _, _, x) =
      x

handleProcess ::
  Pipe Void ByteString ->
  (Pipe Void (Event ByteString ByteString) -> IO r) ->
  H "w" ->
  H "r" ->
  H "r" ->
  Process.ProcessHandle ->
  IO r
handleProcess stdin k stdinW stdoutR stderrR process = do
  eventQueue <- newTQueueIO
  let write = atomically . writeTQueue eventQueue
  Ki.scoped \scope -> do
    Ki.fork_ scope (handleInput stdinW)
    Ki.fork_ scope (handleOutput stdoutR (write . Stdout))
    Ki.fork_ scope (handleOutput stderrR (write . Stderr))
    Ki.fork_ scope (Process.waitForProcess process >>= write . Exit)
    k \callback _ ->
      fix \again -> do
        event <- atomically (readTQueue eventQueue)
        callback (Just event)
        case event of
          Stdout _ -> again
          Stderr _ -> again
          Exit _ -> callback Nothing
  where
    handleInput :: H "w" -> IO ()
    handleInput handle = do
      alive <- newIORef True
      let write :: ByteString -> IO ()
          write bytes =
            readIORef alive >>= \case
              False -> pure ()
              True ->
                try (hWrite handle bytes) >>= \case
                  Left (_ :: IOException) -> writeIORef alive False
                  Right () -> pure ()
      Pipe.run (stdin . Pipe.traverse_ write)
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

terminateProcess :: Process.ProcessHandle -> IO ()
terminateProcess process =
  getProcessId process >>= \case
    Nothing -> pure ()
    Just pid ->
      try (Posix.getProcessGroupIDOf pid) >>= \case
        Left (_ :: IOException) -> pure ()
        Right pgid ->
          try (Posix.signalProcessGroup Posix.sigTERM pgid) >>= \case
            Left (_ :: IOException) -> pure ()
            Right () -> void (Process.waitForProcess process)

getProcessId :: Process.ProcessHandle -> IO (Maybe Posix.CPid)
getProcessId process =
  Process.modifyProcessHandle process \handle ->
    pure
      ( handle,
        case handle of
          Process.ClosedHandle {} -> Nothing
          Process.OpenExtHandle {} -> Nothing
          Process.OpenHandle pid -> Just pid
      )
