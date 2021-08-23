module Subpross
  ( withProcess,
    Spec (..),
    Process (..),
    Stream,
  )
where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception (IOException, bracket, finally, throwIO, try)
import Control.Foldl (FoldM (..))
import qualified Control.Foldl as Foldl
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as Text
import H
import qualified Ki
import System.Exit
import qualified System.Posix.Process as Posix (getProcessGroupIDOf)
import qualified System.Posix.Signals as Posix
import qualified System.Posix.Types as Posix (CPid)
import qualified System.Process as Process
import qualified System.Process.Internals as Process
import Prelude hiding (lines)

data Process = Process
  { stdin :: Stream ByteString -> IO (),
    stdout :: Stream ByteString,
    stderr :: Stream ByteString,
    exitCode :: IO ExitCode
  }

-- TODO cwd, env
data Spec = Spec
  { name :: Text,
    arguments :: [Text]
  }

type Stream a =
  forall r. FoldM IO a r -> IO r

withProcess :: Spec -> (Process -> IO r) -> IO r
withProcess Spec {arguments, name} k = do
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
                  Process.create_group = True,
                  Process.create_new_console = False,
                  Process.cwd = Nothing,
                  Process.delegate_ctlc = False,
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
          \processHandle -> do
            hClose stdinR
            hClose stdoutW
            hClose stderrW
            Ki.scoped \scope -> do
              exitCodeThread <- Ki.async scope (Process.waitForProcess processHandle)
              let -- Is the process still running?
                  running :: STM Bool
                  running =
                    False <$ Ki.awaitSTM exitCodeThread <|> pure True
              k
                Process
                  { stdin =
                      \stream -> do
                        -- Minor optimization: don't bother spawning a thread to write stdin if the process has already
                        -- finished.
                        atomically running >>= \case
                          False -> pure ()
                          True -> do
                            Ki.scoped \scope2 -> do
                              -- Consume the stream in a background thread by forwarding all bytes to the stin handle of
                              -- the subprocess. Ignore exceptions, because they seem uninteresting and race-conditiony
                              -- since the subprocess can end at any time, before we've written the entire stdin stream
                              -- (which may be infinite!).
                              _ <-
                                Ki.async scope2 do
                                  stream (Foldl.mapM_ (hWrite stdinW))
                                  hClose stdinW
                              -- Once the process has exited, we can (forcefully) stop trying to write to its stdin.
                              atomically do
                                running >>= \case
                                  False -> pure ()
                                  True -> retry,
                    stdout = handleStream stdoutR,
                    stderr = handleStream stderrR,
                    exitCode = Ki.await exitCodeThread >>= either throwIO pure
                  }
  where
    fourth :: (a, b, c, d) -> d
    fourth (_, _, _, x) =
      x

handleStream :: H "r" -> Stream ByteString
handleStream handle =
  Foldl.impurely \step initial final -> do
    acc0 <- initial
    let loop acc = do
          chunk <- hRead handle
          if ByteString.null chunk
            then do
              hClose handle
              pure acc
            else step acc chunk >>= loop
    result <- loop acc0
    final result

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
