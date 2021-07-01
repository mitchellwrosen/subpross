{-# LANGUAGE UnboxedTuples #-}

module Subpross
  ( withProcess,
    Config (..),
    Event (..),
    Source (..),
    -- debugging
    onLines,
    linewise,
    listSource,
  )
where

import Control.Concurrent.STM.TQueue
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (when)
import Control.Monad.STM (STM, atomically)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Coerce (coerce)
import Data.Function
import Data.Functor (void)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Internal.Private as Text (span_)
import qualified Data.Text.Unsafe as Text (unsafeTail)
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
  deriving stock (Eq, Functor, Generic, Show)

newtype Source a = Source
  {unSource :: (Maybe a -> IO ()) -> IO ()}
  deriving stock (Functor)

instance Monoid (Source a) where
  mappend = (<>)
  mempty = Source mempty

instance Semigroup (Source a) where
  Source xs <> Source ys =
    Source \k -> do
      xs k
      ys k

listSource :: [a] -> Source a
listSource xs0 =
  Source \k ->
    (`fix` xs0) \again -> \case
      [] -> k Nothing
      x : xs -> do
        k (Just x)
        again xs

linewise :: Source (Event ByteString) -> Source (Event Text)
linewise source =
  Source \k -> do
    outRef <- newIORef (S Text.empty Text.streamDecodeUtf8)
    errRef <- newIORef (S Text.empty Text.streamDecodeUtf8)
    drain source \case
      Stdout chunk -> alterIORef outRef \s -> feed (k . Just . Stdout) s chunk
      Stderr chunk -> alterIORef errRef \s -> feed (k . Just . Stderr) s chunk
      Exit code -> do
        S out _ <- readIORef outRef
        when (not (Text.null out)) (k (Just (Stdout out)))
        S err _ <- readIORef errRef
        when (not (Text.null err)) (k (Just (Stderr out)))
        k (Just (Exit code))

feed :: (Text -> IO ()) -> S -> ByteString -> IO S
feed k (S text0 decode0) chunk =
  case decode0 chunk of
    Text.Some text1 _ decode1 -> do
      text2 <- onLines k (text0 <> text1)
      pure (S text2 decode1)

data S
  = S !Text !(ByteString -> Text.Decoding)

-- | Perform an action on each line in the input, and return the (possibly empty) end of the input that did not end in a
-- newline.
--
--      > onLines putStrLn "foo\nbar\n"
--      foo  -- side-effect
--      bar  -- side-effect
--      ""   -- return value
--
--     > onLines putStrLn  "foo\nbar"
--     foo   -- side-effect
--     "bar" -- return-value
onLines :: (Text -> IO ()) -> Text -> IO Text
onLines action =
  fix \again xs ->
    case Text.null xs of
      False ->
        case Text.span_ (/= '\n') xs of
          (# ys, zs #) ->
            case Text.null zs of
              False -> do
                action ys
                again (Text.unsafeTail zs)
              True -> pure ys
      True -> pure xs

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
              k do
                Source \callback ->
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

-- | Drain a 'Source' with an action to run on each item.
drain :: Source a -> (a -> IO ()) -> IO ()
drain source action =
  unSource source (maybe (pure ()) action)

newQueue :: IO (STM a, a -> STM ())
newQueue = do
  queue <- newTQueueIO
  pure (readTQueue queue, writeTQueue queue)

alterIORef :: IORef a -> (a -> IO a) -> IO ()
alterIORef ref action = do
  x0 <- readIORef ref
  x1 <- action x0
  writeIORef ref x1

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
