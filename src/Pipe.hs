{-# LANGUAGE UnboxedTuples #-}

-- | Z-IO style pipes.
module Pipe
  ( Pipe,
    run,
    debug,
    traverse_,
    stdoutDecodeUtf8,
    stderrDecodeUtf8,
    stdoutLines,
    stderrLines,
  )
where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Function
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Internal.Private as Text (span_)
import qualified Data.Text.Unsafe as Text (unsafeTail)
import Data.Void (Void)
import Event
import Prism (Prism (..))
import Prelude hiding (lines)

type Pipe a b =
  (Maybe b -> IO ()) -> (Maybe a -> IO ())

run :: Pipe Void Void -> IO ()
run pipe =
  pipe (\_ -> pure ()) Nothing

debug :: forall a b. (Show a, Show b) => (Text -> IO ()) -> Text -> Pipe a b -> Pipe a b
debug put name inner emit input = do
  put ("[" <> name <> "] <== " <> Text.pack (show input))
  inner emit' input
  where
    emit' :: Maybe b -> IO ()
    emit' output = do
      put ("[" <> name <> "] ==> " <> Text.pack (show output))
      emit output

prism :: forall s t a b. Prism s t a b -> Pipe a b -> Pipe s t
prism p inner emit = \case
  Nothing -> emit' Nothing
  Just s ->
    case matching p s of
      Left t -> emit (Just t)
      Right a -> emit' (Just a)
  where
    emit' :: Maybe a -> IO ()
    emit' =
      inner (emit . fmap (review p))

traverse_ :: (a -> IO ()) -> Pipe a Void
traverse_ f emit = \case
  Nothing -> emit Nothing
  Just x -> f x

stdoutDecodeUtf8 :: IO (Pipe (Event ByteString a) (Event Text a))
stdoutDecodeUtf8 =
  prism asStdout <$> decodeUtf8

stderrDecodeUtf8 :: IO (Pipe (Event a ByteString) (Event a Text))
stderrDecodeUtf8 =
  prism asStderr <$> decodeUtf8

stdoutLines :: IO (Pipe (Event ByteString a) (Event Text a))
stdoutLines =
  (\f g -> prism asStdout (f . g)) <$> decodeUtf8 <*> lines

stderrLines :: IO (Pipe (Event a ByteString) (Event a Text))
stderrLines = do
  (\f g -> prism asStderr (f . g)) <$> decodeUtf8 <*> lines

decodeUtf8 :: IO (Pipe ByteString Text)
decodeUtf8 = do
  decodeRef <- newIORef Text.streamDecodeUtf8
  pure \emit -> \case
    Nothing -> emit Nothing
    Just chunk ->
      alterIORef decodeRef \decode -> do
        let Text.Some text _ decode' = decode chunk
        emit (Just text)
        pure decode'

lines :: IO (Pipe Text Text)
lines = do
  endRef <- newIORef Text.empty
  pure \emit -> \case
    Nothing -> do
      end <- readIORef endRef
      when (not (Text.null end)) (emit (Just end))
      emit Nothing
    Just chunk -> alterIORef endRef \end -> onLines (emit . Just) (end <> chunk)

------------------------------------------------------------------------------------------------------------------------
-- Misc. utils

alterIORef :: IORef a -> (a -> IO a) -> IO ()
alterIORef ref action = do
  x0 <- readIORef ref
  x1 <- action x0
  writeIORef ref x1

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
