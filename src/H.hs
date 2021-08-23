-- | Handle utils
module H where

import Control.Exception (IOException, catch, onException, uninterruptibleMask)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Coerce (coerce)
import GHC.TypeLits (Symbol)
import System.IO (Handle)
import qualified System.IO as IO
import qualified System.Process as Process
import Prelude hiding (read)

newtype H (s :: Symbol) = H
  {unH :: Handle}

hClose :: H s -> IO ()
hClose (H handle) =
  IO.hClose handle `catch` \(_ :: IOException) -> pure ()

hSetBuffering :: H s -> IO.BufferMode -> IO ()
hSetBuffering =
  coerce IO.hSetBuffering

hRead :: H "r" -> IO ByteString
hRead (H handle) =
  ByteString.hGetSome handle 4096

hWrite :: H "w" -> ByteString -> IO ()
hWrite =
  coerce ByteString.hPut

withPipe :: (H "r" -> H "w" -> IO r) -> IO r
withPipe action = do
  uninterruptibleMask \restore -> do
    (read, write) <- coerce Process.createPipe
    let cleanup = do
          hClose read
          hClose write
    result <- restore (action read write) `onException` cleanup
    cleanup
    pure result
