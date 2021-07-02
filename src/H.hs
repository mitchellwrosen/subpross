-- | Handle utils
module H where

import Control.Exception (bracket, finally)
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
