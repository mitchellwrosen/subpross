module Event
  ( Event (..),
    asStdout,
    asStderr,
  )
where

import GHC.Generics (Generic)
import Prism (Prism (..))
import System.Exit (ExitCode)

data Event a b
  = Stdout a
  | Stderr b
  | Exit ExitCode
  deriving stock (Eq, Functor, Generic, Show)

asStdout :: Prism (Event a x) (Event b x) a b
asStdout =
  Prism
    { matching = \case
        Stdout x -> Right x
        Stderr x -> Left (Stderr x)
        Exit x -> Left (Exit x),
      review = Stdout
    }

asStderr :: Prism (Event x a) (Event x b) a b
asStderr =
  Prism
    { matching = \case
        Stdout x -> Left (Stdout x)
        Stderr x -> Right x
        Exit x -> Left (Exit x),
      review = Stderr
    }
