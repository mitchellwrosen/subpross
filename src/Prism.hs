module Prism where

data Prism s t a b = Prism
  { matching :: s -> Either t a,
    review :: b -> t
  }
