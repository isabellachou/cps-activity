module Lib
    ( Calc(..)
    , calc
    ) where

data Calc a = Add a
            | Sub a
   deriving (Eq,Show)


calc :: Num a => [Calc a] -> a -> (a -> a) -> (a -> a) -> a
calc xx init ka ks = aux xx ka ks where
    aux [] ka' ks' = ks' (ka' init)
    aux (Add i : rest) ka' ks' = aux rest (ka' . (+ i)) ks'
    aux (Sub i : rest) ka' ks' = aux rest ka' (ks' . (subtract i))