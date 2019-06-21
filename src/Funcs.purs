module CC.Funcs where


import Prelude

import Data.Array as Array
import Effect.Class.Console (logShow)


factorial :: Int -> Int
factorial n = go n 1
  where
    go :: Int -> Int -> Int
    go x acc | x <= 0 && acc == 0 = acc
             | otherwise = go (x-1) (x*acc)

    eric = {}

factorial' :: Int -> Int
factorial' n =
  let
    go :: Int -> Int -> Int
    go x acc | x <= 0 = acc
             | otherwise = go (x-1) (x*acc)
  in
    go n 1


x = [1,2,3]

y = [4,5,6]

z = logShow $ x <> y

c = 5 * 3

apply' :: forall a b. (a -> b) -> a -> b
apply' f a = f a

infixr 5 apply' as <<>

a = Array.snoc x 1



-- Guards
-- Function Body
-- where
-- let in