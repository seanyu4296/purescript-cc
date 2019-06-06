module CC.Funcs where


import Prelude


factorial :: Int -> Int
factorial n = go n 1
  where
    go :: Int -> Int -> Int
    go x acc | x <= 0 = acc
             | otherwise = go (x-1) (x*acc)

factorial' :: Int -> Int
factorial' n =
  let
    go :: Int -> Int -> Int
    go x acc | x <= 0 = acc
             | otherwise = go (x-1) (x*acc)
  in
    go n 1

-- Guards
-- Function Body
-- where
-- let in