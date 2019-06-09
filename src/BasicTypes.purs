module CC.BasicTypes where

import Prelude


-- Records
type Student =
  { name :: String
  , age :: Int
  }

type Student' =
  Record
    ( name :: String
    , age :: Int
    )

type Person r =
  { name :: String
  , weight :: Int
  | r
  }
sean :: Student
sean = { name: "s", age: 1}

-- Row polymorphism
showName :: forall r. Person r -> String
showName r = r.name

eric :: Person (age :: Int)
eric = { name: "s", weight: 1, age: 1}

oldEric :: Person (age :: Int)
oldEric = eric { age = 99 }

i :: Int
i = 1

n :: Number
n = 1.0

bool :: Boolean
bool = true

s :: String
s = "awdawd"

c :: Char
c = 'c'

arr :: Array Int
arr = [1,2,3,4]


-- Sample of curried
add :: Int -> Int -> Int
add x y = x + y

addOne :: Array Int -> Array Int
addOne = map (add 1)