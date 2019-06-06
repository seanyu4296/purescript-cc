module CC.FFI where

import Prelude

import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, mkEffectFn2, runEffectFn2)

-- foreign import normal func

foreign import _logWithPrefix :: EffectFn2 String String Unit
foreign import now :: Effect Int
foreign import _add :: Fn2 Int Int Int
foreign import random :: Effect Number
-- foreign import data

logWithPrefix :: String -> String -> Effect Unit
logWithPrefix pref s = runEffectFn2 _logWithPrefix pref s


add :: Int -> Int -> Int
add x y = runFn2 _add x y

addUncurried :: Fn2 Int Int Int
addUncurried = mkFn2 add

logWithPrefixU :: EffectFn2 String String Unit
logWithPrefixU = mkEffectFn2 logWithPrefix
-- foreign import uncurried stuff

-- foreign import alert or log

-- foreign import Math random


-- Fn2 purescript-functions
