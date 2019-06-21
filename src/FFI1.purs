module CC.FFI1
  ( add
  , logWithPrefix
  ) where



import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)



foreign import _log ::
  String -> Effect Unit

foreign import _logWithPrefix ::
  EffectFn2 String String Unit

foreign import _add :: Fn2 Int Int Int


logWithPrefix :: String -> String -> Effect Unit
logWithPrefix = runEffectFn2 _logWithPrefix

add :: Int -> Int -> Int
add = runFn2 _add
