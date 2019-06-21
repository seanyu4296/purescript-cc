module Main where

import Prelude

import CC.IO2 (ServerM, getTwoZapper', x, Err)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_, runAff_)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import ZRPC (class ZRPCWrite, zrpcWrite)

returnsInt :: Effect Int
returnsInt = pure 1


processor :: forall a. ZRPCWrite a => ServerM a -> Effect Unit
processor m = runAff_
  (\e -> case e of
    Left l -> pure unit
    Right r -> do
      traceM $ zrpcWrite r
      pure unit
  )
  (runExceptT m)


main :: Effect Unit
main = do
  (processor $ getTwoZapper'
    "xb9930652-6def-4b2d-84fe-cdfad539ae83"
    "634961d8-b5e0-4c99-a45e-ed5b88d5f570"
  )
-- import Data.Either (Either(..))
-- import Effect (Effect)
-- import Effect.Aff (Aff, catchError, launchAff_, runAff_)
-- import Effect.Class (liftEffect)
-- import Effect.Console (log)
-- import Unsafe.Coerce (unsafeCoerce)


-- launchAff  = Effect (Fiber a )
-- launchAff_ = Effect Unit
-- run


-- fakeSendToHttp :: forall a. a -> Effect Unit
-- fakeSendToHttp = unsafeCoerce

-- runner :: forall a. Aff a -> Effect Unit
-- runner aff = runAff_ (case _ of
--     Right r -> fakeSendToHttp r
--     Left err -> fakeSendToHttp err
--   ) aff


-- unsafeRunAff :: forall a. Aff a -> Effect Unit
-- unsafeRunAff aff = launchAff_ (
--   (aff <#> Right) `catchError` (\e -> pure $ Left e)
-- ) >>= liftEffect <<< (\e -> pure unit)



{-
log <<< show $ a
-}