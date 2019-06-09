module Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, catchError, launchAff_, runAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Unsafe.Coerce (unsafeCoerce)


-- launchAff  = Effect (Fiber a )
-- launchAff_ = Effect Unit
-- run


fakeSendToHttp :: forall a. a -> Effect Unit
fakeSendToHttp = unsafeCoerce

runner :: forall a. Aff a -> Effect Unit
runner aff = runAff_ (case _ of
    Right r -> fakeSendToHttp r
    Left err -> fakeSendToHttp err
  ) aff


unsafeRunAff :: forall a. Aff a -> Effect Unit
unsafeRunAff aff = launchAff_ (
  (aff <#> Right) `catchError` (\e -> pure $ Left e)
) >>= liftEffect <<< (\e -> pure unit)

main :: Effect Unit
main = do
  log "Hello sailor!"

{-
log <<< show $ a
-}