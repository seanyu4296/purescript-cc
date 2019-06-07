module Main2 where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, catchError, launchAff_, runAff_)
import Effect.Console (log)
import Unsafe.Coerce (unsafeCoerce)


fakeSendToHttp :: forall a. a -> Effect Unit
fakeSendToHttp = unsafeCoerce

-- equivalent of unsafeRun but important that
-- Aff is a MonadThrow
unsafeRunAff :: forall a. Aff a -> Effect Unit
unsafeRunAff aff = launchAff_ ((aff <#> Right) `catchError` (\e -> pure $ Left e))

-- alternative
runner :: forall a. Aff a -> Effect Unit
runner aff = runAff_ (case _ of
    Right r -> fakeSendToHttp r
    Left err -> fakeSendToHttp err
  ) aff
