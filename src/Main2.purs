module Main2 where

import Prelude

import CC.IO2 (Err(..))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, catchError, launchAff_, runAff_)
import Unsafe.Coerce (unsafeCoerce)


fakeSendToHttp :: forall a. a -> Effect Unit
fakeSendToHttp = unsafeCoerce

-- equivalent of unsafeRun but important that
-- Aff is a MonadThrow
-- unsafeRunAff :: forall a. Aff a -> Aff (Either Error a)
-- unsafeRunAff aff = launchAff_ ((aff <#> Right) `catchError` (\e -> pure $ Left e))

handleFailure :: forall a. Aff a -> Aff (Either Err a)
handleFailure aff = (aff <#> Right) `catchError` (\e -> pure $ Left InternalErr)





-- alternative
runner :: forall a. Aff a -> Effect Unit
runner aff = runAff_ (case _ of
    Right r -> fakeSendToHttp r
    Left err -> fakeSendToHttp err
  ) aff
