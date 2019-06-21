module CC.IO2 where


import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT(..), throwError)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Debug.Trace (trace, traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import Milkis (URL(..))
import Milkis.Impl.Node (nodeFetch)
import ZRPC (class ConsPrefix, class Generic, class ZRPCRead, class ZRPCWrite, RPCClient, genericShow, genericZRPCRead, genericZRPCWrite, zrpcWrite)
import ZRPC.Clients.Zapper as Zapper


data ZapperTwo = ZapperTwo
  { userOne :: Zapper.User
  , userTwo :: Zapper.User
  }

{-
{ type: "ZapperTwo"
,
}
-}
derive instance eqZapperTwo :: Eq ZapperTwo
derive instance genericZapperTwo :: Generic ZapperTwo _

instance consPrefixZapperTwo :: ConsPrefix ZapperTwo where
  consPrefix _ = ""
instance showZapperTwo :: Show ZapperTwo where
  show = genericShow
instance zrpcReadZapperTwo :: ZRPCRead ZapperTwo where
  zrpcRead x = genericZRPCRead x
instance zrpcWriteZapperTwo :: ZRPCWrite ZapperTwo where
  zrpcWrite x = genericZRPCWrite x

rpcC :: RPCClient
rpcC =
  { rpcURL : URL "http://rpc.staging.z4p.me/rpc"
  , fetchImpl: nodeFetch
  }

x :: Json
x = zrpcWrite Zapper.AccountStatusActive


type ZapperId = String
data Err = InternalErr | Unauthorized

derive instance eqErr :: Eq Err
derive instance genericErr :: Generic Err _

instance consPrefixErr :: ConsPrefix Err where
  consPrefix _ = ""
instance showErr :: Show Err where
  show = genericShow
instance zrpcReadErr :: ZRPCRead Err where
  zrpcRead x = genericZRPCRead x
instance zrpcWriteErr :: ZRPCWrite Err where
  zrpcWrite x = genericZRPCWrite x

-- bind = flatMap
-- pure :: a -> f a
-- pure a = Right a
getTwoZappers ::
  ZapperId -> ZapperId -> Aff (Either Err ZapperTwo)
getTwoZappers zid zid2 = do
  ze <- Zapper.getZapper rpcC { zapperId: zid}
  case ze of
    Left l -> pure $ Left InternalErr
    Right r -> do
      ze2 <- Zapper.getZapper rpcC { zapperId: zid}
      case ze2 of
        Left l -> pure $ Left InternalErr
        Right r2 -> pure $ Right $ ZapperTwo { userOne: r, userTwo: r2}

type ServerM a = ExceptT Err Aff a -- type synonym

-- type ServerImp a = forall m. Monad m => MonadAff m => MonadThrow Err m -> m a
-- EitherT[F[_], L, R]

type ServerImp a = forall m. MonadAff m => MonadThrow Err m => m a



getZapper' :: ZapperId -> ServerImp Zapper.User
getZapper' id =
  liftAff (Zapper.getZapper rpcC { zapperId: id }) -- ExceptT Err Aff (Either Zapper.GetZapperError Zapper.User))
    >>= (case _ of
      Right r -> pure r
      Left _ -> throwError InternalErr
    )

-- class MonadAff a where
--   liftAff :: Aff a -> m a

getTwoZapper' ::
  ZapperId -> ZapperId -> ServerM ZapperTwo
getTwoZapper' id id2 = do
  z <- getZapper' id
  z2 <- getZapper' id2
  pure $ ZapperTwo { userOne: z, userTwo: z2 }