module CC.IO where

-- GOAL to Create getUserById

-- Sequential
import Prelude

import CC.FFI (now)
import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), throwError)
import Control.Parallel (parTraverse)
import Data.DateTime (Date, DateTime(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Milkis (URL(..))
import Milkis.Impl.Node (nodeFetch)
import ZRPC (Aff, HugeNum, Maybe, RPCClient)
import ZRPC.Clients.Bank as Bank
import ZRPC.Clients.Zapper (getZapper)
import ZRPC.Clients.Zapper as Zapper

-- GOAL to Create getUserById

-- Sequential





data ZapperTwo = ZapperTwo Zapper.User Zapper.User
data Err = GenErr


rpcC :: RPCClient
rpcC = { rpcURL: URL ("http://rpc.staging.z4p.me/rpc"), fetchImpl: nodeFetch }


type ZapperId = String



getTwoZappers :: ZapperId -> ZapperId -> Aff (Either Err ZapperTwo)
getTwoZappers id id2 = do
  z <- getZapper rpcC { zapperId: id }
  z2 <- getZapper rpcC { zapperId : id2}
  pure $ case z, z2 of
    Right zr, Right zr2 -> Right $ ZapperTwo zr zr2
    _ , _ -> Left $ GenErr


-- So how do you use this
-- runAff_ -- Go to Main2.purs


-- We see a repetition of this case case ExceptT
type ZapM a = ExceptT Err Aff a -- type synonym
-- Convert to ExceptT
-- MonadAff (liftAff)
getZapper' :: String -> ZapM Zapper.User
getZapper' id =
  liftAff (getZapper rpcC { zapperId: id })
    >>= case _ of
      Right r -> pure r
      Left _ -> throwError GenErr


-- how to have an effect in the middle like date now
getTwoZappers' :: ZapperId -> ZapperId -> ZapM ZapperTwo
getTwoZappers' id id2 = do
  z <- getZapper' id
  z2 <- getZapper' id2
  d <- liftEffect now
  pure $ ZapperTwo z z2


-- (<|>)
-- What if its okay for something to fail
type Holding = {
  id :: Int,
  name :: String
}

type Tag =
  { id :: String
  , code :: Maybe String
  , uuid :: Maybe String
  , barcode :: Maybe String
  , magstripe :: Maybe String
  , holding :: Maybe Holding
  , label :: Maybe String
  , linkedAt :: DateTime
  }

type CurrencyBalance = {
  id :: Int,
  name :: Maybe String,
  redeemable:: Maybe HugeNum,
  pendingEarn:: Maybe HugeNum
}


type User =
  { zapperId :: String
  , firstName :: Maybe String
  , accounts :: Maybe (Array Bank.Account)
  }


-- Add getCurrencyBalances - show (<|>)
getAccounts :: ZapperId -> Aff (Maybe (Array Bank.Account))
getAccounts id = getA <|> pure Nothing
  where
    getA = (Bank.getAccounts rpcC { owner : Bank.BankOwnerZapper { id }, includeInactive : false })
      <#> case _ of
        Right accounts -> Just $ accounts
        Left _ -> Nothing

getZapperAndCB :: ZapperId -> ZapM User
getZapperAndCB id = do
  z <- getZapper' id
  accounts <- liftAff $ getAccounts id
  pure $ { zapperId: z.zapperId, firstName: z.firstName, accounts }


-- Parallel (parTraverse, sequential + parallel)
getZappers :: Array ZapperId -> ZapM (Array Zapper.User)
getZappers ids = parTraverse (\id -> getZapper' id) ids

-- Racing

-- Use catchError at top level
-- Catching (catchError (MonadError)) pagrun ng exceptT chaka catchError
-- MonadThrow (throwError) -> MonadError (catchError)


-- MonadEffect (liftEffect)

-- Show example logShow