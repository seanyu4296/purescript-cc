module ZRPC.Clients.Bank
       ( Redeemable(..)
       , PointsContent(..)
       , BankOwner(..)
       , GetAccountsRequest(..)
       , Account(..)
       , CurrencyType(..)
       , ConversionRate(..)
       , Currency(..)
       , GetCurrenciesRequest(..)
       , GetCurrenciesResponse(..)
       , BankError(..)
       , getAccounts
       , getCurrencies
       ) where

import ZRPC

data Redeemable =
  RedeemableConcrete
    { value :: HugeNum
    }
  | RedeemableUnrestricted

derive instance eqRedeemable :: Eq Redeemable
derive instance genericRedeemable :: Generic Redeemable _

instance consPrefixRedeemable :: ConsPrefix Redeemable where
  consPrefix _ = "Redeemable"
instance showRedeemable :: Show Redeemable where
  show x = genericShow x
instance zrpcReadRedeemable :: ZRPCRead Redeemable where
  zrpcRead x = genericZRPCRead x
instance zrpcWriteRedeemable :: ZRPCWrite Redeemable where
  zrpcWrite x = genericZRPCWrite x

type PointsContent =
  { currencyId :: Int
  , pendingEarn :: Maybe HugeNum
  , pendingRedeem :: Maybe HugeNum
  , cleared :: Maybe HugeNum
  , redeemable :: Maybe Redeemable
  }

data BankOwner =
  BankOwnerZapper
    { id :: String
    }
  | BankOwnerTag
    { id :: String
    }
  | BankOwnerMerchant
    { id :: String
    }
derive instance eqBankOwner :: Eq BankOwner
derive instance genericBankOwner :: Generic BankOwner _

instance consPrefixBankOwner :: ConsPrefix BankOwner where
  consPrefix _ = "BankOwner"
instance showBankOwner :: Show BankOwner where
  show x = genericShow x
instance zrpcReadBankOwner :: ZRPCRead BankOwner where
  zrpcRead x = genericZRPCRead x
instance zrpcWriteBankOwner :: ZRPCWrite BankOwner where
  zrpcWrite x = genericZRPCWrite x

type GetAccountsRequest =
  { owner :: BankOwner
  , includeInactive :: Boolean
  }

type Account =
  { pointsContent :: PointsContent
  }

data CurrencyType =
  CurrencyTypeRegular

  | CurrencyTypeAirAsia

  | CurrencyTypePayPal

derive instance eqCurrencyType :: Eq CurrencyType
derive instance genericCurrencyType :: Generic CurrencyType _

instance consPrefixCurrencyType :: ConsPrefix CurrencyType where
  consPrefix _ = "CurrencyType"
instance showCurrencyType :: Show CurrencyType where
  show x = genericShow x
instance zrpcReadCurrencyType :: ZRPCRead CurrencyType where
  zrpcRead x = genericZRPCRead x
instance zrpcWriteCurrencyType :: ZRPCWrite CurrencyType where
  zrpcWrite x = genericZRPCWrite x

type ConversionRate =
  { to :: Int
  , rate :: HugeNum
  }

type Currency =
  { id :: Int
  , name :: String
  , defaultPriority :: Int
  , currencyType :: CurrencyType
  , conversionRates :: Array ConversionRate
  , symbol :: Maybe String
  }

type GetCurrenciesRequest =
  { currencyIds :: Array Int
  }

type GetCurrenciesResponse =
  { currencies :: Array Currency
  }

data BankError =
  BankErrorUnexpectedError

  | BankErrorInsufficientBalance

  | BankErrorInvalidAmount

  | BankErrorInvalidVoid

  | BankErrorInvalidClear

  | BankErrorInvalidAction

  | BankErrorInvalidAllocation

  | BankErrorTransactionNotFound

  | BankErrorInvalidTxStatus

  | BankErrorInvalidCurrency

  | BankErrorInvalidCoupon

  | BankErrorAccountLinkCurrencyMismatch

  | BankErrorBankAccountNotFound

  | BankErrorCouponNotFound

  | BankErrorAccountAlreadyLinked

  | BankErrorBSIDAlreadyUsed

  | BankErrorNegativeBalanceNotAllowed

  | BankErrorInvalidUser

  | BankErrorUpdateDbError

derive instance eqBankError :: Eq BankError
derive instance genericBankError :: Generic BankError _

instance consPrefixBankError :: ConsPrefix BankError where
  consPrefix _ = "BankError"
instance showBankError :: Show BankError where
  show x = genericShow x
instance zrpcReadBankError :: ZRPCRead BankError where
  zrpcRead x = genericZRPCRead x
instance zrpcWriteBankError :: ZRPCWrite BankError where
  zrpcWrite x = genericZRPCWrite x

getAccounts :: RPCClient -> GetAccountsRequest -> Aff (Either BankError (Array Account))
getAccounts = rpcReq "getAccounts"

getCurrencies :: RPCClient -> GetCurrenciesRequest -> Aff GetCurrenciesResponse
getCurrencies = rpcReq "getCurrencies"

rpcReq ::
  forall req res
  . ZRPCWrite req
  => ZRPCRead res
  => RPCAppReqBuilder req res
rpcReq =
  mkRPCAppReqBuilder (AppName "bank")
