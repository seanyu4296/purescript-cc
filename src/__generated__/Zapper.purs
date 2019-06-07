module ZRPC.Clients.Zapper
       ( AccountStatus(..)
       , ReleaseTag(..)
       , TagLinkInfo(..)
       , Tag(..)
       , Email(..)
       , LocationEntry(..)
       , Gender(..)
       , User(..)
       , EmailType(..)
       , GetZapperRequest(..)
       , GetZapperError(..)
       , ResendEmailVerificationRequest(..)
       , ResendEmailVerificationError(..)
       , GetZapperSuccess(..)
       , GetZapperByMobileRequest(..)
       , GetZapperByTagRequest(..)
       , GetZapperByEmailRequest(..)
       , GetTagInfoFromCodeRequest(..)
       , GetTagInfoFromCodeResponse(..)
       , SetEmailRequest(..)
       , SetEmailError(..)
       , getZapper
       , getZapperByMobile
       , getZapperByTag
       , getZapperByEmail
       , getTagInfoFromCode
       , resendEmailVerification
       , setEmail
       ) where

import ZRPC

data AccountStatus =
  AccountStatusActive

  | AccountStatusSuspended

  | AccountStatusInactive

derive instance eqAccountStatus :: Eq AccountStatus
derive instance genericAccountStatus :: Generic AccountStatus _

instance consPrefixAccountStatus :: ConsPrefix AccountStatus where
  consPrefix _ = "AccountStatus"
instance showAccountStatus :: Show AccountStatus where
  show x = genericShow x
instance zrpcReadAccountStatus :: ZRPCRead AccountStatus where
  zrpcRead x = genericZRPCRead x
instance zrpcWriteAccountStatus :: ZRPCWrite AccountStatus where
  zrpcWrite x = genericZRPCWrite x

type ReleaseTag =
  { id :: String
  , code :: Maybe String
  , uuid :: Maybe String
  , barcode :: Maybe String
  , magstripe :: Maybe String
  , holdingId :: Maybe Int
  , imageUrl :: Maybe String
  , linkedAt :: DateTime
  , tagOwner :: String
  , isVerified :: Boolean
  , isSuspended :: Boolean
  , dateActivated :: Maybe DateTime
  , expiry :: Maybe DateTime
  }

type TagLinkInfo =
  { ownerId :: String
  , isVerified :: Boolean
  , linkedAt :: DateTime
  }

type Tag =
  { id :: String
  , code :: Maybe String
  , uuid :: Maybe String
  , barcode :: Maybe String
  , magstripe :: Maybe String
  , holdingId :: Maybe Int
  , imageUrl :: Maybe String
  , linkInfo :: Maybe TagLinkInfo
  }

data Email =
  EmailConfirmed
    { value :: Maybe String
    }
  | EmailUnconfirmed
    { value :: Maybe String
    }
derive instance eqEmail :: Eq Email
derive instance genericEmail :: Generic Email _

instance consPrefixEmail :: ConsPrefix Email where
  consPrefix _ = "Email"
instance showEmail :: Show Email where
  show x = genericShow x
instance zrpcReadEmail :: ZRPCRead Email where
  zrpcRead x = genericZRPCRead x
instance zrpcWriteEmail :: ZRPCWrite Email where
  zrpcWrite x = genericZRPCWrite x

type LocationEntry =
  { id :: String
  , country :: String
  , region :: Maybe String
  , city :: Maybe String
  }

data Gender =
  GenderMale

  | GenderFemale

derive instance eqGender :: Eq Gender
derive instance genericGender :: Generic Gender _

instance consPrefixGender :: ConsPrefix Gender where
  consPrefix _ = "Gender"
instance showGender :: Show Gender where
  show x = genericShow x
instance zrpcReadGender :: ZRPCRead Gender where
  zrpcRead x = genericZRPCRead x
instance zrpcWriteGender :: ZRPCWrite Gender where
  zrpcWrite x = genericZRPCWrite x

type User =
  { zapperId :: String
  , firstName :: Maybe String
  , lastName :: Maybe String
  , gender :: Maybe Gender
  , streetAddress :: Maybe String
  , zipCode :: Maybe String
  , birthday :: Maybe Date
  , location :: Maybe LocationEntry
  , mobile :: String
  , email :: Maybe Email
  , userName :: Maybe String
  , userKey :: Maybe String
  , dateRegistered :: DateTime
  , avatar :: String
  , branchId :: Maybe String
  , tags :: Array ReleaseTag
  , accountStatus :: AccountStatus
  , refTag :: Maybe String
  , fingerprint :: Maybe Long
  , ipAddress :: Maybe String
  , referredBy :: Maybe String
  , refCode :: Maybe String
  }

data EmailType =
  EmailTypeUnverified
    { email :: String
    , sendConfirmation :: Boolean
    }
  | EmailTypeVerified
    { email :: String
    }
derive instance eqEmailType :: Eq EmailType
derive instance genericEmailType :: Generic EmailType _

instance consPrefixEmailType :: ConsPrefix EmailType where
  consPrefix _ = "EmailType"
instance showEmailType :: Show EmailType where
  show x = genericShow x
instance zrpcReadEmailType :: ZRPCRead EmailType where
  zrpcRead x = genericZRPCRead x
instance zrpcWriteEmailType :: ZRPCWrite EmailType where
  zrpcWrite x = genericZRPCWrite x

type GetZapperRequest =
  { zapperId :: String
  }

data GetZapperError =
  GetZapperErrorProfileNotFound


derive instance eqGetZapperError :: Eq GetZapperError
derive instance genericGetZapperError :: Generic GetZapperError _

instance consPrefixGetZapperError :: ConsPrefix GetZapperError where
  consPrefix _ = "GetZapperError"
instance showGetZapperError :: Show GetZapperError where
  show x = genericShow x
instance zrpcReadGetZapperError :: ZRPCRead GetZapperError where
  zrpcRead x = genericZRPCRead x
instance zrpcWriteGetZapperError :: ZRPCWrite GetZapperError where
  zrpcWrite x = genericZRPCWrite x

type ResendEmailVerificationRequest =
  { zapperId :: String
  }

data ResendEmailVerificationError =
  ResendEmailVerificationErrorExpiredVerification

  | ResendEmailVerificationErrorExceededResendCount

  | ResendEmailVerificationErrorEmailVerificationNotFound

derive instance eqResendEmailVerificationError :: Eq ResendEmailVerificationError
derive instance genericResendEmailVerificationError :: Generic ResendEmailVerificationError _

instance consPrefixResendEmailVerificationError :: ConsPrefix ResendEmailVerificationError where
  consPrefix _ = "ResendEmailVerificationError"
instance showResendEmailVerificationError :: Show ResendEmailVerificationError where
  show x = genericShow x
instance zrpcReadResendEmailVerificationError :: ZRPCRead ResendEmailVerificationError where
  zrpcRead x = genericZRPCRead x
instance zrpcWriteResendEmailVerificationError :: ZRPCWrite ResendEmailVerificationError where
  zrpcWrite x = genericZRPCWrite x

type GetZapperSuccess =
  { user :: Maybe User
  , inactiveUsers :: Maybe (Array User)
  }

type GetZapperByMobileRequest =
  { mobile :: String
  , siloId :: String
  , includeInactive :: Maybe Boolean
  }

type GetZapperByTagRequest =
  { tagId :: String
  , siloId :: String
  , includeInactive :: Maybe Boolean
  }

type GetZapperByEmailRequest =
  { email :: String
  , siloId :: String
  , includeInactive :: Maybe Boolean
  }

type GetTagInfoFromCodeRequest =
  { code :: String
  }

type GetTagInfoFromCodeResponse =
  { tagOpt :: Maybe Tag
  }

type SetEmailRequest =
  { zapperId :: String
  , email :: EmailType
  }

data SetEmailError =
  SetEmailErrorInvalidEmail

  | SetEmailErrorExistingEmail

  | SetEmailErrorUnableToSetEmail

derive instance eqSetEmailError :: Eq SetEmailError
derive instance genericSetEmailError :: Generic SetEmailError _

instance consPrefixSetEmailError :: ConsPrefix SetEmailError where
  consPrefix _ = "SetEmailError"
instance showSetEmailError :: Show SetEmailError where
  show x = genericShow x
instance zrpcReadSetEmailError :: ZRPCRead SetEmailError where
  zrpcRead x = genericZRPCRead x
instance zrpcWriteSetEmailError :: ZRPCWrite SetEmailError where
  zrpcWrite x = genericZRPCWrite x

getZapper :: RPCClient -> GetZapperRequest -> Aff (Either GetZapperError User)
getZapper = rpcReq "getZapper"

getZapperByMobile :: RPCClient -> GetZapperByMobileRequest -> Aff GetZapperSuccess
getZapperByMobile = rpcReq "getZapperByMobile"

getZapperByTag :: RPCClient -> GetZapperByTagRequest -> Aff GetZapperSuccess
getZapperByTag = rpcReq "getZapperByTag"

getZapperByEmail :: RPCClient -> GetZapperByEmailRequest -> Aff GetZapperSuccess
getZapperByEmail = rpcReq "getZapperByEmail"

getTagInfoFromCode :: RPCClient -> GetTagInfoFromCodeRequest -> Aff GetTagInfoFromCodeResponse
getTagInfoFromCode = rpcReq "getTagInfoFromCode"

resendEmailVerification :: RPCClient -> ResendEmailVerificationRequest -> Aff (Either ResendEmailVerificationError Unit)
resendEmailVerification = rpcReq "resendEmailVerification"

setEmail :: RPCClient -> SetEmailRequest -> Aff (Either SetEmailError Unit)
setEmail = rpcReq "setEmail"

rpcReq ::
  forall req res
  . ZRPCWrite req
  => ZRPCRead res
  => RPCAppReqBuilder req res
rpcReq =
  mkRPCAppReqBuilder (AppName "zapper")
