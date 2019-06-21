module CC.TypeClass00 where


import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Effect.Class.Console (log)


-- sealed trait Entity

-- case object Merchant extends Entity
-- case object Branch extends Entity


data Entity = Merchant | Branch | Holding | Silo

angus :: Entity
angus = Merchant

class Show_ a where
  show' :: a -> String

instance showEntity :: Show_ Entity where
  show' entity = case entity of
    Merchant -> "M"
    Branch -> "B"
    Silo -> "S"
    Holding -> "H"

derive instance eqEntity :: Eq Entity

logShow' :: forall a. Show_ a => a -> Effect Unit
logShow' a = log (show' a)

isEqual :: forall a. Eq a => a -> a -> Boolean
isEqual x y = x == y


emptyArr :: Array Int
emptyArr = mempty


newtype MobileNumber = MobileNumber String

data MobileNumber' = MobileNumber' String Int Int
ericMN :: MobileNumber
ericMN = MobileNumber "9999"


derive instance newtypeMN :: Newtype MobileNumber _

instance showMobileNumber :: Show MobileNumber where
  show mn = "63" <> unwrap mn

derive instance eqMN :: Eq MobileNumber


class MkMobileNumber a where
  mkMobileNumber :: a -> MobileNumber


instance mkMobileNumberInt :: MkMobileNumber Int where
  mkMobileNumber i = wrap (show i)