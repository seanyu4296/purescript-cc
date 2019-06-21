module CC.TypeClass0 where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)

-- Basic

-- Create data types
-- Adhoc polymorphism
data Entity = Merchant | Branch | Holding | Silo

class Show_ a where
  show' :: a -> String

-- Show, Eq, Ord, Semigroup, Monoid
instance showEntity :: Show_ Entity where
  show' (Merchant) = "M"
  show' _ = "M"

-- Stupid examples
instance eqEntity :: Eq Entity where
  eq (Merchant) (Merchant) = true
  eq _ _ = false

instance semigroupEntity :: Semigroup Entity where
  append Merchant Merchant = Holding
  append _ _ = Branch

instance monoidEntity :: Monoid Entity where
  mempty = Branch


-- Show what it compiles to
entityS :: Entity -> String
entityS e = show' e

-- Show using of derive instance

derive instance genericEntity :: Generic Entity _
-- derive instance eqEntity :: Eq Entity


-- When is newtype useful
-- newtype - allow us to attach different behavior to a type without changing its representation at runtime
-- no runtime performance overhead.
newtype MobileNumber = MobileNumber String

derive instance newtypeMN :: Newtype MobileNumber _

instance showMobileNumber :: Show MobileNumber where
  show a = "63" <> unwrap a

derive instance eqMN :: Eq MobileNumber

class MkMobileNumber a where
  mkMobileNumber :: a -> MobileNumber

instance mkMobNumInt :: MkMobileNumber Int where
  mkMobileNumber i = wrap $ show i



type X = MobileNumber


-- type synonym
-- export module and use in main
