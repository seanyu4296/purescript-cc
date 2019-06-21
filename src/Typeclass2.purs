module CC.TypeClass2 where


import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromNumber, fromString)
import Data.Maybe (Maybe)

data Option' a = Some' a | None'

derive instance genericOption :: Generic (Option' a) _

instance showOption :: Show a => Show (Option' a) where
  show = genericShow

instance functorOption :: Functor (Option') where
  map :: forall a b. (a -> b) -> Option' a -> Option' b
  map f x = case x of
    Some' m -> Some' (f m)
    None' -> None'


instance applyOption :: Apply (Option') where
  apply :: forall a b. Option' (a -> b) -> Option' a -> Option' b
  apply (Some' fab) (Some' fa) = Some' $ fab fa
  apply _ _ = None'

instance applicativeOption :: Applicative (Option') where
  pure :: forall a. a -> Option' a
  pure a = Some' a

instance bindOption :: Bind (Option') where
  bind :: forall a b. Option' a -> (a -> Option' b) -> Option' b
  bind (Some' a) f = f a
  bind None' _ = None'

instance monadOption :: Monad (Option')



adder :: Int -> Option' Int
adder x = pure $ x + 1

multiplier :: Int -> Option' Int
multiplier x = pure $ x * 2


x :: Option' Int
x = Some' 1
  >>= adder
  >>= multiplier

zz  = do
  z <- Some' 1
  a <- adder z
  multiplier a
-- Show do notation (show usage of pure)

-- om_ :: Number -> String -> Maybe Entity
-- om_ n s = do
--   x <- fromNumber n
--   y <- fromString s
--   pure $ Merchant x y



-- Type -> Type -> Type
data Either' l r = Left' l | Right' r

-- Install purescript-maybe purescript-integers

-- Show pure using own data type
data Entity = Merchant Int Int | Branch Int Int

om :: Number -> String -> Maybe Entity
om n s = Merchant <$> fromNumber n <*> fromString s


-- map = liftA1
-- apply = ap
