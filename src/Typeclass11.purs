module CC.Typeclass11 where


import Prelude

import Control.Apply (lift2)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Maybe (Maybe)

data Option a = Some a | None

-- a => b => a + b

add' :: String -> Number -> Maybe Int
add' x y = lift2 (\a b -> a + b) (Int.fromString x) (Int.fromNumber y)


 -- (Int -> Int -> Entity)
data Entity = Merchant Int Int

formM :: String -> Number -> Maybe Entity
formM x y = Merchant <$> (Int.fromString x) <*> (Int.fromNumber y)

formM' :: String -> Number -> Maybe Entity
formM' x y = do
  i <- Int.fromString x
  i' <- Int.fromNumber y
  pure $ Merchant i i'
-- (Int -> Int -> Entity) <$> Maybe Int <*> Maybe Int
-- (a -> b) -> f a -> f b
-- (Int -> (Int -> Entity))
-- (Maybe (Int -> Entity)) <*> Maybe Int'
-- f (a -> b) -> f a -> f b

-- derive instance genericOption :: Generic (Option a) _

-- instance showOption :: Show a => Show (Option a ) where
--   show = genericShow

instance showOption :: Show a => Show (Option a) where
  show None = "None"
  show (Some x) = "Some(" <> show x <> ")"

instance functorOption :: Functor (Option) where
  map :: forall a b. (a -> b) -> Option a -> Option b
  map f x = case x of
    Some m -> Some (f m)
    None -> None


-- def showSOmething(x: Int)(implicit showString: Show[A])

-- showSomething("awawd")(showASd)


-- Type
-- Type -> Type
-- Type -> Type -> Type
-- derive instance functorOption :: Functor Option

instance applyOption :: Apply (Option) where
  apply :: forall a b. Option (a -> b) -> Option a -> Option b
  apply (Some fab) (Some fa) = Some $ fab fa
  apply _ _ = None

instance applicativeOption :: Applicative (Option) where
  pure :: forall a. a -> Option a
  pure a = Some a

instance bindOption :: Bind (Option) where
  bind :: forall a b. Option a -> (a -> Option b) -> Option b
  bind (Some a) f = f a
  bind None _ = None

instance monadOption :: Monad Option


-- data Entity = Merchant Int Int | Branch Int Int

-- Exercise form a merchant
-- Exercise do Either functor, apply, applicative, bind
-- Show case usefulness of type constraints

-- class Monad m <= ToInt m where
--   fromN :: Number -> m Int
--   fromS :: String -> m Int


-- instance toIntMaybe :: ToInt (Maybe) where
--   fromN n = fromNumber n
--   fromS n = fromString n

-- instance toIntEither :: ToInt (Either l) where
--   fromN n = pure 1
--   fromS n = pure 1



-- getRandom :: forall m. Monad m => m Int
-- getRandom = pure 1

-- getRandom' :: forall m. Monad m => m Int
-- getRandom' = pure 2

-- buildMerchant :: forall m. (Monad m, ToInt m) => m Entity
-- buildMerchant = do
--   x <- getRandom
--   y <- getRandom'
--   pure $ Merchant x y
