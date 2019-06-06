module CC.TypeClass1 where

import Prelude


-- 
data Option a = Some a | None

instance showOption :: Show a => Show (Option a) where
  show None = "None"
  show (Some x) = "Some(" <> show x <> ")"

map :: forall a b. Option a -> (a -> b) -> Option b
map x f = case x of
  Some m -> Some (f m)
  None -> None

flatMap :: forall a b. Option a -> (a -> Option b) -> Option b
flatMap x f = case x of
  Some m -> f m
  None -> None


data Either e a = Left e | Right a

eitherMap :: forall e a b. Either e a -> (a -> b) -> Either e b
eitherMap x f = case x of
  Right a -> Right (f a)
  Left e -> Left e

eitherFlatMap :: forall e a b. Either e a -> (a -> Either e b) -> Either e b
eitherFlatMap x f = case x of
  Right a -> f a
  Left e -> Left e