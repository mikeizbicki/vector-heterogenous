{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Vector.Heterogenous.HList
    where

-- import Data.Semigroup
import Data.Monoid
import GHC.TypeLits
import Unsafe.Coerce

-------------------------------------------------------------------------------
-- HList

data HList :: [*] -> * where
  HNil :: HList '[]
  (:::) :: t -> HList ts -> HList (t ': ts)
  
infixr 5 :::

instance Show (HList '[]) where
    show _ = "HNil"
instance (Show x, Show (HList xs)) => Show (HList (x ': xs)) where
    show (x:::xs) = show x ++":::"++show xs
    
-- instance Semigroup (HList '[]) where
--     HNil <> HNil = HNil
-- instance (Semigroup x, Semigroup (HList xs)) => Semigroup (HList (x ': xs)) where
--     (x:::xs)<>(y:::ys) = (x<>y):::(xs<>ys)

instance Monoid (HList '[]) where
    mempty = HNil
    HNil `mappend` HNil = HNil
instance (Monoid x, Monoid (HList xs)) => Monoid (HList (x ': xs)) where
    mempty = mempty:::mempty
    (x:::xs) `mappend` (y:::ys) = (x `mappend` y):::(xs `mappend` ys)

class HLength xs where
    hlength :: xs -> Int
instance HLength (HList '[]) where
    hlength _ = 0
instance (HLength (HList xs)) => HLength (HList (x ': xs)) where
    hlength (x:::xs) = 1+hlength xs

-------------------------------------------------------------------------------
-- downcasting HList -> []

class ConstraintBox box a where
    box :: a -> box
    unsafeUnbox :: box -> a
    
class Downcast h box where
    downcast :: h -> [box]
    
    downcastAs :: box -> h -> [box]
    downcastAs box = downcast

instance Downcast (HList '[]) a where
    downcast HNil = []

instance (ConstraintBox box x, Downcast (HList xs) box) => Downcast (HList (x ': xs)) box where
    downcast (x:::xs) = (box x):(downcast xs)

-------------------------------------------------------------------------------
-- boxes

data ShowBox = forall a. (Show a) => ShowBox !a

instance Show ShowBox where
    show (ShowBox a) = show a

instance (Show a) => ConstraintBox ShowBox a where
    box a = ShowBox a
    unsafeUnbox (ShowBox a) = unsafeCoerce a


-------------------------------------------------------------------------------
-- type functions

type family Distribute (xs::[a->b]) (t::a) :: [b]
type instance Distribute '[] a = '[]
type instance Distribute (x ': xs) a = (x a) ': (Distribute xs a)

type family Replicate (x::a) (n::Nat) :: [a]
type instance Replicate x n = Replicate1 x (ToNat1 n)
type family Replicate1 (x::a) (n::Nat1) :: [a]
type instance Replicate1 x Zero = '[]
type instance Replicate1 x (Succ n) = x ': (Replicate1 x n)

type family Map (f :: a -> a) (xs::[a]) :: [a]
type instance Map f '[] = '[]
type instance Map f (x ': xs) = (f x) ': (Map f xs)

type family Length (xs::[a]) :: Nat
type instance Length '[] = 0
type instance Length (a ': xs) = 1 + (Length xs)

type family MoveR (xs::[a]) (ys::[a]) :: [a]
type instance MoveR '[] ys = ys
type instance MoveR (x ': xs) ys = MoveR xs (x ': ys)

type family Reverse (xs::[a]) :: [a]
type instance Reverse xs = MoveR xs '[]

type family (xs::[a]) ++ (ys::[a]) :: [a]
type instance xs ++ ys = MoveR (Reverse xs) ys

type family (:!) (xs::[a]) (i::Nat) :: a
type instance (:!) xs n = Index xs (ToNat1 n)

type family Index (xs::[a]) (i::Nat1) :: a
type instance Index (x ': xs) Zero = x
type instance Index (x ': xs) (Succ i) = Index xs i

---------------------------------------

data Nat1 = Zero | Succ Nat1

type family FromNat1 (n :: Nat1) :: Nat
type instance FromNat1 Zero     = 0
type instance FromNat1 (Succ n) = 1 + FromNat1 n

type family ToNat1 (n :: Nat) :: Nat1
type instance ToNat1 0 = Zero
type instance ToNat1 1 = Succ (ToNat1 0)
type instance ToNat1 2 = Succ (ToNat1 1)
type instance ToNat1 3 = Succ (ToNat1 2)
type instance ToNat1 4 = Succ (ToNat1 3)
type instance ToNat1 5 = Succ (ToNat1 4)
type instance ToNat1 6 = Succ (ToNat1 5)
type instance ToNat1 7 = Succ (ToNat1 6)
type instance ToNat1 8 = Succ (ToNat1 7)
type instance ToNat1 9 = Succ (ToNat1 8)
type instance ToNat1 10 = Succ (ToNat1 9)
type instance ToNat1 11 = Succ (ToNat1 10)
type instance ToNat1 12 = Succ (ToNat1 11)
type instance ToNat1 13 = Succ (ToNat1 12)
type instance ToNat1 14 = Succ (ToNat1 13)
type instance ToNat1 15 = Succ (ToNat1 14)
type instance ToNat1 16 = Succ (ToNat1 15)
type instance ToNat1 17 = Succ (ToNat1 16)
type instance ToNat1 18 = Succ (ToNat1 17)
type instance ToNat1 19 = Succ (ToNat1 18)
type instance ToNat1 20 = Succ (ToNat1 19)