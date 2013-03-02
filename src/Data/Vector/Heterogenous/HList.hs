{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Vector.Heterogenous.HList
    where

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

data ShowBox = forall a. (Show a) => ShowBox a

instance Show ShowBox where
    show (ShowBox a) = show a

instance (Show a) => ConstraintBox ShowBox a where
    box a = ShowBox a
    unsafeUnbox (ShowBox a) = unsafeCoerce a

