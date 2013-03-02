{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE DatatypeContexts #-}

module Data.Vector.Heterogenous
    ( Tuple(..)
    , tup
    , module Data.Vector.Heterogenous.HList
    )
    where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic as G
import GHC.ST
import GHC.TypeLits
import Unsafe.Coerce
import Debug.Trace

import Data.Vector.Heterogenous.HList

-------------------------------------------------------------------------------
-- Tuple

newtype Tuple box xs = Tuple (V.Vector box)

instance (Show box) => Show (Tuple box xs) where
    show (Tuple vec) = "(tup $ "++go 0++")"
        where
            go i = if i < V.length vec
                then show (vec V.! i) ++ ":::" ++ go (i+1)
                else "HNil"

tup :: (HLength xs, Downcast xs box) => box -> xs -> Tuple box xs
tup box xs = Tuple $ V.create $ do
    v <- VM.new n
    tupwrite v (n-1) (downcastAs box xs)
    return $ v
        where
            n = hlength xs
            tupwrite v i []     = return ()
            tupwrite v i (x:xs) = (seq x $ VM.write v i x) >> tupwrite v (i-1) xs

-------------------------------------------------------------------------------
-- type functions

type family Map (f :: * -> *) (xs::[*]) :: [*]
type instance Map f '[] = '[]
type instance Map f (x ': xs) = (f x) ': (Map f xs)

type family Length (xs::[*]) :: Nat
type instance Length '[] = 0
type instance Length (a ': xs) = 1 + (Length xs)

type family MoveR (xs::[*]) (ys::[*]) :: [*]
type instance MoveR '[] ys = ys
type instance MoveR (x ': xs) ys = MoveR xs (x ': ys)

type family Reverse (xs::[*]) :: [*]
type instance Reverse xs = MoveR xs '[]

type family (xs::[*]) ++ (ys::[*]) :: [*]
type instance xs ++ ys = MoveR (Reverse xs) ys

type family (:!) (xs::[a]) (i::Nat1) :: a
type instance (:!) (x ': xs) Zero = x
type instance (:!) (x ': xs) (Succ i) = xs :! i

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