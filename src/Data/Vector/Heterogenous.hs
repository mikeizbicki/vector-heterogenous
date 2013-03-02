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
    ( ExistentialVector(..)
    , vec
    , Tuple(..)
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
-- ExistentialVector

newtype ExistentialVector box xs = ExistentialVector { getvec :: V.Vector box }

instance (Show box) => Show (ExistentialVector box xs) where
    show (ExistentialVector vec) = "(vec "++boxname++" $ "++(go $ n-1)++"HNil)"
        where
            boxname = "(ShowBox ())"
            n = V.length vec
            go i = if i >= 0
                then show (vec V.! i)++":::"++go (i-1)
                else ""

vec :: (HLength xs, Downcast xs box) => box -> xs -> ExistentialVector box xs
vec box xs = ExistentialVector $ V.create $ do
    v <- VM.new n
    tupwrite v (n-1) (downcastAs box xs)
    return $ v
        where
            n = hlength xs
            tupwrite v i []     = return ()
            tupwrite v i (x:xs) = (seq x $ VM.write v i x) >> tupwrite v (i-1) xs

-------------------------------------------------------------------------------
-- UnsafeVector

data UnsafeBox = UnsafeBox
    deriving (Read,Show)

newtype Tuple xs = Tuple (V.Vector UnsafeBox)

tup :: (TupleWriter a, HLength a) => a -> Tuple a
tup xs = Tuple $ V.create $ do
    v <- VM.new n
    tupwrite v (n-1) xs
--     tupwrite v (n-1) xs
    return v
    where
        n = hlength xs
        
class TupleWriter t where
    tupwrite :: VM.MVector s UnsafeBox -> Int -> t -> ST s ()

instance (TupleWriter (HList xs)) => TupleWriter (HList (x ': xs)) where
    tupwrite v i (x:::xs) = VM.write v i (unsafeCoerce x) >> tupwrite v (i-1) xs

instance TupleWriter (HList '[]) where
    tupwrite v i b = return ()

data ShowIndex a = ShowIndex Int a

instance (Show (ShowIndex (Tuple a))) => Show (Tuple a) where
    show a = "(tup $ "++(show $ ShowIndex (len-1) a)++")"
        where
            len = let (Tuple vec) = a in V.length vec

instance 
    ( Show x
    , Show (ShowIndex (Tuple (HList xs)))
    ) => Show (ShowIndex (Tuple (HList (x ': xs)))) where
    show (ShowIndex i (Tuple vec)) = 
        show (unsafeCoerce (vec V.! i) :: x)++":::"++
        show (ShowIndex (i-1) (Tuple vec :: Tuple (HList xs)))
    
instance Show (ShowIndex (Tuple (HList '[]))) where
    show (ShowIndex i (Tuple vec)) = "HNil"
        