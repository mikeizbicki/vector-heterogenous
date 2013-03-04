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

module Data.Vector.Heterogenous.Unsafe
    ( UnsafeHVector(..)
    , unhvec
    )
    where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic as G
import GHC.ST
import GHC.TypeLits
import Unsafe.Coerce

import Data.Vector.Heterogenous.HList

-------------------------------------------------------------------------------
-- UnsafeVector

data UnsafeBox = UnsafeBox
    deriving (Read,Show)

newtype UnsafeHVector xs = UnsafeHVector (V.Vector UnsafeBox)

unhvec :: (UnsafeHVectorWriter a, HLength a) => a -> UnsafeHVector a
unhvec xs = UnsafeHVector $ V.create $ do
    v <- VM.new n
    vecwrite v (n-1) xs
    return v
    where
        n = hlength xs
        
class UnsafeHVectorWriter t where
    vecwrite :: VM.MVector s UnsafeBox -> Int -> t -> ST s ()

instance (UnsafeHVectorWriter (HList xs)) => UnsafeHVectorWriter (HList (x ': xs)) where
    vecwrite v i (x:::xs) = VM.write v i (unsafeCoerce x) >> vecwrite v (i-1) xs

instance UnsafeHVectorWriter (HList '[]) where
    vecwrite v i b = return ()

data ShowIndex a = ShowIndex Int a

instance (Show (ShowIndex (UnsafeHVector a))) => Show (UnsafeHVector a) where
    show a = "(vec $ "++(show $ ShowIndex (len-1) a)++")"
        where
            len = let (UnsafeHVector vec) = a in V.length vec

instance 
    ( Show x
    , Show (ShowIndex (UnsafeHVector (HList xs)))
    ) => Show (ShowIndex (UnsafeHVector (HList (x ': xs)))) where
    show (ShowIndex i (UnsafeHVector vec)) = 
        show (unsafeCoerce (vec V.! i) :: x)++":::"++
        show (ShowIndex (i-1) (UnsafeHVector vec :: UnsafeHVector (HList xs)))
    
instance Show (ShowIndex (UnsafeHVector (HList '[]))) where
    show (ShowIndex i (UnsafeHVector vec)) = "HNil"
