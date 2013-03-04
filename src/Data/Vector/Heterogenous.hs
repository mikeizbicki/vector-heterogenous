{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Vector.Heterogenous
    ( HVector(..)
    , vec
    , module Data.Vector.Heterogenous.HList
    , module Data.Vector.Heterogenous.Unsafe
    )
    where

import Data.Semigroup
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic as G
import GHC.ST
import GHC.TypeLits
import Unsafe.Coerce
import Debug.Trace

import Data.Vector.Heterogenous.HList
import Data.Vector.Heterogenous.Unsafe

-------------------------------------------------------------------------------
-- Vector

newtype HVector box (xs::[a]) = HVector { getvec :: V.Vector box }

instance (Show box) => Show (HVector box xs) where
    show (HVector vec) = "(vec "++boxname++" $ "++(go $ n-1)++"HNil)"
        where
            boxname = "(ShowBox ())"
            n = V.length vec
            go i = if i >= 0
                then show (vec V.! i)++":::"++go (i-1)
                else ""

vec :: (HLength (HList xs), Downcast (HList xs) box) => box -> HList xs -> HVector box (xs::[*])
vec box xs = HVector $ V.create $ do
    v <- VM.new n
    vecwrite v (n-1) (downcastAs box xs)
    return $ v
        where
            n = hlength xs
            vecwrite v i []     = return ()
            vecwrite v i (x:xs) = (seq x $ VM.write v i x) >> vecwrite v (i-1) xs

-- instance Semigroup (HVector box '[]) where
--     v1 <> v2 = v1
    
newtype Indexer xs (i::Nat) = Indexer xs

toHList :: HListBuilder (Indexer (HVector box xs) 0) ys => HVector box xs -> ys
toHList hv = buildHList $ mkIndexer hv

mkIndexer :: HVector box xs -> Indexer (HVector box xs) 0
mkIndexer hv = Indexer hv

class HListBuilder xs ys | xs -> ys where
    buildHList :: xs -> ys
    
instance HListBuilder (Indexer (HVector box '[]) i) (HList '[]) where
    buildHList _ = HNil

instance
    ( ConstraintBox box x
    , HListBuilder (Indexer (HVector box xs) (1+i)) (HList xs)
    , SingI i
    ) => HListBuilder (Indexer (HVector box (x ': xs)) i) (HList (x ': xs)) 
        where
    buildHList (Indexer (HVector v)) = 
        (unsafeUnbox $ v V.! i):::(buildHList (Indexer $ HVector v :: Indexer (HVector box xs) (1+i))) 
        where
            i = fromIntegral $ fromSing (sing :: Sing i)

-- instance (SingI a, SingI b) => SingI (a+b)
--     sing = (fromSing (sing::a))+(fromSing (sing::b))

-- toHList :: HVector box xs -> HList xs
-- toHList 
    
instance Semigroup (HVector box xs) where
    v1 <> v2 = undefined
    
-- class View vec i ret | vec i -> ret where
--     view :: vec -> i -> ret
--     
-- instance View (HVector box (x ': xs)) (Sing 0) 
-- 
-- view :: (SingI n, ConstraintBox box (Index xs (ToNat1 n))) => HVector box xs -> Sing (n::Nat) -> xs :! n
-- view (HVector v) _ = unsafeUnbox $ v V.! n
--     where
--         n = fromIntegral $ (sing :: Sing n)