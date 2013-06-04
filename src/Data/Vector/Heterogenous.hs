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
{-# LANGUAGE EmptyDataDecls #-}

-- | Heterogenous vectors.  For more info on heterogenous collections, see <http://www.haskell.org/haskellwiki/Heterogenous_collections>
module Data.Vector.Heterogenous
    ( HVector(..)
    , vec
    , View (..)
    
    -- * Construction helpers
    , ValidHVector(..)
    , toHList
--     , HListBuilder(..)
--     , Indexer (..)
    
    -- * Modules
    , module Data.Vector.Heterogenous.HList
    , module Data.Vector.Heterogenous.Unsafe
    )
    where

import Data.Monoid
-- import Data.Semigroup
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic as G
import GHC.ST
import GHC.TypeLits

import Data.Vector.Heterogenous.HList
import Data.Vector.Heterogenous.Unsafe


-------------------------------------------------------------------------------
-- Vector

newtype HVector box (xs::[a]) = HVector { getvec :: V.Vector box }

instance (Show box) => Show (HVector box xs) where
    show (HVector vec) = "vec "++boxname++" $ "++(go $ n-1)++"HNil"
        where
            boxname = "ShowBox"
            n = V.length vec
            go i = if i >= 0
                then show (vec V.! i)++":::"++go (i-1)
                else ""

-- | creates an "HVector" from an "HList".  For example:

vec :: (HLength (HList xs), Downcast (HList xs) box) => (a->box) -> HList xs -> HVector box (xs::[*])
vec box xs = HVector $ V.create $ do
    v <- VM.new n
    vecwrite v (n-1) (downcastAs box xs)
    return $ v
        where
            n = hlength xs
            vecwrite v i []     = return ()
            vecwrite v i (x:xs) = (seq x $ VM.write v i x) >> vecwrite v (i-1) xs

-------------------------------------------------------------------------------


class 
    ( Downcast (HList xs) box
    , HLength (HList xs)
    , HListBuilder (Indexer (HVector box xs))  (HList xs)
    ) => ValidHVector box xs

instance 
    ( Downcast (HList xs) box
    , HLength (HList xs)
    , HListBuilder (Indexer (HVector box xs))  (HList xs)
    ) => ValidHVector box xs

---------------------------------------

data Indexer xs = Indexer !xs !Int

toHList :: HListBuilder (Indexer (HVector box xs)) ys => HVector box xs -> ys
toHList hv = buildHList $ Indexer hv (V.length (getvec hv) -1)

class HListBuilder xs ys | xs -> ys where
    buildHList :: xs -> ys
    
instance HListBuilder (Indexer (HVector box '[])) (HList '[]) where
    buildHList _ = HNil

instance
    ( ConstraintBox box x
    , HListBuilder (Indexer (HVector box xs)) (HList xs)
    ) => HListBuilder (Indexer (HVector box (x ': xs))) (HList (x ': xs)) 
        where
    buildHList (Indexer (HVector v) i) = 
        (unsafeUnbox $ v V.! i):::(buildHList (Indexer (HVector v) (i-1) :: Indexer (HVector box xs))) 
    
instance 
    ( Monoid (HList xs)
    , Downcast (HList xs) box
    , HLength (HList xs)
    , HListBuilder (Indexer (HVector box xs)) (HList xs)
    ) => Monoid (HVector box xs) where
    mempty = vec (undefined::a->box) $ mempty
    v1 `mappend` v2 = vec (undefined::a->box) $ (toHList v1) `mappend` (toHList v2)

-------------------------------------------------------------------------------
-- Lens

data Empty a

class View vec i ret | vec i -> ret where
    view :: vec -> i -> ret
        
instance (ConstraintBox box x) => View (Indexer (HVector box (x ': xs))) (Empty Zero) x where
    view (Indexer (HVector v) i) _ = unsafeUnbox $ v V.! i
    
instance (ConstraintBox box ret, View (Indexer (HVector box xs)) (Empty n) ret) => View (Indexer (HVector box (x ': xs))) (Empty (Succ n)) ret where
    view (Indexer (HVector v) i) _ = unsafeUnbox $ v V.! i
    
instance 
    ( View (Indexer (HVector box xs)) (Empty (ToNat1 n)) ret
    , SingI n
    ) => View (HVector box xs) (Sing n) ret where
    view hv _ = Indexer hv (V.length (getvec hv) -n-1) `view` (undefined::Empty (ToNat1 n))
        where
            n = fromIntegral $ fromSing (sing :: Sing n)