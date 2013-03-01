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
{-# LANGUAGE OverlappingInstances #-}

module Data.NTuple'
    where

import Control.Lens
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import GHC.ST
import GHC.TypeLits
import Unsafe.Coerce
import Debug.Trace

data Color = Red | Blue | Green | Purple
    deriving (Read,Show,Eq,Ord)

data ShowBox = forall a. (Show a) => ShowBox a

instance Show ShowBox where
    show (ShowBox a) = show a

-------------------------------------------------------------------------------
-- data types

data UnsafeBox = UnsafeBox
    deriving (Read,Show)

data a:::b = b:::a
    deriving (Read, Show, Eq, Ord)

newtype Tuple xs = Tuple (V.Vector UnsafeBox)

tup :: (TupWriter a, TupLen a) => a -> Tuple a
tup xs = Tuple $ V.create $ do
    v <- VM.new n
    tupwrite v (n-1) xs
    return v
    where
        n = tuplen xs

-------------------------------------------------------------------------------
-- type classes

class TupLen t where
    tuplen :: t -> Int

instance (TupLen b) => TupLen (a ::: b) where
    tuplen (b ::: a) = 1 + tuplen b

instance TupLen b where
    tuplen b = 1

class TupWriter t where
    tupwrite :: VM.MVector s UnsafeBox -> Int -> t -> ST s ()

instance (TupWriter b) => TupWriter (a:::b) where
    tupwrite v i (b:::a) = VM.write v i (unsafeCoerce a) >> tupwrite v (i-1) b

instance TupWriter b where
    tupwrite v i b = VM.write v i (unsafeCoerce b)

-------------------------------------------------------------------------------
-- lens

_i :: (SingI n) => Tuple (Replicate n a) -> Int -> a
_i (Tuple vec) i = unsafeCoerce $ vec V.! i

data GetIndex xs (n::Nat) a = GetIndex xs

-- getIndex :: xs -> Sing n -> a

{-class TupIndex i n a | i n -> a where
    _i :: (SingI n) => i -> (Sing n) -> a

instance TupIndex (a:::b) Zero b where
    _i (b:::a) sing = b

instance (TupIndex b i c) => TupIndex (a:::b) (Succ i) c where-}
--     _i (b:::a) _ = _i b undefined -- (sing:: Sing i)

-- instance TupIndex (a:::b) 0 b where
--     _i (b:::a) sing = b
-- 
-- instance (TupIndex b (i+1) c, SingI i, SingI (i+1)) => TupIndex (a:::b) i c where
-- --     _i (b:::a) (sing :: Sing i) = _i b (sing:: Sing (i+1))
--     _i (b:::a) _ = _i b (sing:: Sing (i+1))

class Viewable tup where
    view :: (SingI n) => (Sing n) -> (tup a) -> (tup b)



-- class TypeNatIndex {-n-} s t a b | s -> a, t -> b, s b -> t, t a -> s where
--     _i :: {-n -> -}IndexedLens Int s t a b
-- 
-- instance TypeNatIndex {-(Sing 0)-} (a,b) (a',b) a a' where
-- --     _i {-sing-} f tup = undefined -- Tuple (vec V.// [])
-- --     _i {-sing-} f (Tuple vec) = undefined -- Tuple (vec V.// [])

first :: Simple Lens (a,b) a
first f (a,b) = undefined
-------------------------------------------------------------------------------
-- showing

data ShowIndex a = ShowIndex Int a

-- instance Show (ShowIndex (Tuple a) where
--     show a = ""
    
-- instance (Show (ShowIndex (Tuple' xs))) => Show (NTuple' xs) where
--     show tup = "(tup " ++ (show $ ShowIndex ((len tup)-1) tup) ++ ")"
--         where
--             n = V.length (getvec tup)
-- --             n = fromIntegral $ fromSing (sing :: Sing n)    

instance (Show (ShowIndex (Tuple a))) => Show (Tuple a) where
    show a = "(tup $ "++(show $ ShowIndex (len-1) a)++")"
        where
            len = let (Tuple vec) = a in V.length vec

instance 
    ( Show a
    , Show (ShowIndex (Tuple b))
    ) => Show (ShowIndex (Tuple (a:::b))) where
    show (ShowIndex i (Tuple vec)) = 
        show (ShowIndex (i-1) (Tuple vec :: Tuple b))++":::"++show (unsafeCoerce (vec V.! i) :: a)
    
instance 
    ( Show b
    ) => Show (ShowIndex (Tuple b)) where
    show (ShowIndex i (Tuple vec)) = show (unsafeCoerce (vec V.! i) :: b)
        

-------------------------------------------------------------------------------
-- modification

-- class EmptyTup tup where
--     emptytup :: tup
-- 
-- instance EmptyTup (NTuple' xs) where
--     emptytup = NTuple'
--         { len = 0
--         , getvec = V.replicate n UnsafeBox
--         }
--         where
--             n = 2
-- 
-- class PushBack tup a tup' | tup a -> tup' where
--     pushback :: tup -> a -> tup'
--     
-- instance PushBack (NTuple' xs) a (NTuple' (a ': xs)) where
--     pushback tup a = if n<V.length (getvec tup)
--         then NTuple'
--             { len = (len tup)+1
--             , getvec = runST $ do
--                 v <- V.unsafeThaw $ (getvec tup)
--                 VM.write v (len tup) (unsafeCoerce a)
--                 V.unsafeFreeze v
--             }
--         else NTuple'
--             { len = (len tup)+1
--             , getvec = V.generate ((len tup)*2) $ \i -> if i<(len tup)
--                 then (getvec tup) V.! i
--                 else unsafeCoerce a
--             }
--         where
--             n = len tup
-- 
-- class TupIndexable index where
--     index :: NTuple' xs -> index -> xs :! (ToNat1 (ExtractIndex index))
--     
-- instance (SingI i) => TupIndexable (Index i) where
--     index tup Index = unsafeCoerce (getvec tup V.! i)
--         where
--             i = fromIntegral $ fromSing (sing :: Sing i)

-------------------------------------------------------------------------------
-- type functions

-- data Index (n::Nat) = Index
-- type family ExtractIndex i :: Nat
-- type instance ExtractIndex (Index i) = i

-- class BoxTuple box t t' where
--     boxtuple :: box -> t -> t'

-- instance BoxTuple box (Tuple xs) (V.Vector box) where


test :: (Num a) => Replicate n a -> Int
test = undefined

type family Map (f :: * -> *) (xs::[*]) :: [*]
type instance Map f '[] = '[]
type instance Map f (x ': xs) = (f x) ': (Map f xs)
-- type instance Box (x:::xs) a = a x:::(Box xs a)

type family Replicate (n::Nat) a
type instance Replicate n a = Replicate' (ToNat1 n) a
type family Replicate' (n::Nat1) a
type instance Replicate' (Succ Zero) a = a
type instance Replicate' (Succ (Succ xs)) a = a:::(Replicate' (Succ xs) a)

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