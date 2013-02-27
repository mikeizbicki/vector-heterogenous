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

-------------------------------------------------------------------------------
-- data types

data UnsafeBox = UnsafeBox
    deriving (Read,Show)

data NTuple' (xs::[*]) = NTuple' 
    { len :: Int
    , getvec :: V.Vector UnsafeBox
    }

data a:::b = b:::a
    deriving (Read, Show, Eq, Ord)

class Len t where
    tlen :: t -> Int
    
instance (Len b) => Len (a ::: b) where
    tlen (b ::: a) = 1 + tlen b

instance Len b where
    tlen b = 1

class TupleWriter t where
    tupwrite :: VM.MVector s UnsafeBox -> Int -> t -> ST s ()

instance (TupleWriter b) => TupleWriter (a:::b) where
    tupwrite v i (b:::a) = VM.write v i (unsafeCoerce a) >> tupwrite v (i-1) b

instance TupleWriter b where
    tupwrite v i b = VM.write v i (unsafeCoerce b)

newtype Tuple xs = Tuple (V.Vector UnsafeBox)

tup :: (TupleWriter a, Len a) => a -> Tuple a
tup xs = Tuple $ V.create $ do
    v <- VM.new n
    tupwrite v (n-1) xs
--     go v xs (n-1)
    return v
    where
        n = tlen xs
        
--     where
--         go v (x:[]) 0 = VM.write v 0 x
--         go v (x:xs) i = VM.write v i x >> go v xs (i-1)

-- tup :: a:::b -> 

newtype HList (xs ::[*]) = HList [UnsafeBox]
    deriving (Read,Show)

hnil :: HList '[]
hnil = HList []

hcons :: a -> HList xs -> HList (a ': xs)
hcons a (HList xs) = HList ((unsafeCoerce a):xs)

hlist2tuple :: HList xs -> NTuple' xs
hlist2tuple (HList xs) = NTuple'
    { len = n
    , getvec = vec
    }
    where 
        n = length xs
        vec = V.create $ do
            v <- VM.new n
            go v xs (n-1)
            return v
            
        go v (x:[]) 0 = VM.write v 0 x
        go v (x:xs) i = VM.write v i x >> go v xs (i-1)
        

class MkList t1 t2 | t2 -> t1 where
    mklist :: t1 -> t2
 
-- instance MkList (HList xs) (HList xs) where
--     mklist args = args

instance MkList (HList '[]) (HList '[]) where
    mklist args = HList []

instance (MkList (HList (a ': xs)) r) => MkList (HList xs) (a -> r) where
    mklist list = \a -> mklist (a `hcons` list)

-- test = hlist2tuple $ mklist (HList []) "poop" (89::Int) 'h'

-- terminate :: HList xs -> HList

-- class MkList t where
--     mklist :: [String] -> t
--  
-- instance (Show a) => MkList (IO a) where
--     mklist args = do
--         print args
--         return undefined
--  
-- instance (MkList r, Show a) => MkList (a -> r) where
--     mklist xs = \a -> mklist ((show a) : xs)
-- 
-- tup = mklist []

-- class PrintAllType t where
--     process :: [String] -> t
--  
-- instance PrintAllType [String] where
--     process args = args
--  
-- instance (Show a, PrintAllType r) => PrintAllType (a -> r) where
--     process args = \a -> process (args ++ [show a])
--  
-- printAll :: (PrintAllType t) => t
-- printAll = process ([] :: [String])

-- mkl = mklist []

-- class MkList t where
--     mklist :: HList xs -> t
--  
-- instance MkList (HList '[]) where
--     mklist args = hnil
--  
-- instance (MkList r) => MkList (a -> r) where
--     mklist count = \a -> mklist (a `hcons` count)
-- 
-- mkl = mklist hnil

-------------------------------------------------------------------------------
-- showing

data ShowBox a = ShowBox Int a

-- instance Show (ShowBox (Tuple a) where
--     show a = ""
    
-- instance (Show (ShowBox (Tuple' xs))) => Show (NTuple' xs) where
--     show tup = "(tup " ++ (show $ ShowBox ((len tup)-1) tup) ++ ")"
--         where
--             n = V.length (getvec tup)
-- --             n = fromIntegral $ fromSing (sing :: Sing n)    

instance (Show (ShowBox (Tuple a))) => Show (Tuple a) where
    show a = "(tup $ "++(show $ ShowBox (len-1) a)++")"
        where
            len = let (Tuple vec) = a in V.length vec

instance 
    ( Show a
    , Show (ShowBox (Tuple b))
    ) => Show (ShowBox (Tuple (a:::b))) where
    show (ShowBox i (Tuple vec)) = 
        show (ShowBox (i-1) (Tuple vec :: Tuple b))++":::"++show (unsafeCoerce (vec V.! i) :: a)
    
instance 
    ( Show b
    ) => Show (ShowBox (Tuple b)) where
    show (ShowBox i (Tuple vec)) = show (unsafeCoerce (vec V.! i) :: b)
        

instance Show (ShowBox (NTuple' '[])) where
    show _ = ""
    
instance (Show (ShowBox (NTuple' xs))) => Show (NTuple' xs) where
    show tup = "(tup " ++ (show $ ShowBox ((len tup)-1) tup) ++ ")"
        where
            n = V.length (getvec tup)
--             n = fromIntegral $ fromSing (sing :: Sing n)    

instance 
    ( Show x
    , Show (ShowBox (NTuple' xs))
--     , SingI n
    ) => Show (ShowBox (NTuple' (x ': xs))) where
    show (ShowBox i tup) = show (unsafeCoerce (getvec tup V.! i) :: x)++
        if i>0
           then " "++show (ShowBox (i-1) $ (unsafeCoerce tup::(NTuple' xs)))
           else ""
        where
            n = V.length (getvec tup)
--             n = fromIntegral $ fromSing (sing :: Sing n)

-------------------------------------------------------------------------------
-- modification

class EmptyTup tup where
    emptytup :: tup

instance EmptyTup (NTuple' xs) where
    emptytup = NTuple'
        { len = 0
        , getvec = V.replicate n UnsafeBox
        }
        where
            n = 2
--           n = fromIntegral $ fromSing (sing :: Sing n)

class PushBack tup a tup' | tup a -> tup' where
    pushback :: tup -> a -> tup'
    
instance PushBack (NTuple' xs) a (NTuple' (a ': xs)) where
    pushback tup a = if n<V.length (getvec tup)
        then NTuple'
            { len = (len tup)+1
            , getvec = runST $ do
                v <- V.unsafeThaw $ (getvec tup)
                VM.write v (len tup) (unsafeCoerce a)
                V.unsafeFreeze v
            }
        else NTuple'
            { len = (len tup)+1
            , getvec = V.generate ((len tup)*2) $ \i -> if i<(len tup)
                then (getvec tup) V.! i
                else unsafeCoerce a
            }
        where
            n = len tup

class TupIndexable index where
    index :: NTuple' xs -> index -> xs :! (ToNat1 (ExtractIndex index))
    
instance (SingI i) => TupIndexable (Index i) where
    index tup Index = unsafeCoerce (getvec tup V.! i)
        where
            i = fromIntegral $ fromSing (sing :: Sing i)

-------------------------------------------------------------------------------
-- type functions

data Index (n::Nat) = Index
type family ExtractIndex i :: Nat
type instance ExtractIndex (Index i) = i

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


-------------------------------------------------------------------------------
-- tests

-- tup1 = NTuple' 2 (V.replicate 2 45) :: NTuple' 1 '[Int]
-- tup2 = NTuple' 2 (V.replicate 2 45) :: NTuple' 2 '[Int,Int]
-- tup4 = NTuple' 4 (V.fromList [45,(unsafeCoerce 'a'),(unsafeCoerce "test"),(unsafeCoerce (10::Double))]) :: NTuple' 4 '[Int,Char,String,Double]

e3 = emptytup :: (NTuple' '[])
e8 = emptytup :: (NTuple' '[])

tup3 = pushback (pushback (pushback e3 "test") 89) 'h'

hl = hcons 34 $ hcons 'g' $ hcons "test" hnil
