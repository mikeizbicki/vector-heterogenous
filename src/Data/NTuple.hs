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

module Data.NNTuple'
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

data NTuple (xs :: [*]) = forall n. NTuple (NTuple' n xs)

data NTuple' (n::Nat) (xs::[*]) = NTuple' 
    { len :: Int
    , getvec :: V.Vector Int
    }


-- class CountArgs t where
--     countArgs :: Int -> t
--  
-- instance CountArgs Int where
--     countArgs args = args
--  
-- instance (CountArgs r) => CountArgs (a -> r) where
--     countArgs count = \a -> countArgs (count + 1)
--  
-- -- count :: (CountArgs t) => t
-- count args = NTuple' 
--     where
--         n = countArgs 0 args

-------------------------------------------------------------------------------
-- showing

data ShowBox a = ShowBox Int a

instance Show (ShowBox (NTuple' n '[])) where
    show _ = ""
    
instance (Show (ShowBox (NTuple' n xs)), SingI n) => Show (NTuple' n xs) where
    show tup = "(tup " ++ (show $ ShowBox ((len tup)-1) tup) ++ ")"
        where
            n = fromIntegral $ fromSing (sing :: Sing n)    

instance 
    ( Show x
    , Show (ShowBox (NTuple' n xs))
    , SingI n
    ) => Show (ShowBox (NTuple' n (x ': xs))) where
    show (ShowBox i tup) = show (unsafeCoerce (getvec tup V.! i) :: x)++
        if i>0
           then " "++show (ShowBox (i-1) $ (unsafeCoerce tup::(NTuple' n xs)))
           else ""
        where
            n = fromIntegral $ fromSing (sing :: Sing n)

-------------------------------------------------------------------------------
-- modification

class EmptyTup tup where
    emptytup :: tup

instance (SingI n) => EmptyTup (NTuple' n xs) where
    emptytup = NTuple'
        { len = 0
        , getvec = V.replicate n 0
        }
        where
            n = fromIntegral $ fromSing (sing :: Sing n)

class PushBack tup a tup' | tup a -> tup' where
    pushback :: tup -> a -> tup'
    
-- instance (Length xs <= n) => PushBack (NTuple' n xs) a (NTuple' n (a ': xs)) where
instance PushBack (NTuple' n xs) a (NTuple' n (a ': xs)) where
    pushback tup a = NTuple'
        { len = (len tup)+1
        , getvec = runST $ do
            v <- V.unsafeThaw $ (getvec tup)
            VM.write v (len tup) (unsafeCoerce a)
            V.unsafeFreeze v
        }

class TupIndexable index where
    index :: NTuple' n xs -> index -> xs :! (ToNat1 (ExtractIndex index))
    
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

type family (:!) (xs::[*]) (i::Nat1) :: *
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

tup1 = NTuple' 2 (V.replicate 2 45) :: NTuple' 1 '[Int]
tup2 = NTuple' 2 (V.replicate 2 45) :: NTuple' 2 '[Int,Int]
tup4 = NTuple' 4 (V.fromList [45,(unsafeCoerce 'a'),(unsafeCoerce "test"),(unsafeCoerce (10::Double))]) :: NTuple' 4 '[Int,Char,String,Double]

e3 = emptytup :: (NTuple' 3 '[])
e8 = emptytup :: (NTuple' 8 '[])

