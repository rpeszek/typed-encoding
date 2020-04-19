{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Encoding.Unsafe where

import           Data.Proxy
import           Data.Functor.Identity
import           Data.Encoding.Internal.Types

 
-- | Allows to operate within Enc. These are considered unsafe.
-- keeping the same list of encodings 
newtype Unsafe enc conf str = Unsafe {runUnsafe :: Enc enc conf str} deriving (Show)

withUnsafe :: (Unsafe e c s1 -> Unsafe e c s2) -> Enc e c s1 -> Enc e c s2
withUnsafe f enc = runUnsafe . f $ Unsafe enc

instance Functor (Unsafe enc conf) where
    fmap f (Unsafe (MkEnc p c x)) = Unsafe (MkEnc p c (f x))
  
instance Applicative (Unsafe enc ()) where
    pure = Unsafe . MkEnc Proxy ()  
    Unsafe (MkEnc p c1 f) <*> Unsafe (MkEnc _ c2 x) = Unsafe (MkEnc p () (f x))  

instance Monad (Unsafe enc ()) where 
    Unsafe (MkEnc _ _ x) >>= f = f x     

--TODO JSON instances    


