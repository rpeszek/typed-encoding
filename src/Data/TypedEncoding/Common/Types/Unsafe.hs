{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Unsafe ops inside encoding
module Data.TypedEncoding.Common.Types.Unsafe where

import           Data.Proxy
import           Data.TypedEncoding.Common.Types

 
-- | Allows to operate within Enc. These are considered unsafe.
-- keeping the same list of encodings 
--
-- @since 0.1.0.0 
newtype Unsafe enc conf str = Unsafe {runUnsafe :: Enc enc conf str} deriving (Show)

-- |
-- @since 0.1.0.
withUnsafe :: (Unsafe e c s1 -> Unsafe e c s2) -> Enc e c s1 -> Enc e c s2
withUnsafe f enc = runUnsafe . f $ Unsafe enc

instance Functor (Unsafe enc conf) where
    fmap f (Unsafe (UnsafeMkEnc p c x)) = Unsafe (UnsafeMkEnc p c (f x))
  
instance Applicative (Unsafe enc ()) where
    pure = Unsafe . UnsafeMkEnc Proxy ()  
    Unsafe (UnsafeMkEnc p c1 f) <*> Unsafe (UnsafeMkEnc _ c2 x) = Unsafe (UnsafeMkEnc p () (f x))  

instance Monad (Unsafe enc ()) where 
    Unsafe (UnsafeMkEnc _ _ x) >>= f = f x     

