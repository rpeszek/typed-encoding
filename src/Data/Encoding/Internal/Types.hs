{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Encoding.Internal.Types where

import           Data.Proxy
import           Data.Functor.Identity

-- Not a Functor on purpose
data Enc enc conf str where
    -- | constructor is to be treated as Unsafe to Encode and Decode instance implementations
    -- particular encoding instances may expose smart constructors for limited data types
    MkEnc :: Proxy enc -> conf -> str -> Enc enc conf str
    deriving (Show, Eq) 
 
toEncoding :: conf -> str -> Enc '[] conf str
toEncoding conf str = MkEnc Proxy conf str

fromEncoding :: Enc '[] conf str -> str
fromEncoding = unsafeGetPayload

implTranF :: Functor f => (str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranF f  = implTranF' (\c -> f)

implTranF' :: Functor f =>  (conf -> str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranF' f (MkEnc _ conf str) = (MkEnc Proxy conf) <$> f conf str


implTranP :: Applicative f => (str -> str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranP f  = implTranF' (\c -> pure . f)

implTranP' :: Applicative f => (conf -> str -> str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranP' f  = implTranF' (\c -> pure . f c)

unsafeGetPayload :: Enc enc conf str -> str  
unsafeGetPayload (MkEnc _ _ str) = str

withUnsafeCoerce ::  (s1 -> s2) -> Enc e1 c s1 -> Enc e2 c s2
withUnsafeCoerce f (MkEnc _ conf str)  = (MkEnc Proxy conf (f str)) 

