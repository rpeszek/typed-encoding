{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Internal definition of types

module Data.Encoding.Internal.Types where

import           Data.Proxy
import           Data.Functor.Identity
import           GHC.TypeLits

-- Not a Functor on purpose
data Enc enc conf str where
    -- | constructor is to be treated as Unsafe to Encode and Decode instance implementations
    -- particular encoding instances may expose smart constructors for limited data types
    MkEnc :: Proxy enc -> conf -> str -> Enc enc conf str
    deriving (Show, Eq) 
 
toEncoding :: conf -> str -> Enc '[] conf str
toEncoding conf str = MkEnc Proxy conf str

fromEncoding :: Enc '[] conf str -> str
fromEncoding = getPayload

implTranF :: Functor f => (str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranF f  = implTranF' (\c -> f)

implTranF' :: Functor f =>  (conf -> str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranF' f (MkEnc _ conf str) = (MkEnc Proxy conf) <$> f conf str

implTranP :: Applicative f => (str -> str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranP f  = implTranF' (\c -> pure . f)

implTranP' :: Applicative f => (conf -> str -> str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranP' f  = implTranF' (\c -> pure . f c)

showEnc :: forall s c str xs. (KnownSymbol s, Show c, Show str) => Enc (s ': xs) c str -> String
showEnc (MkEnc _ c s) = "MkEnc [" ++ symbolVal (Proxy :: Proxy s) ++ " ...] " ++ show c ++ " " ++ show s

getPayload :: Enc enc conf str -> str  
getPayload (MkEnc _ _ str) = str

unsafeSetPayload :: conf -> str -> Enc enc conf str 
unsafeSetPayload c str = MkEnc Proxy c str

withUnsafeCoerce ::  (s1 -> s2) -> Enc e1 c s1 -> Enc e2 c s2
withUnsafeCoerce f (MkEnc _ conf str)  = (MkEnc Proxy conf (f str)) 

unsafeChangePayload ::  (s1 -> s2) -> Enc e c s1 -> Enc e c s2
unsafeChangePayload f (MkEnc p conf str)  = (MkEnc p conf (f str)) 

-- | Representing errors in recovery (recreation of encoded types).
newtype RecreateEx = RecreateEx String deriving (Show, Eq)

-- | Type safety over encodings makes decoding process safe.
-- However failures are still possible due to bugs or unsafe payload modifications.
-- UnexpectedDecodeEx represents such errors.
newtype UnexpectedDecodeEx = UnexpectedDecodeEx String deriving (Show, Eq)