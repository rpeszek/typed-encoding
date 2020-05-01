{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE RankNTypes #-}

-- |
-- Internal definition of types

module Data.TypedEncoding.Internal.Types.Enc where

import           Data.Proxy
import           Data.Functor.Identity
import           GHC.TypeLits

import           Data.TypedEncoding.Internal.Class.Util

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XAllowAmbiguousTypes
-- >>> import qualified Data.Text as T

-- Not a Functor on purpose
data Enc enc conf str where
    -- | constructor is to be treated as Unsafe to Encode and Decode instance implementations
    -- particular encoding instances may expose smart constructors for limited data types
    MkEnc :: Proxy enc -> conf -> str -> Enc enc conf str
    deriving (Show, Eq) 

-- |
-- >>> let disptest = unsafeSetPayload () "hello" :: Enc '["TEST"] () T.Text
-- >>> displ disptest
-- "MkEnc '[TEST] () (Text hello)"
instance (KnownAnnotation xs, Show c, Displ str) => Displ ( Enc xs c str) where
    displ (MkEnc p c s) = 
        "MkEnc '" ++ displ (Proxy :: Proxy xs) ++ " " ++ show c ++ " " ++ displ s


toEncoding :: conf -> str -> Enc '[] conf str
toEncoding conf str = MkEnc Proxy conf str

fromEncoding :: Enc '[] conf str -> str
fromEncoding = getPayload

-- TODO make all implTran functions module-private
-- TODO disambiguate implEncode from implDecode, from implCheckPrevF for type safety
-- especially since these are always used in combo with asRecreateErr_ or asUnexpected 

implTranF :: Functor f => (str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranF f  = implTranF' (\c -> f)


implDecodeF :: Functor f => (str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implDecodeF = implTranF

implCheckPrevF :: Functor f => (str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implCheckPrevF = implTranF


implTranF' :: Functor f =>  (conf -> str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranF' f (MkEnc _ conf str) = (MkEnc Proxy conf) <$> f conf str


implDecodeF' :: Functor f =>  (conf -> str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implDecodeF' = implTranF'

implTranP :: Applicative f => (str -> str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranP f  = implTranF' (\c -> pure . f)

implEncodeP :: Applicative f => (str -> str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implEncodeP = implTranP

implTranP' :: Applicative f => (conf -> str -> str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranP' f  = implTranF' (\c -> pure . f c)

implEncodeP' :: Applicative f => (conf -> str -> str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implEncodeP' = implTranP'


getPayload :: Enc enc conf str -> str  
getPayload (MkEnc _ _ str) = str

unsafeSetPayload :: conf -> str -> Enc enc conf str 
unsafeSetPayload c str = MkEnc Proxy c str

withUnsafeCoerce ::  (s1 -> s2) -> Enc e1 c s1 -> Enc e2 c s2
withUnsafeCoerce f (MkEnc _ conf str)  = (MkEnc Proxy conf (f str)) 

unsafeChangePayload ::  (s1 -> s2) -> Enc e c s1 -> Enc e c s2
unsafeChangePayload f (MkEnc p conf str)  = (MkEnc p conf (f str)) 

