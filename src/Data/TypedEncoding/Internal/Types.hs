{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Internal definition of types

module Data.TypedEncoding.Internal.Types where

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

-- TODO make all implTran functions module-private
-- TODO disambiguate implEncode from implDecode, from implCheckPrevF for type safety
-- especially since these are always used in combo with asRecreateErr or asUnexpected 

implTranF :: Functor f => (str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranF f  = implTranF' (\c -> f)

-- TODO could this type be more precise?
implEncodeF :: (Show err, KnownSymbol x) => Proxy x -> (str -> Either err str) ->  Enc enc1 conf str -> Either EncodeEx (Enc enc2 conf str) 
implEncodeF p f = implTranF (either (Left . EncodeEx p) Right . f) 

implDecodeF :: Functor f => (str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implDecodeF = implTranF

implCheckPrevF :: Functor f => (str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implCheckPrevF = implTranF


implTranF' :: Functor f =>  (conf -> str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranF' f (MkEnc _ conf str) = (MkEnc Proxy conf) <$> f conf str

implEncodeF' :: (Show err, KnownSymbol x) => Proxy x -> (conf -> str -> Either err str) ->  Enc enc1 conf str -> Either EncodeEx (Enc enc2 conf str) 
implEncodeF' p f = implTranF' (\c -> either (Left . EncodeEx p) Right . f c) 

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



-- | Represents errors in recovery (recreation of encoded types).
data RecreateEx where
    RecreateEx:: (Show e, KnownSymbol x) => Proxy x -> e -> RecreateEx 

instance Show RecreateEx where
    show (RecreateEx prxy a) = "(RecreateEx \"" ++ symbolVal prxy ++ "\" (" ++ show a ++ "))"


-- | Represents errors in encoding
data EncodeEx where
    EncodeEx:: (Show a, KnownSymbol x) => Proxy x -> a -> EncodeEx 

instance Show EncodeEx where
    show (EncodeEx prxy a) = "(EncodeEx \"" ++ symbolVal prxy ++ "\" (" ++ show a ++ "))"



-- | Type safety over encodings makes decoding process safe.
-- However failures are still possible due to bugs or unsafe payload modifications.
-- UnexpectedDecodeEx represents such errors.
data UnexpectedDecodeEx where
    UnexpectedDecodeEx :: (Show a, KnownSymbol x) => Proxy x -> a -> UnexpectedDecodeEx

instance Show UnexpectedDecodeEx where
    show (UnexpectedDecodeEx prxy a) = "(UnexpectedDecodeEx \"" ++ symbolVal prxy ++ "\" (" ++ show a ++ "))"
