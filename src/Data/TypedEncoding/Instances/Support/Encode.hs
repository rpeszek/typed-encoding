
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | v0.2 style encoding combinators
-- 
-- @since 0.3.0.0
module Data.TypedEncoding.Instances.Support.Encode where

import           Data.TypedEncoding.Instances.Support.Unsafe
import           Data.TypedEncoding.Combinators.Unsafe
import           Data.TypedEncoding.Common.Types.Enc
import           Data.Proxy
import           Data.TypedEncoding.Common.Types
import           GHC.TypeLits

-- | 
-- Create @"r-"@ - like encoding based on 'Data.TypedEncoding.Common.Class.fromEncF' like function. 
-- 
-- Useful for small not performance critical encoding since it executed provided @fromEnc@ function
-- to verify the encoding
--
-- this method does not restrict @nm@ to follow "r-" naming convention but is expected to be used to define @"r-"@ encoding. 
_implEncFromString :: forall nm err a c str . 
                       (KnownSymbol nm
                       , Show err) 
                       => 
                       (Enc '[nm] () str -> Either err a) 
                       -> Encoding (Either EncodeEx) nm (AlgNm nm) c str
_implEncFromString fn = UnsafeMkEncoding Proxy f
   where 
       f :: forall xs . Enc xs c str -> Either EncodeEx (Enc (nm ': xs) c str)
       f enc = 
           case fn (unsafeSetPayload () $ getPayload enc) of
               Right _ -> Right $ withUnsafeCoerce id enc
               Left err ->  Left $ EncodeEx (Proxy :: Proxy nm) err

-- * Compiler figures out algorithm, these appear fast enough 

_implEncodingP :: forall nm f c str . Applicative f => (str -> str) -> Encoding f nm (AlgNm nm)  c str
_implEncodingP f = _mkEncoding $ implTranF (pure . f)

_implEncodingConfP :: forall nm f c str . Applicative f => (c -> str -> str) -> Encoding f nm (AlgNm nm) c str
_implEncodingConfP f = _mkEncoding $ implTranF' (\c -> pure . f c)

_implEncodingEx :: forall nm err c str . (KnownSymbol nm, Show err) => (str -> Either err str) -> Encoding (Either EncodeEx) nm (AlgNm nm) c str
_implEncodingEx f = _mkEncoding $ implTranF (either (Left . EncodeEx p) Right . f) 
   where
        p = Proxy :: Proxy nm

_implEncodingEncodeEx :: forall nm c str . (KnownSymbol nm) => (str -> Either EncodeEx str) -> Encoding (Either EncodeEx) nm (AlgNm nm) c str
_implEncodingEncodeEx f = _mkEncoding $ implTranF f 


_implEncodingConfEx :: forall nm err c str . (KnownSymbol nm, Show err) => (c -> str -> Either err str) -> Encoding (Either EncodeEx) nm (AlgNm nm) c str
_implEncodingConfEx f = _mkEncoding $ implTranF' (\c -> either (Left . EncodeEx p) Right . f c) 
    where
        p = Proxy :: Proxy nm



-- * Assume @alg ~ nm@ or explicit @alg@ 

implEncodingP :: forall nm f c str . Applicative f => (str -> str) -> Encoding f nm nm c str
implEncodingP f = UnsafeMkEncoding Proxy $ implTranF (pure . f)

implEncodingEx :: forall nm err c str . (KnownSymbol nm, Show err) =>  (str -> Either err str) -> Encoding (Either EncodeEx) nm nm c str
implEncodingEx = implEncodingEx' @nm @nm

implEncodingEx' :: forall alg nm err c str . (KnownSymbol nm, Show err) =>  (str -> Either err str) -> Encoding (Either EncodeEx) nm alg c str
implEncodingEx' f = UnsafeMkEncoding Proxy $ implTranF (either (Left . EncodeEx p) Right . f) 
   where
        p = Proxy :: Proxy nm

implEncodingEncodeEx' :: forall alg nm c str . (KnownSymbol nm) =>  (str -> Either EncodeEx str) -> Encoding (Either EncodeEx) nm alg c str
implEncodingEncodeEx' f = UnsafeMkEncoding Proxy $ implTranF f 
   where
        p = Proxy :: Proxy nm


