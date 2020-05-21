
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}


module Data.TypedEncoding.Instances.Support.Encode where

import           Data.TypedEncoding.Instances.Support.Deprecated
import           Data.TypedEncoding.Internal.Enc
import           Data.String
import           Data.Proxy
import           Text.Read
import           Data.TypedEncoding.Common.Types
import           Data.TypedEncoding.Combinators.Unsafe
import           Data.TypedEncoding.Common.Class.IsStringR 
import           GHC.TypeLits


-- * Compiler figure out algorithm, these appear fast enough 

_implEncodingP :: forall nm f c str . Applicative f => (str -> str) -> Encoding f nm (AlgNm nm)  c str
_implEncodingP f = _mkEncoding $ implTranF (pure . f)

_implEncodingConfP :: forall nm f c str . Applicative f => (c -> str -> str) -> Encoding f nm (AlgNm nm) c str
_implEncodingConfP f = _mkEncoding $ implTranF' (\c -> pure . f c)

_implEncodingEx :: forall nm err c str . (KnownSymbol nm, Show err) => (str -> Either err str) -> Encoding (Either EncodeEx) nm (AlgNm nm) c str
_implEncodingEx f = _mkEncoding $ implTranF (either (Left . EncodeEx p) Right . f) 
   where
        p = Proxy :: Proxy nm

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

