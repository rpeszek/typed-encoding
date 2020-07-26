
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Common decoding combinators
-- 
-- @since 0.3.0.0
module Data.TypedEncoding.Instances.Support.Decode where

import           Data.TypedEncoding.Instances.Support.Unsafe
import           Data.TypedEncoding.Common.Types.Decoding
import           Data.Proxy
import           Data.TypedEncoding.Common.Types

-- * Universal decoding for all "r-" types

-- | 
--
-- @since 0.3.0.0
decAnyR :: forall r f c str . (Restriction r, Applicative f) => Decoding f r r c str
decAnyR = decAnyR' @r @r

-- |
-- @since 0.3.0.0
decAnyR' :: forall alg r f c str . (Restriction r, Applicative f) => Decoding f r alg c str
decAnyR' = UnsafeMkDecoding Proxy (implTranP id) 

-- |
-- @since 0.3.0.0
decAnyR_ :: forall r f c str alg . (Restriction r, Algorithm r alg, Applicative f) => Decoding f r alg c str
decAnyR_ = _mkDecoding $ implTranP id


-- * v0.2 style decoding combinators

-- * Compiler figure out algorithm, these appear fast enough 

_implDecodingF :: forall nm f c str . Functor f => (str -> f str) -> Decoding f nm (AlgNm nm) c str
_implDecodingF f = _mkDecoding $ implTranF f

_implDecodingConfF :: forall nm f c str . Functor f => (c -> str -> f str) -> Decoding f nm (AlgNm nm) c str
_implDecodingConfF f = _mkDecoding $ implTranF' f


-- * Assume @alg ~ nm@ or explicit @alg@ 

implDecodingF :: forall nm f c str . Functor f => (str -> f str) -> Decoding f nm nm c str
implDecodingF = implDecodingF' @nm @nm

implDecodingF' :: forall alg nm f c str . Functor f => (str -> f str) -> Decoding f nm alg c str
implDecodingF' f = UnsafeMkDecoding Proxy $ implTranF f



