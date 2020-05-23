
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | v0.2 style decoding combinators
module Data.TypedEncoding.Instances.Support.Decode where

import           Data.TypedEncoding.Instances.Support.Unsafe
import           Data.TypedEncoding.Common.Types.Decoding
import           Data.Proxy
import           Data.TypedEncoding.Common.Types



-- * Compiler figure out algorithm, these appear fast enough 

_implDecodingF :: forall nm f c str . Functor f => (str -> f str) -> Decoding f nm (AlgNm nm) c str
_implDecodingF f = mkDecoding $ implTranF f

_implDecodingConfF :: forall nm f c str . Functor f => (c -> str -> f str) -> Decoding f nm (AlgNm nm) c str
_implDecodingConfF f = mkDecoding $ implTranF' f


-- * Assume @alg ~ nm@ or explicit @alg@ 

implDecodingF :: forall nm f c str . Functor f => (str -> f str) -> Decoding f nm nm c str
implDecodingF = implDecodingF' @nm @nm

implDecodingF' :: forall alg nm f c str . Functor f => (str -> f str) -> Decoding f nm alg c str
implDecodingF' f = UnsafeMkDecoding Proxy $ implTranF f



