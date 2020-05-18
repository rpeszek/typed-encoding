{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- |
-- Internal definition of types
--
-- Decoding types for @Enc@
module Data.TypedEncoding.Internal.Types.Decoding where

import           Data.Proxy
import           GHC.TypeLits

import           Data.TypedEncoding.Internal.Types.Enc
import           Data.TypedEncoding.Internal.Common


data Decoding f (nm :: Symbol) (alg :: Symbol) conf str where
    -- | Consider this constructor as private or use it with care
    --
    -- Using this constructor:
    -- @
    -- MkDecoding :: Proxy nm -> (forall (xs :: [Symbol]) . Enc (nm ': xs) conf str -> f (Enc xs conf str)) -> Decoding f nm (AlgNm nm) conf str
    -- @
    -- 
    -- would make compilation much slower
    UnsafeMkDecoding :: Proxy nm -> (forall (xs :: [Symbol]) . Enc (nm ': xs) conf str -> f (Enc xs conf str)) -> Decoding f nm alg conf str

-- | Type safe smart constructor
-- adding the type family @(AlgNm nm)@ restriction to UnsafeMkDecoding slows down compilation, especially in tests.      
mkDecoding :: forall f (nm :: Symbol) conf str . (forall (xs :: [Symbol]) . Enc (nm ': xs) conf str -> f (Enc xs conf str)) -> Decoding f nm (AlgNm nm) conf str
mkDecoding = UnsafeMkDecoding Proxy

runDecoding :: forall alg nm f xs conf str . Decoding f nm alg conf str -> Enc (nm ': xs) conf str -> f (Enc xs conf str)
runDecoding (UnsafeMkDecoding _ fn) = fn

-- | Same as 'runDecoding" but compiler figures out algorithm name
--
-- Using it can slowdown compilation
_runDecoding :: forall nm f xs conf str alg . (AlgNm nm ~ alg) => Decoding f nm alg conf str -> Enc (nm ': xs) conf str -> f (Enc xs conf str)
_runDecoding = runDecoding @(AlgNm nm)

-- |
-- Wraps a list of @Decoding@ elements.
--
-- Similarly to 'Decoding' it can be used with a typeclass
-- 'Data.TypedDecoding.Internal.Class.Encode.EncodeAll'
data Decodings f (nms :: [Symbol]) (algs :: [Symbol]) conf str where
    -- | constructor is to be treated as Unsafe to Encode and Decode instance implementations
    -- particular encoding instances may expose smart constructors for limited data types
    ZeroD :: Decodings f '[] '[] conf str
    AppendD ::  Decoding f nm alg conf str -> Decodings f nms algs conf str -> Decodings f (nm ': nms) (alg ': algs) conf str

runDecodings :: forall algs nms f c str . (Monad f) => Decodings f nms algs c str -> Enc nms c str -> f (Enc ('[]::[Symbol]) c str)
runDecodings ZeroD enc0 = pure enc0
runDecodings (AppendD fn xs) enc = 
        let re :: f (Enc _ c str) = runDecoding fn enc
        in re >>= runDecodings xs


-- | At possibly big compilation cost, have compiler figure out algorithm names.
_runDecodings :: forall nms f c str algs . (Monad f, algs ~ AlgNmMap nms) => Decodings f nms algs c str -> Enc nms c str -> f (Enc ('[]::[Symbol]) c str)
_runDecodings = runDecodings @(AlgNmMap nms)