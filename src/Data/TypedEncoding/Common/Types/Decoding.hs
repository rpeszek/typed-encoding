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
-- Decoding types for @Enc@
--
-- This module is re-exported in "Data.TypedEncoding" and it is best not to import it directly.
module Data.TypedEncoding.Common.Types.Decoding where

import           Data.Proxy
import           GHC.TypeLits

import           Data.TypedEncoding.Common.Types.Enc
import           Data.TypedEncoding.Common.Types.Common

-- |
-- Similar to 'Data.TypedEncoding.Common.Types.Enc.Encoding'
--
-- Wraps the decoding function.
--
-- Can be used with 'Data.TypedEncoding.Common.Class.Decode.Decode' type class.
-- 
-- "Examples.TypedEncoding.Instances.DiySignEncoding" contains an implementation example.
--
-- "Examples.TypedEncoding.Overview" shows decoding usage examples.
--
-- @since 0.3.0.0
data Decoding f (nm :: Symbol) (alg :: Symbol) conf str where
    -- | Consider this constructor as private or use it with care
    --
    -- Using that constructor:
    -- @
    -- UnsafeMkDecoding :: Proxy nm -> (forall (xs :: [Symbol]) . Enc (nm ': xs) conf str -> f (Enc xs conf str)) -> Decoding f nm (AlgNm nm) conf str
    -- @
    -- 
    -- would make compilation much slower
    UnsafeMkDecoding :: Proxy nm -> (forall (xs :: [Symbol]) . Enc (nm ': xs) conf str -> f (Enc xs conf str)) -> Decoding f nm alg conf str

-- | Type safe smart constructor
-- (See also 'Data.TypedEncoding.Common.Types.Enc._mkEncoding')  
--
-- @since 0.3.0.0    
mkDecoding :: forall f (nm :: Symbol) conf str . (forall (xs :: [Symbol]) . Enc (nm ': xs) conf str -> f (Enc xs conf str)) -> Decoding f nm (AlgNm nm) conf str
mkDecoding = _mkDecoding

{-# DEPRECATED mkDecoding "Use _mkDecoding" #-}



-- | Type safe smart constructor
-- (See also 'Data.TypedEncoding.Common.Types.Enc._mkEncoding')  
-- 
-- This function follows the naming convention of using "_" when the typechecker figures out @alg@
-- @since 0.5.0.0    
_mkDecoding :: forall f (nm :: Symbol) conf str . (forall (xs :: [Symbol]) . Enc (nm ': xs) conf str -> f (Enc xs conf str)) -> Decoding f nm (AlgNm nm) conf str
_mkDecoding = UnsafeMkDecoding Proxy

-- |
-- This function assumes @mn ~ alg@, making its type different from previous (before v.0.5) versions.
--
-- @since 0.5.0.0
runDecoding :: forall nm f xs conf str . Decoding f nm nm conf str -> Enc (nm ': xs) conf str -> f (Enc xs conf str)
runDecoding (UnsafeMkDecoding _ fn) = fn

-- |
-- @since 0.3.0.0
runDecoding' :: forall alg nm f xs conf str . Decoding f nm alg conf str -> Enc (nm ': xs) conf str -> f (Enc xs conf str)
runDecoding' (UnsafeMkDecoding _ fn) = fn

-- | Same as 'runDecoding" but compiler figures out algorithm name
--
-- Using it can slowdown compilation
--
-- @since 0.3.0.0  
_runDecoding :: forall nm f xs conf str alg . (AlgNm nm ~ alg) => Decoding f nm alg conf str -> Enc (nm ': xs) conf str -> f (Enc xs conf str)
_runDecoding = runDecoding' @(AlgNm nm)

-- |
-- Wraps a list of @Decoding@ elements.
--
-- Similarly to 'Data.TypedEncoding.Common.Types.Enc.Encodings' can be used with a typeclass
-- 'Data.TypedDecoding.Internal.Class.Decode.DecodeAll'
--
-- @since 0.3.0.0  
data Decodings f (nms :: [Symbol]) (algs :: [Symbol]) conf str where
    -- | constructor is to be treated as Unsafe to Encode and Decode instance implementations
    -- particular encoding instances may expose smart constructors for limited data types
    ZeroD :: Decodings f '[] '[] conf str
    ConsD ::  Decoding f nm alg conf str -> Decodings f nms algs conf str -> Decodings f (nm ': nms) (alg ': algs) conf str

-- |
-- This function assumes @nms ~ algs@, making its type different from previous (before v.0.5) versions.
--
-- @since 0.5.0.0
runDecodings :: forall nms f c str . (Monad f) => Decodings f nms nms c str -> Enc nms c str -> f (Enc ('[]::[Symbol]) c str)
runDecodings = runDecodings' @nms @nms


runDecodings' :: forall algs nms f c str . (Monad f) => Decodings f nms algs c str -> Enc nms c str -> f (Enc ('[]::[Symbol]) c str)
runDecodings' ZeroD enc0 = pure enc0
runDecodings' (ConsD fn xs) enc = 
        let re :: f (Enc _ c str) = runDecoding' fn enc
        in re >>= runDecodings' xs

-- | At possibly big compilation cost, have compiler figure out algorithm names.
--
-- @since 0.3.0.0  
_runDecodings :: forall nms f c str algs . (Monad f, algs ~ AlgNmMap nms) => Decodings f nms algs c str -> Enc nms c str -> f (Enc ('[]::[Symbol]) c str)
_runDecodings = runDecodings' @(AlgNmMap nms)