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
-- Validation types for @Enc@
--
-- See also
--
-- * "Data.TypedEncoding.Combinators.Validate"
-- * "Data.TypedEncoding.Common.Class.Validate"
--
-- Use of 'Data.TypedEncoding.Combinators.Unsafe.unsafeSetPayload' currently recommended
-- for recovering 'Enc' from trusted input sources (if avoiding cost of Validation is important).
--
-- This module is re-exported in "Data.TypedEncoding" and it is best not to import it directly.

module Data.TypedEncoding.Common.Types.Validation where

import           Data.Proxy
import           GHC.TypeLits

import           Data.TypedEncoding.Common.Types.Enc
import           Data.TypedEncoding.Common.Types.Common


-- |
-- Validation unwraps a layer of encoding and offers payload data down the encoding stack
-- for further verification. 
--
-- For "enc-" encodings this will typically be decoding step.
-- 
-- For "r-" encodings this will typically be encoding step.
--
-- @since 0.3.0.0
data Validation f (nm :: Symbol) (alg :: Symbol) conf str where
    UnsafeMkValidation :: Proxy nm -> (forall (xs :: [Symbol]) . Enc (nm ': xs) conf str -> f (Enc xs conf str)) -> Validation f nm alg conf str

-- | Type safe smart constructor
-- adding the type family @(AlgNm nm)@ restriction to UnsafeMkValidation slows down compilation, especially in tests.      
--
-- @since 0.3.0.0
mkValidation :: forall f (nm :: Symbol) conf str . (forall (xs :: [Symbol]) . Enc (nm ': xs) conf str -> f (Enc xs conf str)) -> Validation f nm (AlgNm nm) conf str
mkValidation = UnsafeMkValidation Proxy
{-# DEPRECATED mkValidation "Use _mkValidation" #-}

-- | Type safe smart constructor
-- adding the type family @(AlgNm nm)@ restriction to UnsafeMkValidation slows down compilation, especially in tests.      
--
-- This function follows the naming convention of using "_" when the typechecker figures out @alg@
--
-- @since 0.5.0.0
_mkValidation :: forall f (nm :: Symbol) conf str . (forall (xs :: [Symbol]) . Enc (nm ': xs) conf str -> f (Enc xs conf str)) -> Validation f nm (AlgNm nm) conf str
_mkValidation = UnsafeMkValidation Proxy


runValidation :: forall nm f xs conf str . Validation f nm nm conf str -> Enc (nm ': xs) conf str -> f (Enc xs conf str)
runValidation (UnsafeMkValidation _ fn) = fn

runValidation' :: forall alg nm f xs conf str . Validation f nm alg conf str -> Enc (nm ': xs) conf str -> f (Enc xs conf str)
runValidation' (UnsafeMkValidation _ fn) = fn

-- | Same as 'runValidation" but compiler figures out algorithm name
--
-- Using it can slowdown compilation
--
-- @since 0.3.0.0
_runValidation :: forall nm f xs conf str alg . (AlgNm nm ~ alg) => Validation f nm alg conf str -> Enc (nm ': xs) conf str -> f (Enc xs conf str)
_runValidation = runValidation' @(AlgNm nm)

-- |
-- Wraps a list of @Validation@ elements.
--
-- Similarly to 'Validation' it can be used with a typeclass
-- 'Data.TypedValidation.Internal.Class.Encode.EncodeAll'
--
-- @since 0.3.0.0
data Validations f (nms :: [Symbol]) (algs :: [Symbol]) conf str where
    -- | constructor is to be treated as Unsafe to Encode and Decode instance implementations
    -- particular encoding instances may expose smart constructors for limited data types
    ZeroV :: Validations f '[] '[] conf str
    ConsV ::  Validation f nm alg conf str -> Validations f nms algs conf str -> Validations f (nm ': nms) (alg ': algs) conf str



-- | This basically puts payload in decoded state.
-- More useful combinators are in "Data.TypedEncoding.Combinators.Validate"
--
-- (@runValidationChecks@ before v0.5)
--
-- @since 0.5.0.0
runValidationChecks' :: forall algs nms f c str . (Monad f) => Validations f nms algs c str -> Enc nms c str -> f (Enc ('[]::[Symbol]) c str)
runValidationChecks' ZeroV enc0 = pure enc0
runValidationChecks' (ConsV fn xs) enc = 
        let re :: f (Enc _ c str) = runValidation' fn enc
        in re >>= runValidationChecks' xs


-- -- | At possibly big compilation cost, have compiler figure out algorithm names.
-- _runValidations :: forall nms f c str algs . (Monad f, algs ~ AlgNmMap nms) => Validations f nms algs c str -> Enc nms c str -> f (Enc ('[]::[Symbol]) c str)
-- _runValidations = runValidations @(AlgNmMap nms)