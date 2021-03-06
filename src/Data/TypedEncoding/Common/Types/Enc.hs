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
-- Contains main @Enc@ type that carries encoded payload as well as
-- @Encoding@ and @Encodings@ types contains encoding functions.
-- This module also contains basic combinators for these types.
--
-- This module is re-exported in "Data.TypedEncoding" and it is best not to import it directly.

module Data.TypedEncoding.Common.Types.Enc where

import           Data.Proxy
import           GHC.TypeLits

import           Data.TypedEncoding.Common.Class.Common
import           Data.TypedEncoding.Common.Types.Common

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XAllowAmbiguousTypes
-- >>> import qualified Data.ByteString as B
-- >>> import qualified Data.Text as T
-- >>> import Data.Functor.Identity
-- >>> import Data.TypedEncoding
-- >>> import Data.TypedEncoding.Instances.Enc.Base64 ()
-- >>> import Data.TypedEncoding.Instances.Restriction.BoundedAlphaNums (encFBan)

-- |
-- Contains encoded data annotated by 
--
-- * @nms@ list of @Symbol@s with encoding names (encoding stack)
-- * @conf@ that can contain configuration / encoding information such as digest.
-- * @str@ the encoded data
--
--  Example: 
--
-- @
-- Enc '["r-ASCII"] () ByteString
-- @
--
-- @since 0.1.0.0
data Enc nms conf str where
    -- | 
    -- @since 0.3.0.0 renamed from MkEnc 
    --
    -- Use of this constructor should be kept to a minimum. 
    --
    -- Use of 'Data.TypedEncoding.Combinators.Unsafe.unsafeSetPayload' currently recommended
    -- for recovering 'Enc' from trusted input sources (if avoiding cost of "Data.TypedEncoding.Common.Types.Validation" is important).
    UnsafeMkEnc :: Proxy nms -> conf -> str -> Enc nms conf str
    deriving (Show, Eq) 

-- TODO should Enc be a Functor in conf?

-- |
-- >>> let disptest = UnsafeMkEnc Proxy () "hello" :: Enc '["TEST"] () T.Text
-- >>> displ disptest
-- "Enc '[TEST] () (Text hello)"
instance (SymbolList xs, Show c, Displ str) => Displ ( Enc xs c str) where
    displ (UnsafeMkEnc p c s) = 
        "Enc '" ++ displ (Proxy :: Proxy xs) ++ " " ++ show c ++ " " ++ displ s

-- |
-- @since 0.1.0.0
toEncoding :: conf -> str -> Enc ('[] :: [Symbol]) conf str
toEncoding = UnsafeMkEnc Proxy 

-- |
-- @since 0.1.0.0
fromEncoding :: Enc '[] conf str -> str
fromEncoding = getPayload

-- |
-- @since 0.1.0.0
getPayload :: Enc enc conf str -> str  
getPayload (UnsafeMkEnc _ _ str) = str

-- |
-- @since 0.5.2.0
getContent :: Enc enc conf str -> (conf, str)   
getContent (UnsafeMkEnc _ c str) = (c, str)

-- |
-- Wraps the encoding function.
-- Contains type level information about the encoding name and the algorithm used. 
--
-- This type is used by programs implementing encoding instance.
-- Such program needs to define a value of this type. 
-- It also implements 'Data.TypedEncoding.Common.Class.Encode.Encode' instance that simply returns that value.
--
-- Programs using encoding can access this type using 'Data.TypedEncoding.Common.Class.Encode.Encode.encoding'
-- (from the @Encode@ typeclass) but a better (and recommended) approach is to use its plural sibling 'Encodings' 
-- defined below.
--
-- This type has 2 symbol type variables:
--
-- * @nm@  defines the encoding
-- * @alg@  defines algorithm 
--
-- These two are related, currently this library only supports 
--
-- * Names @nm@ containing ":" using format "alg:...", for example name "r-ban:999" has "r-ban" algorithm 
-- * Names without ":" require that @nm ~ alg@
--
-- Future version are likely to relax this, possibly introducing ability do define more than one algorithm
-- for given encoding. 
--
-- Using 2 variables allows us to define typeclass constraints that work
-- with definitions like @"r-ban"@ where @"r-ban:@" can be followed by arbitrary
-- string literal.
--
-- This existential definition is intended for clarity. /typed-encoding/ supports type level lists of encodings
-- and each encoding should not know what encodings have already been applied.
--
-- However, this construction is mostly equivalent to storing a simple one level encoding function 
-- @Enc ('[]:: [Symbol]) conf str -> f (Enc '[nm] conf str)@ 
-- (see '_mkEncoding1' and 'runEncoding1'' below).  
--
-- Examples: 
--
-- @
-- Encoding (Either EncodeEx) "r-ban:9" "r-ban" () String
-- @
--
-- encodes a single character @ <= 9'@
--
-- @
-- Encoding Identity "enc-B64" "enc-B64" () ByteString
-- @
--
-- Represents a /Base 64/ encoder that can operate on any stack of previous encodings.
-- (encoding name and algorithm name are "enc-B64", there is no  
-- additional configuration @()@ needed and it runs in the @Identity@ Functor.
--
-- Similar boilerplate for /Decoding/ and /Validation/ is specified in separate modules.
--
-- @since 0.3.0.0
data Encoding f (nm :: Symbol) (alg :: Symbol) conf str where
    -- | Consider this constructor as private or use it with care
    --
    -- Defining constructor like this:
    -- @
    -- MkEncoding :: Proxy nm -> (forall (xs :: [Symbol]) . Enc xs conf str -> f (Enc (nm ': xs) conf str)) -> Encoding f nm (AlgNm nm) conf str
    -- @
    -- 
    -- would make compilation much slower
    UnsafeMkEncoding :: Proxy nm -> (forall (xs :: [Symbol]) . Enc xs conf str -> f (Enc (nm ': xs) conf str)) -> Encoding f nm alg conf str



-- | Type safe smart constructor
--
-- Adding the type family @(AlgNm nm)@ mapping to @Encoding@ constructor slows down the compilation.  
-- Using smart constructor does not have that issue.
--
-- This approach also provides more future flexibility with possibility of future overloads relaxing current 
-- limitations on @alg@ names. 
-- 
-- /Notice underscore @_@ convention, it indicates a use of @Algorithm@ @AlgNm@: compiler figures out @alg@ value. These can be slower to compile when used. /
--
-- Here are other conventions that relate to the existence of @alg@
--
-- * functions ending with: @'@, for example 'Data.TypedEncoding.Combinators.Encode.encodeF'' have @alg@
--   as first type variable in the @forall@ list.
--
-- * functions without tick tend to assume @nm ~ alg@
-- 
-- This particular function appears to not increase compilation time. 
--
-- @since 0.3.0.0
_mkEncoding :: forall f (nm :: Symbol) conf str . 
         (forall (xs :: [Symbol]) . Enc xs conf str -> f (Enc (nm ': xs) conf str)) -> Encoding f nm (AlgNm nm) conf str
_mkEncoding = UnsafeMkEncoding Proxy

-- |
-- Defines encoding by only specifying a simple one level encoding function. 
-- This typically is not used in constructing encodings as there are more convenient combinators for doing this
-- (e.g. in "Data.TypedEncoding.Instances.Support").
-- It is here for completeness to show that the @Encoding@ definition is a bit overdone.
-- 
-- @since 0.5.2.0
_mkEncoding1 :: forall f (nm :: Symbol) conf str . 
        Functor f  => (Enc ('[]:: [Symbol]) conf str -> f (Enc '[nm] conf str)) -> Encoding f nm (AlgNm nm) conf str
_mkEncoding1 fn = UnsafeMkEncoding Proxy (fmap (mkenc Proxy . getContent) . fn . mkenc Proxy . getContent)
  where 
      mkenc p (c,s) = UnsafeMkEnc p c s
 
-- |
-- @since 0.3.0.0
runEncoding' :: forall alg nm f xs conf str . Encoding f nm alg conf str -> Enc xs conf str -> f (Enc (nm ': xs) conf str)
runEncoding' (UnsafeMkEncoding _ fn) = fn

-- |
-- Version of @runEncoding'@ function specialized to empty encoding
--
-- @since 0.5.2.0
runEncoding1' :: forall alg nm f  conf str . Encoding f nm alg conf str -> Enc ('[] :: [Symbol]) conf str -> f (Enc '[nm] conf str)
runEncoding1'  = runEncoding' @alg @nm @f @'[]

-- | Same as 'runEncoding'' but compiler figures out algorithm name
--
-- Using it can slowdown compilation
--
-- This combinator has @Algorithm nm alg@ constraint (which currently stands for @TakeUntil ":" nm ~ alg@.
--
-- @runEncoding@ functions are typically not used directly, @runEncodings@ functions defined below or @encodeAll@ 
-- functions are used instead.  
--
-- In the following example (and other examples) we use displ convenience function that provides String display of the encoding.
-- The @"r-ban:111"@ allows only strings with 3 characters satisfying alphanumeric bound of '1'
--
-- >>> fmap displ (_runEncoding encFBan $ toEncoding () "000" :: Either EncodeEx (Enc '["r-ban:111"] () T.Text))
-- Right "Enc '[r-ban:111] () (Text 000)"
--
-- @since 0.3.0.0
_runEncoding :: forall nm f xs conf str alg . (Algorithm nm alg) => Encoding f nm alg conf str -> Enc xs conf str -> f (Enc (nm ': xs) conf str)
_runEncoding = runEncoding' @(AlgNm nm)


-- |
-- HList like construction that defines a list of @Encoding@ elements.
--
-- This type is used by programs using / manipulating encodings.
--
-- Can be easily accessed with 'Data.TypedEncoding.Common.Class.Encode.EncodeAll' constraint using
-- 'Data.TypedEncoding.Common.Class.Encode.EncodeAll.encodings'.  But could also be used by creating
-- @Encodings@ list by hand.
--
-- @since 0.3.0.0
data Encodings f (nms :: [Symbol]) (algs :: [Symbol]) conf str where
    ZeroE :: Encodings f '[] '[] conf str
    ConsE :: Encoding f nm alg conf str -> Encodings f nms algs conf str -> Encodings f (nm ': nms) (alg ': algs) conf str

infixr 5 -:-
(-:-) ::  Encoding f nm alg conf str -> Encodings f nms algs conf str -> Encodings f (nm ': nms) (alg ': algs) conf str
(-:-) = ConsE 

-- |
-- Runs encodings, requires -XTypeApplication annotation specifying the algorithm(s)
--
-- >>> runEncodings' @'["r-ban"] (encFBan -:- ZeroE) . toEncoding () $ "000" :: Either EncodeEx (Enc '["r-ban:111"] () T.Text)
-- Right (UnsafeMkEnc Proxy () "000")
--
-- Polymorphic access to encodings is provided by @EncodeAll@ typeclass so we can simply write:
--
-- >>> runEncodings' @'["r-ban"] encodings . toEncoding () $ "22" :: Either EncodeEx (Enc '["r-ban:111"] () T.Text)
-- Left (EncodeEx "r-ban:111" ("Input list has wrong size expecting 3 but length \"22\" == 2"))
--
-- This library also offers backward compatible equivalents @encodeFAll@ to @runEncodings@ functions 
-- (see "Data.TypedEncoding.Combinators.Encode") which are basically equivalent to something like
-- @
-- runEncoding' encoding
-- @
--
-- >>> encodeFAll' @'["r-ban"] . toEncoding () $ "111" :: Either EncodeEx (Enc '["r-ban:111"] () T.Text)
-- Right (UnsafeMkEnc Proxy () "111")
--
-- >>> fmap displ . encodeFAll' @'["r-ban"] @'["r-ban:111"] @(Either EncodeEx) @() @T.Text . toEncoding () $ "111"
-- Right "Enc '[r-ban:111] () (Text 111)"
--
-- @since 0.3.0.0
runEncodings' :: forall algs nms f c str . (Monad f) => Encodings f nms algs c str -> Enc ('[]::[Symbol]) c str -> f (Enc nms c str)
runEncodings' ZeroE enc0 = pure enc0
runEncodings' (ConsE fn enc) enc0 = 
        let re :: f (Enc _ c str) = runEncodings' enc enc0
        in re >>= runEncoding' fn


-- | At a possibly some compilation cost, have compiler figure out algorithm names.
--
-- >>> _runEncodings encodings . toEncoding () $ ("Hello World") :: Identity (Enc '["enc-B64","enc-B64"] () B.ByteString)
-- Identity (UnsafeMkEnc Proxy () "U0dWc2JHOGdWMjl5YkdRPQ==")
-- 
-- >>> _runEncodings encodings . toEncoding () $ ("22") :: Either EncodeEx (Enc '["r-ban:111"] () T.Text)
-- Left (EncodeEx "r-ban:111" ("Input list has wrong size expecting 3 but length \"22\" == 2"))
--
-- (see also '_runEncoding')
-- @since 0.3.0.0
_runEncodings :: forall nms f c str algs . (Monad f, algs ~ AlgNmMap nms) => Encodings f nms algs c str -> Enc ('[]::[Symbol]) c str -> f (Enc nms c str)
_runEncodings = runEncodings' @(AlgNmMap nms)