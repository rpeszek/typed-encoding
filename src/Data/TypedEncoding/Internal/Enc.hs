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

module Data.TypedEncoding.Internal.Enc where

import           Data.Proxy
import           GHC.TypeLits

import           Data.TypedEncoding.Common.Class.Util
import           Data.TypedEncoding.Common.Types.Common

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XAllowAmbiguousTypes
-- >>> import qualified Data.ByteString as B
-- >>> import qualified Data.Text as T
-- >>> import Data.Functor.Identity
-- >>> import Data.TypedEncoding
-- >>> import Data.TypedEncoding.Instances.Enc.Base64 ()
-- >>> import Data.TypedEncoding.Instances.Restriction.BoundedAlphaNums ()

-- Contains encoded data annotated by 
--
-- * @nms@ list of encodings (encoding stack) typically has kind @[Symbol]@ 
-- * @conf@ that can contain configuration / encoding information such as digest.
-- * @str@ the encoded data
--
--  Example: 
--
-- @
-- Enc ["r-ASCII"] () ByteString
-- @
data Enc nms conf str where
    -- | constructor is to be treated as Unsafe to Encode and Decode instance implementations
    -- particular encoding instances may expose smart constructors for limited data types
    MkEnc :: Proxy nms -> conf -> str -> Enc nms conf str
    deriving (Show, Eq) 

-- |
-- >>> let disptest = MkEnc Proxy () "hello" :: Enc '["TEST"] () T.Text
-- >>> displ disptest
-- "MkEnc '[TEST] () (Text hello)"
instance (SymbolList xs, Show c, Displ str) => Displ ( Enc xs c str) where
    displ (MkEnc p c s) = 
        "MkEnc '" ++ displ (Proxy :: Proxy xs) ++ " " ++ show c ++ " " ++ displ s


toEncoding :: conf -> str -> Enc ('[] :: [Symbol]) conf str
toEncoding = MkEnc Proxy 

fromEncoding :: Enc '[] conf str -> str
fromEncoding = getPayload

getPayload :: Enc enc conf str -> str  
getPayload (MkEnc _ _ str) = str

 

-- |
-- Wraps encoding function that adds encoding to existing stack.
-- Contains type level information about algorithm used. 
--
-- This type is used by programs implementing encoding instance.
-- Such program needs to define a value of this type. 
-- It also implements 'Data.TypedEncoding.Common.Class.Encode.Encode' instance that simply returns that value.
--
-- Programs using encoding can access this type using 'Data.TypedEncoding.Common.Class.Encode.Encode.encoding'
-- but a better (and recommended) approach is to use plural sibling 'Encodings'.
--
-- * @"nm"@ symbol defines the encoding
-- * @"alg"@ symbol defines algorithm 
--
-- These two are related, currently this library only supports 
--
-- * Names containing ":" using format "alg:...", for example name "r-ban:999" has "r-ban" algorithm 
-- * Names without ":" require that @nm ~ alg@
--
-- Future version are likely to relax this, possibly introducing ability do defined more than one algorithm
-- for given encoding. 
--
-- Examples: 
--
-- @
-- Encoding Identity "enc-B64" "enc-B64" () ByteString
-- @
--
-- Represents a /Byte 64/ encoder that can operate on any stack of previous encodings.
-- (encoding name and algorithm name are "enc-B64", there is no  
-- additional configuration @()@ needed and it runs in the @Identity@ Functor.
-- 
-- @
-- Encoding (Either EncodeEx) "r-ban:9" "r-ban" () String
-- @
--
-- encodes a single character @ <= 9'@
--
-- Encoding instance needs to either define a function that return this type or 
-- implement 
-- 'Data.TypedEncoding.Common.Class.Encode.Encode'
-- or (preferably) both.
--
-- Similar boilerplate for Decoding and Validation is specified in separate modules.
data Encoding f (nm :: Symbol) (alg :: Symbol) conf str where
    -- | Consider this constructor as private or use it with care
    --
    -- Using this constructor:
    -- @
    -- MkEncoding :: Proxy nm -> (forall (xs :: [Symbol]) . Enc xs conf str -> f (Enc (nm ': xs) conf str)) -> Encoding f nm (AlgNm nm) conf str
    -- @
    -- 
    -- would make compilation much slower
    UnsafeMkEncoding :: Proxy nm -> (forall (xs :: [Symbol]) . Enc xs conf str -> f (Enc (nm ': xs) conf str)) -> Encoding f nm alg conf str

-- | Type safe smart constructor
--
-- Adding the type family @(AlgNm nm)@ restriction to UnsafeMkEncoding slows down compilation, especially in tests.  
-- Using smart constructor does not have that issue.
--
-- This approach also provides more future flexibility with possibility of future overloads relaxing current 
-- limitations on @alg@ names. 
-- 
-- /Notice underscore @_@ convention, it indicates a use of @Algorithm@ @AlgNm@: compiler figures out @alg@ value. These can be slower to compile when used. /
-- 
-- This particular function appears to not increase compilation time. 
_mkEncoding :: forall f (nm :: Symbol) conf str . (forall (xs :: [Symbol]) . Enc xs conf str -> f (Enc (nm ': xs) conf str)) -> Encoding f nm (AlgNm nm) conf str
_mkEncoding = UnsafeMkEncoding Proxy

runEncoding :: forall alg nm f xs conf str . Encoding f nm alg conf str -> Enc xs conf str -> f (Enc (nm ': xs) conf str)
runEncoding (UnsafeMkEncoding _ fn) = fn

-- | Same as 'runEncoding" but compiler figures out algorithm name
--
-- Using it can slowdown compilation
--
-- This combinator has @Algorithm nm alg@ constraint (which stands for @TakeUntil ":" nm ~ alg@.
-- If rules on @alg@ are relaxed this will just return the /default/ algorithm.
--
-- If that happens @-XTypeApplications@ annotations will be needed and @_@ methods will simply 
-- use default algorithm name.
_runEncoding :: forall nm f xs conf str alg . (Algorithm nm alg) => Encoding f nm alg conf str -> Enc xs conf str -> f (Enc (nm ': xs) conf str)
_runEncoding = runEncoding @(AlgNm nm)

-- |
-- HList like construction that defines a list of @Encoding@ elements.
--
-- This type is used by programs using / manipulating encodings.
--
-- Can be easily accessed with 'Data.TypedEncoding.Common.Class.Encode.EncodeAll' constraint using
-- 'Data.TypedEncoding.Common.Class.Encode.EncodeAll.encodings'.  But could also be used by creating
-- @Encodings@ list by hand.
--
--
data Encodings f (nms :: [Symbol]) (algs :: [Symbol]) conf str where
    -- | constructor is to be treated as Unsafe to Encode and Decode instance implementations
    -- particular encoding instances may expose smart constructors for limited data types
    ZeroE :: Encodings f '[] '[] conf str
    AppendE ::  Encoding f nm alg conf str -> Encodings f nms algs conf str -> Encodings f (nm ': nms) (alg ': algs) conf str

-- |
-- Runs encodings, requires -XTypeApplication annotation specifying the algorithm(s)
--
-- >>> runEncodings @'["r-ban"] encodings . toEncoding () $ ("22") :: Either EncodeEx (Enc '["r-ban:111"] () T.Text)
-- Left (EncodeEx "r-ban:111" ("Input list has wrong size expecting 3 but length \"22\" == 2"))


runEncodings :: forall algs nms f c str . (Monad f) => Encodings f nms algs c str -> Enc ('[]::[Symbol]) c str -> f (Enc nms c str)
runEncodings ZeroE enc0 = pure enc0
runEncodings (AppendE fn enc) enc0 = 
        let re :: f (Enc _ c str) = runEncodings enc enc0
        in re >>= runEncoding fn


-- | At a possibly some compilation cost, have compiler figure out algorithm names.
--
-- >>> _runEncodings encodings . toEncoding () $ ("Hello World") :: Identity (Enc '["enc-B64","enc-B64"] () B.ByteString)
-- Identity (MkEnc Proxy () "U0dWc2JHOGdWMjl5YkdRPQ==")
-- 
-- >>> _runEncodings encodings . toEncoding () $ ("22") :: Either EncodeEx (Enc '["r-ban:111"] () T.Text)
-- Left (EncodeEx "r-ban:111" ("Input list has wrong size expecting 3 but length \"22\" == 2"))
--
-- (see also '_runEncoding')
_runEncodings :: forall nms f c str algs . (Monad f, algs ~ AlgNmMap nms) => Encodings f nms algs c str -> Enc ('[]::[Symbol]) c str -> f (Enc nms c str)
_runEncodings = runEncodings @(AlgNmMap nms)