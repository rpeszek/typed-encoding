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

import           Data.TypedEncoding.Class.Util
import           Data.TypedEncoding.Types.Common

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XAllowAmbiguousTypes
-- >>> import qualified Data.Text as T

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
-- >>> let disptest = unsafeSetPayload () "hello" :: Enc '["TEST"] () T.Text
-- >>> displ disptest
-- "MkEnc '[TEST] () (Text hello)"
instance (SymbolList xs, Show c, Displ str) => Displ ( Enc xs c str) where
    displ (MkEnc p c s) = 
        "MkEnc '" ++ displ (Proxy :: Proxy xs) ++ " " ++ show c ++ " " ++ displ s


toEncoding :: conf -> str -> Enc '[] conf str
toEncoding = MkEnc Proxy 

fromEncoding :: Enc '[] conf str -> str
fromEncoding = getPayload


getPayload :: Enc enc conf str -> str  
getPayload (MkEnc _ _ str) = str

unsafeSetPayload :: conf -> str -> Enc enc conf str 
unsafeSetPayload  = MkEnc Proxy 

withUnsafeCoerce ::  (s1 -> s2) -> Enc e1 c s1 -> Enc e2 c s2
withUnsafeCoerce f (MkEnc _ conf str)  = MkEnc Proxy conf (f str)

withUnsafeCoerceF :: forall e1 e2 f c s1 s2 . Functor f => (s1 -> f s2) -> Enc e1 c s1 -> f (Enc e2 c s2)
withUnsafeCoerceF f (MkEnc _ conf str)  = MkEnc Proxy conf <$> f str

unsafeChangePayload ::  (s1 -> s2) -> Enc e c s1 -> Enc e c s2
unsafeChangePayload f (MkEnc p conf str)  = MkEnc p conf (f str) 

getTransformF :: forall e1 e2 f c s1 s2 . Functor f => (Enc e1 c s1 -> f (Enc e2 c s2)) -> c -> s1 -> f s2
getTransformF fn c str = getPayload <$> fn (unsafeSetPayload c str)
 

-- |
-- Wraps encoding function that adds encoding to existing stack.
-- Contains type level information about algorithm used. 
--
-- Example:
--
-- @
-- Encoding Identity "enc-B64" "enc-B64" () ByteString
-- @
--
-- Contains function that applies base 64 encoding (encoding name and algorithm name are "enc-B64") 
-- to ByteString with no additional configuration @()@ and runs in @Identity@ Functor.
-- 
-- @
-- Encoding (Either EncodeEx) "r-ban:9" "r-ban" () String
-- @
-- encodes a single character <= @'Z'@
--
-- Encoding instance needs to either define a function that return this type or 
-- implement 
-- 'Data.TypedEncoding.Class.Encode.Encode'
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
-- adding the type family @(AlgNm nm)@ restriction to UnsafeMkEncoding slows down compilation, especially in tests.      
mkEncoding :: forall f (nm :: Symbol) conf str . (forall (xs :: [Symbol]) . Enc xs conf str -> f (Enc (nm ': xs) conf str)) -> Encoding f nm (AlgNm nm) conf str
mkEncoding = UnsafeMkEncoding Proxy

runEncoding :: forall alg nm f xs conf str . Encoding f nm alg conf str -> Enc xs conf str -> f (Enc (nm ': xs) conf str)
runEncoding (UnsafeMkEncoding _ fn) = fn

-- | Same as 'runEncoding" but compiler figures out algorithm name
--
-- Using it can slowdown compilation
_runEncoding :: forall nm f xs conf str alg . (AlgNm nm ~ alg) => Encoding f nm alg conf str -> Enc xs conf str -> f (Enc (nm ': xs) conf str)
_runEncoding = runEncoding @(AlgNm nm)

-- |
-- Wraps a list of @Encoding@ elements.
--
-- Similarly to 'Encoding' it can be used with a typeclass
-- 'Data.TypedEncoding.Class.Encode.EncodeAll'
data Encodings f (nms :: [Symbol]) (algs :: [Symbol]) conf str where
    -- | constructor is to be treated as Unsafe to Encode and Decode instance implementations
    -- particular encoding instances may expose smart constructors for limited data types
    ZeroE :: Encodings f '[] '[] conf str
    AppendE ::  Encoding f nm alg conf str -> Encodings f nms algs conf str -> Encodings f (nm ': nms) (alg ': algs) conf str

runEncodings :: forall algs nms f c str . (Monad f) => Encodings f nms algs c str -> Enc ('[]::[Symbol]) c str -> f (Enc nms c str)
runEncodings ZeroE enc0 = pure enc0
runEncodings (AppendE fn enc) enc0 = 
        let re :: f (Enc _ c str) = runEncodings enc enc0
        in re >>= runEncoding fn


-- | At possibly big compilation cost, have compiler figure out algorithm names.
_runEncodings :: forall nms f c str algs . (Monad f, algs ~ AlgNmMap nms) => Encodings f nms algs c str -> Enc ('[]::[Symbol]) c str -> f (Enc nms c str)
_runEncodings = runEncodings @(AlgNmMap nms)