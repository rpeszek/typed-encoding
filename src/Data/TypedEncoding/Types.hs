{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE RankNTypes #-}

-- |
-- Internal definition of types

module Data.TypedEncoding.Types (
        module Data.TypedEncoding.Types
        -- * Main encoding type and basic combinators.
        , module Data.TypedEncoding.Internal.Enc
        , module Data.TypedEncoding.Types.Decoding
        , module Data.TypedEncoding.Types.Validation
        -- * Untyped version and existentially quantified versions of Enc
        , module Data.TypedEncoding.Types.CheckedEnc
        -- * Not verified encoded data
        , module Data.TypedEncoding.Types.UncheckedEnc
        -- * Commmon types
        , module Data.TypedEncoding.Types.Common
    ) where

import           Data.TypedEncoding.Internal.Enc
import           Data.TypedEncoding.Types.Decoding
import           Data.TypedEncoding.Types.Validation
import           Data.TypedEncoding.Types.CheckedEnc
import           Data.TypedEncoding.Types.UncheckedEnc
import           Data.TypedEncoding.Types.Common

import           Data.Proxy
-- import           Data.Functor.Identity
import           GHC.TypeLits
-- import           Data.TypedEncoding.Class.Util

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XAllowAmbiguousTypes
-- >>> import qualified Data.Text as T


-- | Represents errors in recovery (recreation of encoded types).
data RecreateEx where
    RecreateEx:: (Show e, KnownSymbol x) => Proxy x -> e -> RecreateEx
    RecreateExUnkStep::   (Show e) => e -> RecreateEx

instance Show RecreateEx where
    show (RecreateEx prxy a) = "(RecreateEx \"" ++ symbolVal prxy ++ "\" (" ++ show a ++ "))"
    show (RecreateExUnkStep  a) = "(UnknownDecodeStep (" ++ show a ++ "))"


recreateErrUnknown :: (Show e) => e -> RecreateEx
recreateErrUnknown  = RecreateExUnkStep

-- instance Eq RecreateEx where
--     (RecreateEx prxy1 a1) == RecreateEx prxy2 a2 = (symbolVal prxy1) == (symbolVal prxy2)


-- | Represents errors in encoding
data EncodeEx where
    EncodeEx:: (Show a, KnownSymbol x) => Proxy x -> a -> EncodeEx 

instance Show EncodeEx where
    show (EncodeEx prxy a) = "(EncodeEx \"" ++ symbolVal prxy ++ "\" (" ++ show a ++ "))"

asEncodeEx :: (Show a, KnownSymbol x) => Proxy x -> Either a b -> Either EncodeEx b
asEncodeEx p = either (Left . EncodeEx p) Right 


-- | Useful when manually recreating using recovery
encToRecrEx :: EncodeEx ->  RecreateEx
encToRecrEx (EncodeEx p a) = RecreateEx p a

mergeEncodeEx ::  KnownSymbol x => Proxy x -> EncodeEx -> Maybe EncodeEx -> EncodeEx
mergeEncodeEx _ ex Nothing = ex
mergeEncodeEx p (EncodeEx _ a) (Just (EncodeEx _ b)) = EncodeEx p $ "Errors: " ++ show (a,b)

emptyEncErr ::  KnownSymbol x =>  Proxy x -> EncodeEx 
emptyEncErr p = EncodeEx p ("unexpected" :: String)

-- | Type safety over encodings makes decoding process safe.
-- However failures are still possible due to bugs or unsafe payload modifications.
-- UnexpectedDecodeEx represents such errors.
data UnexpectedDecodeEx where
    UnexpectedDecodeEx :: (Show a, KnownSymbol x) => Proxy x -> a -> UnexpectedDecodeEx

instance Show UnexpectedDecodeEx where
    show (UnexpectedDecodeEx prxy a) = "(UnexpectedDecodeEx \"" ++ symbolVal prxy ++ "\" (" ++ show a ++ "))"


-- * Base combinators that rely on types defined here

mergeErrs :: err -> (err -> Maybe err -> err) -> Either err a -> Either err b -> Either err c
mergeErrs _ fn (Left er1) (Left er2) = Left (fn er1 $ Just er2)
mergeErrs _ fn (Left er1) _ = Left (fn er1 Nothing)
mergeErrs _ fn _  (Left er2) = Left (fn er2 Nothing) 
mergeErrs de fn (Right r) (Right y) = Left de