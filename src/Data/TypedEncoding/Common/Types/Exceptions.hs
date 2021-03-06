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
-- Exception types used in /typed-encoding/
--
-- This module is re-exported in "Data.TypedEncoding" and it is best not to import it directly.

module Data.TypedEncoding.Common.Types.Exceptions where


import           Data.Proxy
import           GHC.TypeLits


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
-- @since 0.1.0.0 
data EncodeEx where
    EncodeEx:: (Show a, KnownSymbol x) => Proxy x -> a -> EncodeEx 

instance Show EncodeEx where
    show (EncodeEx prxy a) = "(EncodeEx \"" ++ symbolVal prxy ++ "\" (" ++ show a ++ "))"

-- |
-- @since 0.2.2.0
asEncodeEx :: (Show a, KnownSymbol x) => Proxy x -> Either a b -> Either EncodeEx b
asEncodeEx p = either (Left . EncodeEx p) Right 


-- | Useful when manually recreating using recovery
-- @since 0.2.2.0
encToRecrEx :: EncodeEx ->  RecreateEx
encToRecrEx (EncodeEx p a) = RecreateEx p a

-- |
-- @since 0.2.1.0
mergeEncodeEx ::  KnownSymbol x => Proxy x -> EncodeEx -> Maybe EncodeEx -> EncodeEx
mergeEncodeEx _ ex Nothing = ex
mergeEncodeEx p (EncodeEx _ a) (Just (EncodeEx _ b)) = EncodeEx p $ "Errors: " ++ show (a,b)

-- |
-- @since 0.2.1.0
emptyEncErr ::  KnownSymbol x =>  Proxy x -> EncodeEx 
emptyEncErr p = EncodeEx p ("unexpected" :: String)

-- | Type safety over encodings makes decoding process safe.
-- However failures are still possible due to bugs or unsafe payload modifications.
-- UnexpectedDecodeEx represents such errors.
--
-- @since 0.1.0.0 
data UnexpectedDecodeEx where
    UnexpectedDecodeEx :: (Show a, KnownSymbol x) => Proxy x -> a -> UnexpectedDecodeEx

instance Show UnexpectedDecodeEx where
    show (UnexpectedDecodeEx prxy a) = "(UnexpectedDecodeEx \"" ++ symbolVal prxy ++ "\" (" ++ show a ++ "))"


-- * Base combinators that rely on types defined here

-- |
-- @since 0.2.1.0
mergeErrs :: err -> (err -> Maybe err -> err) -> Either err a -> Either err b -> Either err c
mergeErrs _ fn (Left er1) (Left er2) = Left (fn er1 $ Just er2)
mergeErrs _ fn (Left er1) _ = Left (fn er1 Nothing)
mergeErrs _ fn _  (Left er2) = Left (fn er2 Nothing) 
mergeErrs de fn (Right r) (Right y) = Left de