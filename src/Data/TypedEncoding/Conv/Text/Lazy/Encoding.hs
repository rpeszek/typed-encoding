

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Lazy version of "Data.TypedEncoding.Conv.Text.Encoding"
-- @since 0.2.2.0
module Data.TypedEncoding.Conv.Text.Lazy.Encoding where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TEL    

import           Data.TypedEncoding.Instances.Support
import qualified Data.TypedEncoding.Common.Util.TypeLits as Knds
import           Data.TypedEncoding.Instances.Restriction.UTF8 ()
import           Data.TypedEncoding.Instances.Restriction.ASCII ()
import           Data.TypedEncoding.Unsafe (withUnsafe)


-- | Lazy version of 'Data.TypedEncoding.Conv.Text.Encoding.decodeUtf8'
decodeUtf8 :: forall xs c t y ys encs. (
          Knds.UnSnoc xs ~ '(,) ys y
         , Superset "r-UTF8" y 
         , encs ~ RemoveRs ys
         , AllEncodeInto "r-UTF8" encs
        ) => Enc xs c BL.ByteString -> Enc xs c TL.Text 
decodeUtf8 = withUnsafe (fmap TEL.decodeUtf8)

-- | simplified version of @decodeUtf8@ that works on single /r-/ encodings
-- @since 0.5.2.0
decodeUtf8_1 :: (
         Superset "r-UTF8" y 
         ) => Enc '[y] c  BL.ByteString -> Enc '[y] c TL.Text 
decodeUtf8_1 = decodeUtf8

-- | Lazy version of 'Data.TypedEncoding.Conv.Text.Encoding.encodeUtf8'
encodeUtf8 :: forall xs c t y ys encs. (
          Knds.UnSnoc xs ~ '(,) ys y
         , Superset "r-UTF8" y 
         , encs ~ RemoveRs ys
         , AllEncodeInto "r-UTF8" encs
        ) => Enc xs c TL.Text -> Enc xs c BL.ByteString 
encodeUtf8 = withUnsafe (fmap TEL.encodeUtf8)

-- | simplified version of @decodeUtf8@ that works on single /r-/ encodings
-- @since 0.5.2.0
encodeUtf8_1 :: (
         Superset "r-UTF8" y 
         ) => Enc '[y] c  TL.Text -> Enc '[y] c BL.ByteString 
encodeUtf8_1 = encodeUtf8