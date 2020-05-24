{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Defines /Base64/ encoding
--
-- @since 0.1.0.0
module Data.TypedEncoding.Instances.Enc.Base64 where

import           Data.TypedEncoding
import           Data.TypedEncoding.Instances.Support
import           Data.TypedEncoding.Instances.Support.Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE 
import qualified Data.Text.Lazy.Encoding as TEL 

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as BL64



-- $setup
-- >>> :set -XOverloadedStrings -XScopedTypeVariables -XKindSignatures -XMultiParamTypeClasses -XDataKinds -XPolyKinds -XPartialTypeSignatures -XFlexibleInstances -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Instances.Text()
-- >>> import Test.QuickCheck.Instances.ByteString()

-----------------
-- * Conversions
-----------------

-- |
-- @since 0.1.0.0 
acceptLenientS :: Enc ("enc-B64-len" ': ys) c B.ByteString -> Enc ("enc-B64" ': ys) c B.ByteString 
acceptLenientS = withUnsafeCoerce (B64.encode . B64.decodeLenient)

-- |
-- @since 0.1.0.0 
acceptLenientL :: Enc ("enc-B64-len" ': ys) c BL.ByteString -> Enc ("enc-B64" ': ys) c BL.ByteString 
acceptLenientL = withUnsafeCoerce (BL64.encode . BL64.decodeLenient)

-- | allow to treat B64 encodings as ASCII forgetting about B64 encoding
-- 
--
-- >>> let tstB64 = encodeAll . toEncoding () $ "Hello World" :: Enc '["enc-B64"] () B.ByteString
-- >>> displ (flattenAs tstB64 :: Enc '["r-ASCII"] () B.ByteString)
-- "Enc '[r-ASCII] () (ByteString SGVsbG8gV29ybGQ=)"
--
-- @since 0.1.0.0 
instance FlattenAs "r-ASCII" "enc-B64-nontext" where

-- |
-- @since 0.1.0.0 
instance FlattenAs "r-ASCII" "enc-B64" where

-- |
-- This is not precise, actually /Base 64/ uses a subset of ASCII
-- and that would require a new definition @"r-B64"@.
--
-- This instance likely to be changed / corrected in the future if @"r-B64"@ is defined.
--
-- >>> let tstB64 = encodeAll . toEncoding () $ "Hello World" :: Enc '["enc-B64"] () B.ByteString
-- >>> displ (_encodesInto @"r-ASCII" $ tstB64)
-- "Enc '[r-ASCII,enc-B64] () (ByteString SGVsbG8gV29ybGQ=)"
--
-- >>> displ (_encodesInto @"r-UTF8" $ tstB64)
-- "Enc '[r-UTF8,enc-B64] () (ByteString SGVsbG8gV29ybGQ=)"
--
-- @since 0.3.0.0
instance EncodingSuperset "enc-B64" where
    type EncSuperset "enc-B64" = "r-ASCII"

-- * Encoders

-- |
-- @since 0.3.0.0 
instance Applicative f => Encode f "enc-B64" "enc-B64" c B.ByteString where
    encoding = encB64B

-- |
-- @since 0.3.0.0 
encB64B :: Applicative f => Encoding f "enc-B64" "enc-B64" c B.ByteString
encB64B = _implEncodingP B64.encode

-- |
-- @since 0.3.0.0 
instance Applicative f => Encode f "enc-B64" "enc-B64" c BL.ByteString where
    encoding = encB64BL

-- |
-- @since 0.3.0.0 
encB64BL :: Applicative f => Encoding f "enc-B64" "enc-B64" c BL.ByteString
encB64BL = _implEncodingP BL64.encode


-- | This instance will likely be removed in future versions (performance concerns)
--
-- @since 0.3.0.0
instance Applicative f => Encode f "enc-B64" "enc-B64" c T.Text where
    encoding = endB64T

-- | This function will likely be removed in future versions (performance concerns)
endB64T :: Applicative f => Encoding f "enc-B64" "enc-B64" c T.Text
endB64T = _implEncodingP (TE.decodeUtf8 . B64.encode . TE.encodeUtf8)  

-- * Decoders

-- |
-- @since 0.3.0.0 
instance (UnexpectedDecodeErr f, Applicative f) => Decode f "enc-B64" "enc-B64" c B.ByteString where
    decoding = decB64B

-- | Effectful decoding for corruption detection.
-- This protocol is used, for example, in emails. 
-- It is a well known encoding and hackers will have no problem 
-- making undetectable changes, but error handling at this stage
-- could verify that email was corrupted.
--
-- @since 0.3.0.0
decB64B :: (UnexpectedDecodeErr f, Applicative f) => Decoding f "enc-B64" "enc-B64" c B.ByteString
decB64B = _implDecodingF (asUnexpected @"enc-B64" . B64.decode)

-- |
-- @since 0.3.0.0 
instance (UnexpectedDecodeErr f, Applicative f) => Decode f "enc-B64" "enc-B64" c BL.ByteString where
    decoding = decB64BL

-- |
-- @since 0.3.0.0 
decB64BL :: (UnexpectedDecodeErr f, Applicative f) => Decoding f "enc-B64" "enc-B64" c BL.ByteString
decB64BL = _implDecodingF (asUnexpected @"enc-B64" . BL64.decode)


-- Kept for now but performance issues

-- | WARNING (performance)
--
-- @since 0.3.0.0
instance (UnexpectedDecodeErr f, Applicative f) => Decode f "enc-B64" "enc-B64" c T.Text where
    decoding = decB64T

-- |
-- @since 0.3.0.0 
decB64T :: (UnexpectedDecodeErr f, Applicative f) => Decoding f "enc-B64" "enc-B64" c T.Text
decB64T = _implDecodingF (asUnexpected @"enc-B64"  . fmap TE.decodeUtf8 . B64.decode . TE.encodeUtf8) 
{-# WARNING decB64T "This method was not optimized for performance." #-}

-- | WARNING (performance)
--
-- @since 0.3.0.0 
instance (UnexpectedDecodeErr f, Applicative f) => Decode f "enc-B64" "enc-B64" c TL.Text where
    decoding = decB64TL

-- |
-- @since 0.3.0.0 
decB64TL :: (UnexpectedDecodeErr f, Applicative f) => Decoding f "enc-B64" "enc-B64" c TL.Text
decB64TL = _implDecodingF (asUnexpected @"enc-B64"  . fmap TEL.decodeUtf8 . BL64.decode . TEL.encodeUtf8) 
{-# WARNING decB64TL "This method was not optimized for performance." #-}


-- * Validation

-- |
-- @since 0.3.0.0 
instance (RecreateErr f, Applicative f) => Validate f "enc-B64" "enc-B64" c B.ByteString where
    validation = validFromDec decB64B

-- |
-- @since 0.3.0.0 
instance (RecreateErr f, Applicative f) => Validate f "enc-B64" "enc-B64" c BL.ByteString where
    validation = validFromDec decB64BL

-- |
-- @since 0.3.0.0 
instance (RecreateErr f, Applicative f) => Validate f "enc-B64" "enc-B64" c T.Text where
    validation = validFromDec decB64T

-- |
-- @since 0.3.0.0 
instance (RecreateErr f, Applicative f) => Validate f "enc-B64" "enc-B64" c TL.Text where
    validation = validFromDec decB64TL

-- | Lenient decoding does not fail
-- 
-- @since 0.3.0.0 
instance Applicative f => Validate f "enc-B64-len" "enc-B64-len" c B.ByteString where
    validation = mkValidation $ implTranP id

-- | Lenient decoding does not fail
-- 
-- @since 0.3.0.0 
instance Applicative f => Validate f "enc-B64-len" "enc-B64-len" c BL.ByteString where
    validation = mkValidation $ implTranP id



