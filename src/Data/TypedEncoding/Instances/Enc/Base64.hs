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


import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as BL64
import           Data.TypedEncoding.Instances.Restriction.Base64 ()



-- $setup
-- >>> :set -XOverloadedStrings -XScopedTypeVariables -XKindSignatures -XMultiParamTypeClasses -XDataKinds -XPolyKinds -XPartialTypeSignatures -XFlexibleInstances -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Instances.ByteString()
-- >>> :{  
-- instance Arbitrary (UncheckedEnc () B.ByteString) where 
--      arbitrary = do
--          payload <- frequency [ (5, fmap (getPayload . encodeAll @'["enc-B64"] @(). toEncoding ()) $ arbitrary) 
--                             , (1, arbitrary)]
--          pure $ toUncheckedEnc ["enc-B64"] () payload
-- :}


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

-- |
-- Validated "r-B64" is guaranteed to decode.
-- 
-- Use flattenAs in the other direction.
--  
-- This would not be safe for Text
asEncodingB :: Enc '["r-B64"] c B.ByteString ->  Enc '["enc-B64"] c B.ByteString 
asEncodingB = withUnsafeCoerce id

-- |
-- Validated "r-B64" is guaranteed to decode.  
-- This would not be safe for Text
asEncodingBL :: Enc '["r-B64"] c BL.ByteString ->  Enc '["enc-B64"] c BL.ByteString 
asEncodingBL = withUnsafeCoerce id


-- @"enc-B64-nontext"@ is deprecated, use "r-B64"
--
-- @since 0.1.0.0 
instance FlattenAs "r-ASCII" "enc-B64-nontext" where

-- | allow to treat B64 encodings as ASCII forgetting about B64 encoding.
--
-- Converting to "r-B64" is also an option now.
--
-- >>> let tstB64 = encodeAll . toEncoding () $ "Hello World" :: Enc '["enc-B64"] () B.ByteString
-- >>> displ (flattenAs $ tstB64 :: Enc '["r-ASCII"] () B.ByteString)
-- "Enc '[r-ASCII] () (ByteString SGVsbG8gV29ybGQ=)"
--
--
-- @since 0.1.0.0 
instance FlattenAs "r-ASCII" "enc-B64" where

-- |
-- @since 0.5.1.0 
instance FlattenAs "r-B64" "enc-B64" where

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
    type EncSuperset "enc-B64" = "r-B64"

-- |
-- >>> tstChar8Encodable @ '["enc-B64-len", "enc-B64"]
-- "I am CHAR8 encodable"
instance EncodingSuperset "enc-B64-len" where
    type EncSuperset "enc-B64-len" = "r-B64"

-- * Encoders

-- |
-- @since 0.3.0.0 
instance Applicative f => Encode f "enc-B64" "enc-B64" c B.ByteString where
    encoding = encB64B

-- |
--
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
-- prop> _propSafeDecoding @"enc-B64" @() @B.ByteString encB64B decB64B ()
-- 
-- prop> _propSafeValidatedDecoding @"enc-B64" @() @B.ByteString validation decB64B () . getUncheckedPayload @() @B.ByteString
--
-- @since 0.3.0.0
decB64B :: (UnexpectedDecodeErr f, Applicative f) => Decoding f "enc-B64" "enc-B64" c B.ByteString
decB64B = _implDecodingF (asUnexpected @"enc-B64" . B64.decode)

-- |
-- @since 0.3.0.0 
instance (UnexpectedDecodeErr f, Applicative f) => Decode f "enc-B64" "enc-B64" c BL.ByteString where
    decoding = decB64BL

-- |
-- prop> _propSafeDecoding @"enc-B64" @() @BL.ByteString encB64BL decB64BL
--
-- @since 0.3.0.0 
decB64BL :: (UnexpectedDecodeErr f, Applicative f) => Decoding f "enc-B64" "enc-B64" c BL.ByteString
decB64BL = _implDecodingF (asUnexpected @"enc-B64" . BL64.decode)


-- * Validation

-- |
-- @since 0.3.0.0 
instance (RecreateErr f, Applicative f) => Validate f "enc-B64" "enc-B64" c B.ByteString where
    validation = validFromDec decB64B

-- |
-- @since 0.3.0.0 
instance (RecreateErr f, Applicative f) => Validate f "enc-B64" "enc-B64" c BL.ByteString where
    validation = validFromDec decB64BL


-- | Lenient decoding does not fail
-- 
-- @since 0.3.0.0 
instance Applicative f => Validate f "enc-B64-len" "enc-B64-len" c B.ByteString where
    validation = _mkValidation $ implTranP id

-- | Lenient decoding does not fail
-- 
-- @since 0.3.0.0 
instance Applicative f => Validate f "enc-B64-len" "enc-B64-len" c BL.ByteString where
    validation = _mkValidation $ implTranP id



