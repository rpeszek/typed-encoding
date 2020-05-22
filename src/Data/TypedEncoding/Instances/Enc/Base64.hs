{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

-- | Defines /Base64/ encoding
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
-- >>> :set -XScopedTypeVariables -XKindSignatures -XMultiParamTypeClasses -XDataKinds -XPolyKinds -XPartialTypeSignatures -XFlexibleInstances
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Instances.Text()
-- >>> import Test.QuickCheck.Instances.ByteString()

-----------------
-- * Conversions
-----------------

acceptLenientS :: Enc ("enc-B64-len" ': ys) c B.ByteString -> Enc ("enc-B64" ': ys) c B.ByteString 
acceptLenientS = withUnsafeCoerce (B64.encode . B64.decodeLenient)

acceptLenientL :: Enc ("enc-B64-len" ': ys) c BL.ByteString -> Enc ("enc-B64" ': ys) c BL.ByteString 
acceptLenientL = withUnsafeCoerce (BL64.encode . BL64.decodeLenient)

-- | allow to treat B64 encodings as ASCII forgetting about B64 encoding
-- 
--
-- >>> let tstB64 = encodeAll . toEncoding () $ "Hello World" :: Enc '["enc-B64"] () B.ByteString
-- >>> displ (flattenAs tstB64 :: Enc '["r-ASCII"] () B.ByteString)
-- "Enc '[r-ASCII] () (ByteString SGVsbG8gV29ybGQ=)"
instance FlattenAs "r-ASCII" "enc-B64-nontext" where
instance FlattenAs "r-ASCII" "enc-B64" where


-- * Encoders

instance Applicative f => Encode f "enc-B64" "enc-B64" c B.ByteString where
    encoding = encB64B

encB64B :: Applicative f => Encoding f "enc-B64" "enc-B64" c B.ByteString
encB64B = _implEncodingP B64.encode

instance Applicative f => Encode f "enc-B64" "enc-B64" c BL.ByteString where
    encoding = encB64BL

encB64BL :: Applicative f => Encoding f "enc-B64" "enc-B64" c BL.ByteString
encB64BL = _implEncodingP BL64.encode


-- | This instance will likely be removed in future versions (performance concerns)
instance Applicative f => Encode f "enc-B64" "enc-B64" c T.Text where
    encoding = endB64T

-- | This function will likely be removed in future versions (performance concerns)
endB64T :: Applicative f => Encoding f "enc-B64" "enc-B64" c T.Text
endB64T = _implEncodingP (TE.decodeUtf8 . B64.encode . TE.encodeUtf8)  

-- * Decoders

instance (UnexpectedDecodeErr f, Applicative f) => Decode f "enc-B64" "enc-B64" c B.ByteString where
    decoding = decB64B

-- | Effectful decoding for corruption detection.
-- This protocol is used, for example, in emails. 
-- It is a well known encoding and hackers will have no problem 
-- making undetectable changes, but error handling at this stage
-- could verify that email was corrupted.
decB64B :: (UnexpectedDecodeErr f, Applicative f) => Decoding f "enc-B64" "enc-B64" c B.ByteString
decB64B = _implDecodingF (asUnexpected @"enc-B64" . B64.decode)

instance (UnexpectedDecodeErr f, Applicative f) => Decode f "enc-B64" "enc-B64" c BL.ByteString where
    decoding = decB64BL

decB64BL :: (UnexpectedDecodeErr f, Applicative f) => Decoding f "enc-B64" "enc-B64" c BL.ByteString
decB64BL = _implDecodingF (asUnexpected @"enc-B64" . BL64.decode)


-- Kept for now but performance issues

-- | DEPRECATED (performance)
instance (UnexpectedDecodeErr f, Applicative f) => Decode f "enc-B64" "enc-B64" c T.Text where
    decoding = decB64T

decB64T :: (UnexpectedDecodeErr f, Applicative f) => Decoding f "enc-B64" "enc-B64" c T.Text
decB64T = _implDecodingF (asUnexpected @"enc-B64"  . fmap TE.decodeUtf8 . B64.decode . TE.encodeUtf8) 
{-# DEPRECATED decB64T "Performance, manually control conversion to and from ByteString" #-}

-- | DEPRECATED (performance)
instance (UnexpectedDecodeErr f, Applicative f) => Decode f "enc-B64" "enc-B64" c TL.Text where
    decoding = decB64TL

decB64TL :: (UnexpectedDecodeErr f, Applicative f) => Decoding f "enc-B64" "enc-B64" c TL.Text
decB64TL = _implDecodingF (asUnexpected @"enc-B64"  . fmap TEL.decodeUtf8 . BL64.decode . TEL.encodeUtf8) 
{-# DEPRECATED decB64TL "Performance, manually control conversion to and from ByteString" #-}


-- * Validation

instance (RecreateErr f, Applicative f) => Validate f "enc-B64" "enc-B64" c B.ByteString where
    validation = validFromDec decB64B

instance (RecreateErr f, Applicative f) => Validate f "enc-B64" "enc-B64" c BL.ByteString where
    validation = validFromDec decB64BL

instance (RecreateErr f, Applicative f) => Validate f "enc-B64" "enc-B64" c T.Text where
    validation = validFromDec decB64T

instance (RecreateErr f, Applicative f) => Validate f "enc-B64" "enc-B64" c TL.Text where
    validation = validFromDec decB64TL

-- | Lenient decoding does not fail
instance Applicative f => Validate f "enc-B64-len" "enc-B64-len" c B.ByteString where
    validation = mkValidation $ implTranP id

-- | Lenient decoding does not fail
instance Applicative f => Validate f "enc-B64-len" "enc-B64-len" c BL.ByteString where
    validation = mkValidation $ implTranP id



