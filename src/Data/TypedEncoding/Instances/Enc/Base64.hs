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

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE 
import qualified Data.Text.Lazy.Encoding as TEL 

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as BL64

-- import qualified Data.ByteString.Base64.URL as B64URL
-- import qualified Data.ByteString.Base64.URL.Lazy as BL64URL

-- $setup
-- >>> :set -XScopedTypeVariables -XKindSignatures -XMultiParamTypeClasses -XDataKinds -XPolyKinds -XPartialTypeSignatures -XFlexibleInstances
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Instances.Text()
-- >>> import Test.QuickCheck.Instances.ByteString()

-----------------
-- Conversions --
-----------------

-- | 
-- DEPRECATED use 'Data.TypedEncoding.Conv.Text.Encoding.decodeUtf8'
-- and 'Data.TypedEncoding.Conv.Text.utf8Demote'
-- 
-- Will be removed in 0.3.x.x
--
-- See warning in 'Data.TypedEncoding.Instances.Restriction.ASCII.byteString2TextS'
byteString2TextS :: Enc ("enc-B64" ': "r-UTF8" ': ys) c B.ByteString -> Enc ("enc-B64" ': ys) c T.Text 
byteString2TextS = withUnsafeCoerce TE.decodeUtf8

-- | 
-- DEPRECATED use 'Data.TypedEncoding.Conv.Text.Lazy.Encoding.decodeUtf8'
-- and 'Data.TypedEncoding.Conv.Text.utf8Demote'
--
-- Will be removed in 0.3.x.x
--
-- See warning in 'Data.TypedEncoding.Instances.Restriction.ASCII.byteString2TextS'
byteString2TextL :: Enc ("enc-B64" ': "r-UTF8" ': ys) c BL.ByteString -> Enc ("enc-B64" ': ys) c TL.Text 
byteString2TextL = withUnsafeCoerce TEL.decodeUtf8

-- DEPRECATED use 'Data.TypedEncoding.Conv.Text.Encoding.encodeUtf8'
-- and 'Data.TypedEncoding.Conv.Text.utf8Promote'
-- 
-- Will be removed in 0.3.x.x
text2ByteStringS :: Enc ("enc-B64" ': ys) c T.Text -> Enc ("enc-B64" ': "r-UTF8" ': ys) c B.ByteString 
text2ByteStringS = withUnsafeCoerce TE.encodeUtf8

-- DEPRECATED use 'Data.TypedEncoding.Conv.Text.Lazy.Encoding.encodeUtf8'
-- and 'Data.TypedEncoding.Conv.Text.utf8Promote'
-- 
-- Will be removed in 0.3.x.x
text2ByteStringL  :: Enc ("enc-B64" ': ys) c TL.Text -> Enc ("enc-B64" ': "r-UTF8" ': ys) c BL.ByteString 
text2ByteStringL  = withUnsafeCoerce TEL.encodeUtf8


-- | B64 encoded bytestring can be converted to Text as "enc-B64-nontext" preventing it from 
-- being B64-decoded directly to Text
byteString2TextS' :: Enc ("enc-B64" ': ys) c B.ByteString -> Enc ("enc-B64-nontext" ': ys) c T.Text 
byteString2TextS' = withUnsafeCoerce TE.decodeUtf8

-- DEPRECATED 
byteString2TextL' :: Enc ("enc-B64" ': ys) c BL.ByteString -> Enc ("enc-B64-nontext" ': ys) c TL.Text 
byteString2TextL' = withUnsafeCoerce TEL.decodeUtf8

-- DEPRECATED 
text2ByteStringS' :: Enc ("enc-B64-nontext" ': ys) c T.Text -> Enc ("enc-B64" ': ys) c B.ByteString 
text2ByteStringS' = withUnsafeCoerce TE.encodeUtf8

-- DEPRECATED 
text2ByteStringL'  :: Enc ("enc-B64-nontext" ': ys) c TL.Text -> Enc ("enc-B64" ': ys) c BL.ByteString 
text2ByteStringL'  = withUnsafeCoerce TEL.encodeUtf8



acceptLenientS :: Enc ("enc-B64-len" ': ys) c B.ByteString -> Enc ("enc-B64" ': ys) c B.ByteString 
acceptLenientS = withUnsafeCoerce (B64.encode . B64.decodeLenient)

acceptLenientL :: Enc ("enc-B64-len" ': ys) c BL.ByteString -> Enc ("enc-B64" ': ys) c BL.ByteString 
acceptLenientL = withUnsafeCoerce (BL64.encode . BL64.decodeLenient)

-- | allow to treat B64 encodings as ASCII forgetting about B64 encoding
-- 
--
-- >>> let tstB64 = encodeAll . toEncoding () $ "Hello World" :: Enc '["enc-B64"] () B.ByteString
-- >>> displ (flattenAs tstB64 :: Enc '["r-ASCII"] () B.ByteString)
-- "MkEnc '[r-ASCII] () (ByteString SGVsbG8gV29ybGQ=)"
instance FlattenAs "r-ASCII" "enc-B64-nontext" where
instance FlattenAs "r-ASCII" "enc-B64" where

-- DEPRECATED will be removed
--
-- dangerous, with new approach.
-- Supersets are for "r-" types only
instance Superset "r-ASCII" "enc-B64-nontext" where
instance Superset "r-ASCII" "enc-B64" where

-----------------
-- Encodings   --
-----------------


instance Applicative f => EncodeF f (Enc xs c B.ByteString) (Enc ("enc-B64" ': xs) c B.ByteString) where
    encodeF = implEncodeP B64.encode 
        
-- | Effectful instance for corruption detection.
-- This protocol is used, for example, in emails. 
-- It is a well known encoding and hackers will have no problem 
-- making undetectable changes, but error handling at this stage
-- could verify that email was corrupted.
instance (UnexpectedDecodeErr f, Applicative f) => DecodeF f (Enc ("enc-B64" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
    decodeF = implDecodeF (asUnexpected @"enc-B64" . B64.decode) 

instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c B.ByteString) (Enc ("enc-B64" ': xs) c B.ByteString) where
    checkPrevF = implCheckPrevF (asRecreateErr @"enc-B64" .  B64.decode) 

instance Applicative f => RecreateF f (Enc xs c B.ByteString) (Enc ("enc-B64-len" ': xs) c B.ByteString) where
    checkPrevF = implTranP id

instance Applicative f => EncodeF f  (Enc xs c BL.ByteString) (Enc ("enc-B64" ': xs) c BL.ByteString) where
    encodeF = implEncodeP BL64.encode 

instance (UnexpectedDecodeErr f, Applicative f) => DecodeF f  (Enc ("enc-B64" ': xs) c BL.ByteString) (Enc xs c BL.ByteString) where
    decodeF = implDecodeF (asUnexpected @"enc-B64"  . BL64.decode)

instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c BL.ByteString) (Enc ("enc-B64" ': xs) c BL.ByteString) where
    checkPrevF = implCheckPrevF (asRecreateErr @"enc-B64" .  BL64.decode) 

instance Applicative f => RecreateF f (Enc xs c BL.ByteString) (Enc ("enc-B64-len" ': xs) c BL.ByteString) where
    checkPrevF = implTranP id

-- B64URL currently not supported
-- instance Applicative f => EncodeF f (Enc xs c B.ByteString) (Enc ("enc-B64URL" ': xs) c B.ByteString) where
--     encodeF = implEncodeP B64URL.encode 
-- instance DecodeF (Either String) (Enc ("enc-B64URL" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
--     decodeF = implDecodeF B64URL.decode 
-- instance DecodeF Identity (Enc ("enc-B64URL" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
--     decodeF = implTranP B64URL.decodeLenient 

-- instance Applicative f => EncodeF f (Enc xs c BL.ByteString) (Enc ("enc-B64URL" ': xs) c BL.ByteString) where
--     encodeF = implEncodeP BL64URL.encode 
-- instance DecodeF (Either String) (Enc ("enc-B64URL" ': xs) c BL.ByteString) (Enc xs c BL.ByteString) where
--     decodeF = implDecodeF BL64URL.decode 
-- instance DecodeF Identity (Enc ("enc-B64URL" ': xs) c BL.ByteString) (Enc xs c BL.ByteString) where
--     decodeF = implTranP BL64URL.decodeLenient 

instance Applicative f => EncodeF f (Enc xs c T.Text) (Enc ("enc-B64" ': xs) c T.Text) where
    encodeF = implEncodeP (TE.decodeUtf8 . B64.encode . TE.encodeUtf8)   

instance (UnexpectedDecodeErr f, Applicative f) => DecodeF f (Enc ("enc-B64" ': xs) c T.Text) (Enc xs c T.Text) where
    decodeF = implDecodeF (asUnexpected @"enc-B64"  . fmap TE.decodeUtf8 . B64.decode . TE.encodeUtf8) 

instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c T.Text) (Enc ("enc-B64" ': xs) c T.Text) where
    checkPrevF = implCheckPrevF (asRecreateErr @"enc-B64" . fmap TE.decodeUtf8 .  B64.decode . TE.encodeUtf8) 

instance Applicative f => EncodeF f (Enc xs c TL.Text) (Enc ("enc-B64" ': xs) c TL.Text) where
    encodeF = implEncodeP (TEL.decodeUtf8 . BL64.encode . TEL.encodeUtf8)   

instance (UnexpectedDecodeErr f, Applicative f) => DecodeF f (Enc ("enc-B64" ': xs) c TL.Text) (Enc xs c TL.Text) where
    decodeF = implDecodeF (asUnexpected @"enc-B64"  . fmap TEL.decodeUtf8 . BL64.decode . TEL.encodeUtf8) 

instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c TL.Text) (Enc ("enc-B64" ': xs) c TL.Text) where
    checkPrevF = implCheckPrevF (asRecreateErr @"enc-B64" . fmap TEL.decodeUtf8 .  BL64.decode . TEL.encodeUtf8) 
