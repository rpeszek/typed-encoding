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

acceptLenientS :: Enc ("enc-B64-len" ': ys) c B.ByteString -> Enc ("enc-B64" ': ys) c B.ByteString 
acceptLenientS = withUnsafeCoerce (B64.encode . B64.decodeLenient)

acceptLenientL :: Enc ("enc-B64-len" ': ys) c BL.ByteString -> Enc ("enc-B64" ': ys) c BL.ByteString 
acceptLenientL = withUnsafeCoerce (BL64.encode . BL64.decodeLenient)

-- | allow to treat B64 encodings as ASCII forgetting about B64 encoding
-- 
--
-- >>> let tstB64 = encAll . toEncoding () $ "Hello World" :: Enc '["enc-B64"] () B.ByteString
-- >>> displ (flattenAs tstB64 :: Enc '["r-ASCII"] () B.ByteString)
-- "MkEnc '[r-ASCII] () (ByteString SGVsbG8gV29ybGQ=)"
instance FlattenAs "r-ASCII" "enc-B64-nontext" where
instance FlattenAs "r-ASCII" "enc-B64" where


-----------------
-- Encodings   --
-----------------
instance Applicative f => Encode f "enc-B64" "enc-B64" c B.ByteString where
    encoding = encB64B

encB64B :: Applicative f => Encoding f "enc-B64" "enc-B64" c B.ByteString
encB64B = mkEncoding (implEncodeP B64.encode)

instance Applicative f => Encode f "enc-B64" "enc-B64" c BL.ByteString where
    encoding = encB64BL

encB64BL :: Applicative f => Encoding f "enc-B64" "enc-B64" c BL.ByteString
encB64BL = mkEncoding (implEncodeP BL64.encode)

instance Applicative f => Encode f "enc-B64" "enc-B64" c T.Text where
    encoding = endB64T

endB64T :: Applicative f => Encoding f "enc-B64" "enc-B64" c T.Text
endB64T = mkEncoding $ implEncodeP (TE.decodeUtf8 . B64.encode . TE.encodeUtf8)  


---------------------------
-- TODO OLD

-- instance WhichEncoder (Either EncodeEx) xs grps c B.ByteString => WhichEncoder (Either EncodeEx) ("enc-B64" ': xs) ("enc-B64" ': grps) c B.ByteString where
--     encoder = encodeFEncoder @(Either EncodeEx) @"enc-B64" @"enc-B64"

-- instance Applicative f => EncodeF f (Enc xs c B.ByteString) (Enc ("enc-B64" ': xs) c B.ByteString) where
--     encodeF = implEncodeP B64.encode 
        
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

-- instance Applicative f => EncodeF f  (Enc xs c BL.ByteString) (Enc ("enc-B64" ': xs) c BL.ByteString) where
--     encodeF = implEncodeP BL64.encode 

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

-- instance Applicative f => EncodeF f (Enc xs c T.Text) (Enc ("enc-B64" ': xs) c T.Text) where
--     encodeF = implEncodeP (TE.decodeUtf8 . B64.encode . TE.encodeUtf8)   

instance (UnexpectedDecodeErr f, Applicative f) => DecodeF f (Enc ("enc-B64" ': xs) c T.Text) (Enc xs c T.Text) where
    decodeF = implDecodeF (asUnexpected @"enc-B64"  . fmap TE.decodeUtf8 . B64.decode . TE.encodeUtf8) 

instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c T.Text) (Enc ("enc-B64" ': xs) c T.Text) where
    checkPrevF = implCheckPrevF (asRecreateErr @"enc-B64" . fmap TE.decodeUtf8 .  B64.decode . TE.encodeUtf8) 

-- instance Applicative f => EncodeF f (Enc xs c TL.Text) (Enc ("enc-B64" ': xs) c TL.Text) where
--     encodeF = implEncodeP (TEL.decodeUtf8 . BL64.encode . TEL.encodeUtf8)   

instance (UnexpectedDecodeErr f, Applicative f) => DecodeF f (Enc ("enc-B64" ': xs) c TL.Text) (Enc xs c TL.Text) where
    decodeF = implDecodeF (asUnexpected @"enc-B64"  . fmap TEL.decodeUtf8 . BL64.decode . TEL.encodeUtf8) 

instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c TL.Text) (Enc ("enc-B64" ': xs) c TL.Text) where
    checkPrevF = implCheckPrevF (asRecreateErr @"enc-B64" . fmap TEL.decodeUtf8 .  BL64.decode . TEL.encodeUtf8) 
