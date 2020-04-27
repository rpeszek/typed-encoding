{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Data.TypedEncoding.Instances.Enc.Base64 where

import           Data.TypedEncoding
import           Data.TypedEncoding.Instances.Support

import           Data.Proxy
import           Data.Functor.Identity
import           GHC.TypeLits

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE 
import qualified Data.Text.Lazy.Encoding as TEL 

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as BL64

import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Base64.URL.Lazy as BL64URL

-- $setup
-- >>> :set -XScopedTypeVariables -XKindSignatures -XMultiParamTypeClasses -XDataKinds -XPolyKinds -XPartialTypeSignatures -XFlexibleInstances
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Instances.Text()
-- >>> import Test.QuickCheck.Instances.ByteString()


-----------------
-- Conversions --
-----------------

-- | Type-safer version of Byte-string to text conversion that prevent invalid UTF8 bytestrings
-- to be conversted to B64 encoded Text.
byteString2TextS :: Enc ("enc-B64" ': "r-UTF8" ': ys) c B.ByteString -> Enc ("enc-B64" ': ys) c T.Text 
byteString2TextS = withUnsafeCoerce (TE.decodeUtf8)

byteString2TextL :: Enc ("enc-B64" ': "r-UTF8" ': ys) c BL.ByteString -> Enc ("enc-B64" ': ys) c TL.Text 
byteString2TextL = withUnsafeCoerce (TEL.decodeUtf8)

-- | Converts encoded text to ByteString adding "r-UTF8" annotation.
-- The question is why "r-UTF8", not for example, "r-UTF16"?
-- No reason, there maybe a diffrent combinator for that in the future or one that accepts a proxy.
text2ByteStringS :: Enc ("enc-B64" ': ys) c T.Text -> Enc ("enc-B64" ': "r-UTF8" ': ys) c B.ByteString 
text2ByteStringS = withUnsafeCoerce (TE.encodeUtf8)

text2ByteStringL  :: Enc ("enc-B64" ': ys) c TL.Text -> Enc ("enc-B64" ': "r-UTF8" ': ys) c BL.ByteString 
text2ByteStringL  = withUnsafeCoerce (TEL.encodeUtf8)


-- | B64 encoded bytestring can be converted to Text as "enc-B64-nontext" preventing it from 
-- being B64-decoded directly to Text
byteString2TextS' :: Enc ("enc-B64" ': ys) c B.ByteString -> Enc ("enc-B64-nontext" ': ys) c T.Text 
byteString2TextS' = withUnsafeCoerce (TE.decodeUtf8)

byteString2TextL' :: Enc ("enc-B64" ': ys) c BL.ByteString -> Enc ("enc-B64-nontext" ': ys) c TL.Text 
byteString2TextL' = withUnsafeCoerce (TEL.decodeUtf8)

text2ByteStringS' :: Enc ("enc-B64-nontext" ': ys) c T.Text -> Enc ("enc-B64" ': ys) c B.ByteString 
text2ByteStringS' = withUnsafeCoerce (TE.encodeUtf8)

text2ByteStringL'  :: Enc ("enc-B64-nontext" ': ys) c TL.Text -> Enc ("enc-B64" ': ys) c BL.ByteString 
text2ByteStringL'  = withUnsafeCoerce (TEL.encodeUtf8)

acceptLenientS :: Enc ("enc-B64-len" ': ys) c B.ByteString -> Enc ("enc-B64" ': ys) c B.ByteString 
acceptLenientS = withUnsafeCoerce (B64.encode . B64.decodeLenient)

acceptLenientL :: Enc ("enc-B64-len" ': ys) c BL.ByteString -> Enc ("enc-B64" ': ys) c BL.ByteString 
acceptLenientL = withUnsafeCoerce (BL64.encode . BL64.decodeLenient)

-- | allow to treat B64 encodings as ASCII forgetting about B64 encoding
-- 
--
-- >>> let tstB64 = encodeAll . toEncoding () $ "Hello World" :: Enc '["enc-B64"] () B.ByteString
-- >>> displ (flattenAs (Proxy :: Proxy "r-ASCII") tstB64 :: Enc '["r-ASCII"] () B.ByteString)
-- "MkEnc '[r-ASCII] () (ByteString SGVsbG8gV29ybGQ=)"
instance FlattenAs "enc-B64-nontext" "r-ASCII" where
instance FlattenAs "enc-B64" "r-ASCII" where


-----------------
-- Encodings   --
-----------------

-- prxyB64 = Proxy :: Proxy "enc-B64"

instance Applicative f => EncodeF f (Enc xs c B.ByteString) (Enc ("enc-B64" ': xs) c B.ByteString) where
    encodeF = implEncodeP B64.encode 
        
-- | Effectful instance for corruption detection.
-- This protocol is used, for example, in emails. 
-- It is a well known encoding and hackers will have no problem 
-- making undetectable changes, but error handling at this stage
-- could verify that email was corrupted.
instance (UnexpectedDecodeErr f, Applicative f) => DecodeF f (Enc ("enc-B64" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
    decodeF = implDecodeF (asUnexpected_ @"enc-B64" . B64.decode) 

instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c B.ByteString) (Enc ("enc-B64" ': xs) c B.ByteString) where
    checkPrevF = implCheckPrevF (asRecreateErr_ @"enc-B64" .  B64.decode) 

instance Applicative f => RecreateF f (Enc xs c B.ByteString) (Enc ("enc-B64-len" ': xs) c B.ByteString) where
    checkPrevF = implTranP (id) 

instance Applicative f => EncodeF f  (Enc xs c BL.ByteString) (Enc ("enc-B64" ': xs) c BL.ByteString) where
    encodeF = implEncodeP BL64.encode 

instance (UnexpectedDecodeErr f, Applicative f) => DecodeF f  (Enc ("enc-B64" ': xs) c BL.ByteString) (Enc xs c BL.ByteString) where
    decodeF = implDecodeF (asUnexpected_ @"enc-B64"  . BL64.decode)

instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c BL.ByteString) (Enc ("enc-B64" ': xs) c BL.ByteString) where
    checkPrevF = implCheckPrevF (asRecreateErr_ @"enc-B64" .  BL64.decode) 

instance Applicative f => RecreateF f (Enc xs c BL.ByteString) (Enc ("enc-B64-len" ': xs) c BL.ByteString) where
    checkPrevF = implTranP (id) 

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
    decodeF = implDecodeF (asUnexpected_ @"enc-B64"  . fmap TE.decodeUtf8 . B64.decode . TE.encodeUtf8) 

instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c T.Text) (Enc ("enc-B64" ': xs) c T.Text) where
    checkPrevF = implCheckPrevF (asRecreateErr_ @"enc-B64" . fmap TE.decodeUtf8 .  B64.decode . TE.encodeUtf8) 

instance Applicative f => EncodeF f (Enc xs c TL.Text) (Enc ("enc-B64" ': xs) c TL.Text) where
    encodeF = implEncodeP (TEL.decodeUtf8 . BL64.encode . TEL.encodeUtf8)   

instance (UnexpectedDecodeErr f, Applicative f) => DecodeF f (Enc ("enc-B64" ': xs) c TL.Text) (Enc xs c TL.Text) where
    decodeF = implDecodeF (asUnexpected_ @"enc-B64"  . fmap TEL.decodeUtf8 . BL64.decode . TEL.encodeUtf8) 

instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c TL.Text) (Enc ("enc-B64" ': xs) c TL.Text) where
    checkPrevF = implCheckPrevF (asRecreateErr_ @"enc-B64" . fmap TEL.decodeUtf8 .  BL64.decode . TEL.encodeUtf8) 
