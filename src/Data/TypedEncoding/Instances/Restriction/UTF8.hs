{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

-- | 'UTF-8' encoding
module Data.TypedEncoding.Instances.Restriction.UTF8 where

import           Data.TypedEncoding.Instances.Support

import           Data.Proxy

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE 
import qualified Data.Text.Lazy.Encoding as TEL 
import           Data.Either


-- $setup
-- >>> :set -XScopedTypeVariables -XKindSignatures -XMultiParamTypeClasses -XDataKinds -XPolyKinds -XPartialTypeSignatures -XFlexibleInstances -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Instances.Text()
-- >>> import Test.QuickCheck.Instances.ByteString()
-- >>> import Data.TypedEncoding
-- >>> import Data.TypedEncoding.Internal.Util (proxiedId)
-- >>> let emptyUTF8B = unsafeSetPayload () "" ::  Enc '["r-UTF8"] () B.ByteString 
-- >>> :{  
-- instance Arbitrary (Enc '["r-UTF8"] () B.ByteString) where 
--      arbitrary =  fmap (fromRight emptyUTF8B) 
--                   . flip suchThat isRight 
--                   . fmap (encodeFAll @'["r-UTF8"] @(Either EncodeEx) @(). toEncoding ()) $ arbitrary 
-- :}



-----------------
-- Encodings  --
-----------------

prxyUtf8 = Proxy :: Proxy "r-UTF8"

-- TODO these may need rethinking (performance)

-- | UTF8 encodings are defined for ByteString only as that would not make much sense for Text
--
-- >>> encodeFAll . toEncoding () $ "\xc3\xb1" :: Either EncodeEx (Enc '["r-UTF8"] () B.ByteString)
-- Right (UnsafeMkEnc Proxy () "\195\177")
--
-- >>> encodeFAll . toEncoding () $ "\xc3\x28" :: Either EncodeEx (Enc '["r-UTF8"] () B.ByteString)
-- Left (EncodeEx "r-UTF8" (Cannot decode byte '\xc3': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream))
--
-- Following test uses 'verEncoding' helper that checks that bytes are encoded as Right iff they are valid UTF8 bytes
--
-- prop> \(b :: B.ByteString) -> verEncoding b (fmap (fromEncoding . decodeAll . proxiedId (Proxy :: Proxy (Enc '["r-UTF8"] _ _))) . (encodeFAll :: _ -> Either EncodeEx _). toEncoding () $ b)
instance Encode (Either EncodeEx) "r-UTF8" "r-UTF8" c B.ByteString where
    encoding = encUTF8B

instance Encode (Either EncodeEx) "r-UTF8" "r-UTF8" c BL.ByteString where
    encoding = encUTF8BL :: Encoding (Either EncodeEx) "r-UTF8" "r-UTF8" c BL.ByteString

encUTF8B :: Encoding (Either EncodeEx) "r-UTF8" "r-UTF8" c B.ByteString
encUTF8B = _implEncodingEx (fmap TE.encodeUtf8 . TE.decodeUtf8')

encUTF8BL :: Encoding (Either EncodeEx) "r-UTF8" "r-UTF8" c BL.ByteString
encUTF8BL = _implEncodingEx (fmap TEL.encodeUtf8 . TEL.decodeUtf8')

-- * Decoding

instance (Applicative f) => Decode f "r-UTF8" "r-UTF8" c str where
    decoding = decAnyR

instance (RecreateErr f, Applicative f) =>  Validate f "r-UTF8" "r-UTF8" c B.ByteString  where
    validation = validR encUTF8B

instance (RecreateErr f, Applicative f) =>  Validate f "r-UTF8" "r-UTF8" c BL.ByteString  where
    validation = validR encUTF8BL


--- Utilities ---

-- | helper function checks that given ByteString, 
-- if is encoded as Left is must be not Utf8 decodable
-- is is encoded as Right is must be Utf8 encodable 
verEncoding :: B.ByteString -> Either err B.ByteString -> Bool
verEncoding bs (Left _) = isLeft . TE.decodeUtf8' $ bs
verEncoding bs (Right _) = isRight . TE.decodeUtf8' $ bs
