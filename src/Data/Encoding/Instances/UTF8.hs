{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE KindSignatures #-}

-- | 
module Data.Encoding.Instances.UTF8 where

import           Data.Encoding.Instances.Support

import           Data.Proxy
import           Data.Functor.Identity
import           GHC.TypeLits

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE 
import qualified Data.Text.Lazy.Encoding as TEL 

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8

import           GHC.TypeLits
import           Data.Char
import           Data.Encoding.Internal.Utils (explainBool)
import           Data.Encoding.Unsafe (withUnsafe)
import           Control.Arrow
import           Data.Text.Encoding.Error (UnicodeException)

-----------------
-- Conversions --
-----------------

-- Right tst = encodeFAll . toEncoding () $ "Hello World" :: Either UnicodeException (Enc '["r-UTF8"] () B.ByteString)
-- tstTxt = byteString2TextS tst

-- | Type-safer version of @Data.Text.Encoding.encodeUtf8@
--
-- >>> text2ByteStringS $ toEncoding () ("text" :: T.Text)
-- MkEnc Proxy () "text"
text2ByteStringS :: Enc ys c T.Text -> Enc ("r-UTF8" ': ys) c B.ByteString
text2ByteStringS = withUnsafeCoerce TE.encodeUtf8

-- | Type-safer version of Data.Text.Encoding.decodeUtf8
--
-- >>> let Right tst = encodeFAll . toEncoding () $ "Hello World" :: Either UnicodeException (Enc '["r-UTF8"] () B.ByteString)
-- >>> byteString2TextS tst
-- MkEnc Proxy () "Hello World"
byteString2TextS :: Enc ("r-UTF8" ': ys) c B.ByteString -> Enc ys c T.Text 
byteString2TextS = withUnsafeCoerce TE.decodeUtf8

text2ByteStringL :: Enc ys c TL.Text -> Enc ("r-UTF8" ': ys) c BL.ByteString
text2ByteStringL = withUnsafeCoerce TEL.encodeUtf8

byteString2TextL :: Enc ("r-UTF8" ': ys) c BL.ByteString -> Enc ys c TL.Text 
byteString2TextL = withUnsafeCoerce TEL.decodeUtf8

-----------------
-- Encondings  --
-----------------

-- TODO these are quick and dirty

-- | UTF8 encodings are defined for ByteStream only as that would not make much sense for Text
--
-- >>> encodeFAll . toEncoding () $ "\xc3\xb1" :: Either UnicodeException (Enc '["r-UTF8"] () B.ByteString)
-- Right (MkEnc Proxy () "\195\177")
-- >>> encodeFAll . toEncoding () $ "\xc3\x28" :: Either UnicodeException (Enc '["r-UTF8"] () B.ByteString)
-- Left Cannot decode byte '\xc3': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream
instance EncodeF (Either UnicodeException) (Enc xs c B.ByteString) (Enc ("r-UTF8" ': xs) c B.ByteString) where
    encodeF = implTranF (fmap TE.encodeUtf8 . TE.decodeUtf8')
instance Applicative f => DecodeF f (Enc ("r-UTF8" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
    decodeF = implTranP id 

instance EncodeF (Either UnicodeException) (Enc xs c BL.ByteString) (Enc ("r-UTF8" ': xs) c BL.ByteString) where
    encodeF = implTranF (fmap TEL.encodeUtf8 . TEL.decodeUtf8')
instance Applicative f => DecodeF f (Enc ("r-UTF8" ': xs) c BL.ByteString) (Enc xs c BL.ByteString) where
    decodeF = implTranP id 

