{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Examples or moving between type annotated encodings
module Examples.Conversions where

import           Data.Encoding
import qualified Data.Encoding.Instances.Base64 as EnB64
import qualified Data.Encoding.Instances.ASCII as EnASCII
import qualified Data.Encoding.Instances.UTF8  as EnUTF8

import           Data.Proxy

import qualified Data.Text as T
import qualified Data.ByteString as B
import           Data.Text.Encoding.Error (UnicodeException)

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds


-- | A simple ASCII example
--
-- >>>  encodeFAll . toEncoding () $ "HeLlo world" :: Either EnASCII.NonAsciiChar (Enc '["r-ASCII"] () B.ByteString) 
-- Right (MkEnc Proxy () "HeLlo world")
exAsciiE :: Either EnASCII.NonAsciiChar (Enc '["r-ASCII"] () B.ByteString)
exAsciiE = encodeFAll . toEncoding () $ "HeLlo world" 

-- sorry for this bad short-cut
Right exAscii = exAsciiE

-- | It makes sense to keep the ASCII annotation when converting to Text
exAsciiT :: Enc '["r-ASCII"] () T.Text
exAsciiT = EnASCII.byteString2TextS exAscii

-- | Simple UTF8 ByteString encoding the same "HeLlo world"
exUtf8E :: Either UnicodeException (Enc '["r-UTF8"] () B.ByteString)
exUtf8E = encodeFAll . toEncoding () $ "HeLlo world" 

-- | I should be able to convert the ASCII version instead!
-- This is done using Subset typeclass.
-- Subsets make sense for character sets / "r-" types!
--
-- @inject@ method accepts proxy to specify superset to use
--
-- >>> showEnc $ inject (Proxy :: Proxy "r-UTF8") exAscii
-- "MkEnc [r-UTF8 ...] () \"HeLlo world\""
exUtf8 :: Enc '["r-UTF8"] () B.ByteString
exUtf8 = inject Proxy exAscii

-- | We can apply a tranformation to UFT8 ByteString
exUtf8B64 :: Enc '["enc-B64", "r-UTF8"] () B.ByteString
exUtf8B64 = encodePart (Proxy :: Proxy '["enc-B64"]) exUtf8 

-- | .. and copy it over to Text.
-- but UTF8 would be redundant in Text so the "r-UTF8" is dropped
--
-- >>> :t EnB64.byteString2TextS exUtf8B64
-- EnB64.byteString2TextS exUtf8B64 :: Enc '["enc-B64"] () T.Text
--
-- Conversly moving back to ByteString recovers the annotation.
-- (there could be a choice of a UTF annotation to recover in the future)
-- 
-- >>> :t EnB64.text2ByteStringS exUtf8B64T
-- EnB64.text2ByteStringS exUtf8B64T
-- ... :: Enc '["enc-B64", "r-UTF8"] () B.ByteString
exUtf8B64T :: Enc '["enc-B64"] () T.Text
exUtf8B64T = EnB64.byteString2TextS exUtf8B64  

-- | Byte64 encodes binary data in ASCII text. 
-- thus, we should be able to treat "enc-B64" as "r-ASCII" losing some information.
-- this is done using FlattenAs type class
--
-- >>> :t flattenAs (Proxy :: Proxy "r-ASCII") exUtf8B64
-- flattenAs (Proxy :: Proxy "r-ASCII") exUtf8B64
-- ... :: Enc '["r-ASCII"] () B.ByteString
b64IsAscii :: Enc '["r-ASCII"] () B.ByteString
b64IsAscii = flattenAs Proxy exUtf8B64