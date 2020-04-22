{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Examples or moving between type annotated encodings
--
-- Modules that define encoding and decoding instances also provide conversion functions.
-- 
-- Currently, these are separate functions, generalization of conversions seems hard.
--
-- These examples discuss handling of __subsets__ (for character sets), __leniency__, and __flattening__. 
module Examples.TypedEncoding.Conversions where

import           Data.TypedEncoding
import qualified Data.TypedEncoding.Instances.Base64 as EnB64
import qualified Data.TypedEncoding.Instances.ASCII as EnASCII
import qualified Data.TypedEncoding.Instances.UTF8  as EnUTF8

import           Data.Proxy

import qualified Data.Text as T
import qualified Data.ByteString as B
import           Data.Text.Encoding.Error (UnicodeException)

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds
--
-- This module contains some ghci friently values to play with.
--
-- Each value is documented in a doctest style by including an equivalent ghci ready expression.
-- These documents generate a test suite for this libarary as well.


-- * Moving between Text and ByteString


eHelloAsciiB :: Either EnASCII.NonAsciiChar (Enc '["r-ASCII"] () B.ByteString)
eHelloAsciiB = encodeFAll . toEncoding () $ "HeLlo world" 
-- ^ Example value to play with
--
-- >>>  encodeFAll . toEncoding () $ "HeLlo world" :: Either EnASCII.NonAsciiChar (Enc '["r-ASCII"] () B.ByteString) 
-- Right (MkEnc Proxy () "HeLlo world")

Right helloAsciiB = eHelloAsciiB
-- ^ above with either removed

helloAsciiT :: Enc '["r-ASCII"] () T.Text
helloAsciiT = EnASCII.byteString2TextS helloAsciiB
-- ^ When converted to Text the annotation is preserved.
--
-- Currently separate function is defined for each allowed conversion. 
--
-- >>> EnASCII.byteString2TextS helloAsciiB
-- MkEnc Proxy () "HeLlo world"
-- >>> :t EnASCII.byteString2TextS helloAsciiB
-- EnASCII.byteString2TextS helloAsciiB :: Enc '["r-ASCII"] () T.Text


-- * Subsets

helloUtf8B :: Enc '["r-UTF8"] () B.ByteString
helloUtf8B = inject Proxy helloAsciiB
-- ^ To get UTF8 annotation, instead of doing this: 
--
-- >>> encodeFAll . toEncoding () $ "HeLlo world" :: Either UnicodeException (Enc '["r-UTF8"] () B.ByteString)
-- Right (MkEnc Proxy () "HeLlo world")
-- 
-- We should be able to convert the ASCII version.
--
-- This is done using 'Subset' typeclass.
--
-- @inject@ method accepts proxy to specify superset to use.
--
-- >>> showEnc $ inject (Proxy :: Proxy "r-UTF8") helloAsciiB
-- "MkEnc [r-UTF8 ...] () \"HeLlo world\""



-- * More complex rules

helloUtf8B64B :: Enc '["enc-B64", "r-UTF8"] () B.ByteString
helloUtf8B64B = encodePart (Proxy :: Proxy '["enc-B64"]) helloUtf8B 
-- ^ We put Base64 on the UFT8 ByteStream
--
-- >>> encodePart (Proxy :: Proxy '["enc-B64"]) helloUtf8B
-- MkEnc Proxy () "SGVMbG8gd29ybGQ="

helloUtf8B64T :: Enc '["enc-B64"] () T.Text
helloUtf8B64T = EnB64.byteString2TextS helloUtf8B64B  
-- ^ .. and copy it over to Text.
-- but UTF8 would be redundant in Text so the "r-UTF8" is dropped
--
-- >>> :t EnB64.byteString2TextS helloUtf8B64B
-- EnB64.byteString2TextS helloUtf8B64B :: Enc '["enc-B64"] () T.Text
--
-- Conversly moving back to ByteString recovers the annotation.
-- (there could be a choice of a UTF annotation to recover in the future)
-- 
-- >>> :t EnB64.text2ByteStringS helloUtf8B64T
-- EnB64.text2ByteStringS helloUtf8B64T
-- ... :: Enc '["enc-B64", "r-UTF8"] () B.ByteString

notTextB :: Enc '["enc-B64"] () B.ByteString
notTextB = encodeAll . toEncoding () $ "\195\177"
-- ^ 'notTextB' a binary, one that does not even represent valid UTF8.
-- 
-- >>> encodeAll . toEncoding () $ "\195\177" :: Enc '["enc-B64"] () B.ByteString
-- MkEnc Proxy () "w7E="
--
-- 'EnB64.byteString2TextS'' is a fuction that allows to convert Base 64 ByteString that is not UTF8.
-- 
-- >>> :t EnB64.byteString2TextS' notTextB
-- EnB64.byteString2TextS' notTextB
-- ... :: Enc '["enc-B64-nontext"] () T.Text
--
-- The result is annotated as "enc-B64-nontext" which prevents decoding it within 'T.Text' type.
-- We can only move it back to ByteStream as "enc-B64".



-- * Lenient recovery

lenientSomething :: Enc '["enc-B64-len"] () B.ByteString
lenientSomething = recreateAll . toEncoding () $ "abc==CB"
-- ^ 
-- >>> recreateAll . toEncoding () $ "abc==CB" :: Enc '["enc-B64-len"] () B.ByteString
-- MkEnc Proxy () "abc==CB"
--
-- The rest of Haskell does lenient decoding, type safety allows this library to use it for recovery.
-- lenient algorithms are not partial and automatically fix invalid input:
--
-- >>> recreateFAll . toEncoding () $ "abc==CB" :: Either RecreateEx (Enc '["enc-B64"] () B.ByteString)
-- Left (RecreateEx "\"invalid padding\"")
--
-- This library allows to recover to "enc-B64-len" which is different than "enc-B64"
--
-- 'EnB64.acceptLenientS' allows to convert "enc-B64-len" to "enc-B64"
--
-- >>> EnB64.acceptLenientS lenientSomething
-- MkEnc Proxy () "abc="
--
-- This is now properly encoded data
--
-- >>> recreateFAll . toEncoding () $ "abc=" :: Either RecreateEx (Enc '["enc-B64"] () B.ByteString)
-- Right (MkEnc Proxy () "abc=")
--
-- Except the content could be surprising
--
-- >>> decodeAll $ EnB64.acceptLenientS lenientSomething
-- MkEnc Proxy () "i\183"


-- * Flattening

b64IsAscii :: Enc '["r-ASCII"] () B.ByteString
b64IsAscii = flattenAs Proxy helloUtf8B64B
-- ^ Base 64 encodes binary data as ASCII text. 
-- thus, we should be able to treat "enc-B64" as "r-ASCII" losing some information.
-- this is done using 'FlattenAs' type class
--
-- >>> :t flattenAs (Proxy :: Proxy "r-ASCII") helloUtf8B64B
-- flattenAs (Proxy :: Proxy "r-ASCII") helloUtf8B64B
-- ... :: Enc '["r-ASCII"] () B.ByteString
