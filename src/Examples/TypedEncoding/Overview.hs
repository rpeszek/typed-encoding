{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- {-# LANGUAGE PartialTypeSignatures #-}
-- {-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | type-encoding overview examples. 
--
-- This library is concerned with 3 main operations done on strings:
-- __encoding__, __decoding__, and __recovery__.  Examples in this module cover all
-- of these base cases.
--
-- This module uses encoding instances found in 
--
-- * "Data.TypedEncoding.Instances.Enc.Base64"
-- * "Data.TypedEncoding.Instances.Restriction.ASCII"
-- * "Data.TypedEncoding.Instances.Do.Sample"
--

module Examples.TypedEncoding.Overview where

import           Data.TypedEncoding
import           Data.TypedEncoding.Instances.Enc.Base64 ()
import           Data.TypedEncoding.Instances.Restriction.ASCII ()
import           Data.TypedEncoding.Instances.Do.Sample
 
import qualified Data.ByteString as B
import qualified Data.Text as T


-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications
--
-- This module contains some ghci friendly values to play with.
--
-- Each value is documented in a doctest style by including an equivalent ghci ready expression.
-- These documents generate a test suite for this library as well.

-- * Basics

-- | "Hello World" encoded as Base64
--
-- >>> helloB64 
-- MkEnc Proxy () "SGVsbG8gV29ybGQ="
--
-- >>> displ helloB64
-- "MkEnc '[enc-B64] () (ByteString SGVsbG8gV29ybGQ=)"
-- 
-- >>> encAll . toEncoding () $ "Hello World" :: Enc '["enc-B64"] () B.ByteString
-- MkEnc Proxy () "SGVsbG8gV29ybGQ="
--
-- There is currently an alternative polymorphic way (prototype) to create encodings by /running/ 'Encoder' on 'encodings'.
-- This approach works better with more open / dynamic encoding setup it also provides first class @Encoder@. 
-- In future versions encodeAll will either be improved to provide similar flexibility or will be deprecated and removed.
-- 
-- TODO v0.3
-- displ <$> (runEncoder @'["enc-B64"] encoder $ toEncoding () "Hello" :: Either EncodeEx (Enc '["enc-B64"] () B.ByteString))
-- Right "MkEnc '[enc-B64] () (ByteString SGVsbG8=)"
helloB64 :: Enc '["enc-B64"] () B.ByteString
helloB64 = encAll . toEncoding () $ "Hello World"

-- | Previous text decoded from Base64
--
-- >>> fromEncoding . decodeAll $ helloB64 
-- "Hello World"
helloB64Decoded :: B.ByteString
helloB64Decoded = fromEncoding . decodeAll $ helloB64

-- | 'recreateFAll' allows for recovering data at program boundaries (for example, when parsing JSON input).
-- It makes sure that the content satisfies specified encodings.
-- 
-- >>> recreateFAll . toEncoding () $ "SGVsbG8gV29ybGQ=" :: Either RecreateEx (Enc '["enc-B64"] () B.ByteString)
-- Right (MkEnc Proxy () "SGVsbG8gV29ybGQ=")
--
-- >>> recreateFAll . toEncoding () $ "SGVsbG8gV29ybGQ" :: Either RecreateEx (Enc '["enc-B64"] () B.ByteString)
-- Left (RecreateEx "enc-B64" ("invalid padding"))
--
-- The above example start by placing payload in zero-encoded @Enc '[] ()@ type and then apply 'recreateFAll'
-- this is a good way to recreate encoded type if encoding is known. 
--
-- If is it not, 'UncheckedEnc' type can be used. 
--
-- (See 'Examples.TypedEncoding.ToEncString' for better example).
-- 
-- This module is concerned only with the first approach. 
--
-- >>> let unchecked = toUncheckedEnc ["enc-B64"] () ("SGVsbG8gV29ybGQ=" :: T.Text)
-- >>> verifyUncheckedEnc' @'["enc-B64"] unchecked
-- Just (Right (MkEnc Proxy () "SGVsbG8gV29ybGQ="))
helloB64Recovered :: Either RecreateEx (Enc '["enc-B64"] () B.ByteString)
helloB64Recovered = recreateFAll . toEncoding () $ "SGVsbG8gV29ybGQ="

-- | "Hello World" double-Base64 encoded.
-- Notice the same code used as in single encoding, the game is played at type level.
--
-- >>> encAll . toEncoding () $ "Hello World" :: Enc '["enc-B64","enc-B64"] () B.ByteString  
-- MkEnc Proxy () "U0dWc2JHOGdWMjl5YkdRPQ=="
--
-- >>> displ helloB64B64
-- "MkEnc '[enc-B64,enc-B64] () (ByteString U0dWc2JHOGdWMjl5YkdRPQ==)"
helloB64B64 :: Enc '["enc-B64","enc-B64"] () B.ByteString
helloB64B64 = encAll . toEncoding () $ "Hello World"

-- | Double Base64 encoded "Hello World" with one layer of encoding removed
--
-- >>> decodePart @'["enc-B64"] $ helloB64B64 :: Enc '["enc-B64"] () B.ByteString
-- MkEnc Proxy () "SGVsbG8gV29ybGQ="
--
-- >>> helloB64B64PartDecode == helloB64
-- True
helloB64B64PartDecode :: Enc '["enc-B64"] () B.ByteString
helloB64B64PartDecode = decodePart @'["enc-B64"] helloB64B64

-- | 'helloB64B64' all the way to 'B.ByteString'
--
-- Notice a similar polymorphism in decoding.
--
-- >>> fromEncoding . decodeAll $ helloB64B64 :: B.ByteString 
-- "Hello World"
-- 
-- We can also decode all the parts: 
--
-- >>> fromEncoding . decodePart @'["enc-B64","enc-B64"] $ helloB64B64
-- "Hello World"
helloB64B64Decoded :: B.ByteString
helloB64B64Decoded = fromEncoding . decodeAll $ helloB64B64

-- | what happens when we try to recover encoded once text to @Enc '["enc-B64", "enc-B64"]@. 
--
-- Again, notice the same expression is used as in previous recovery. 
--
-- >>> recreateFAll . toEncoding () $ "SGVsbG8gV29ybGQ=" :: Either RecreateEx (Enc '["enc-B64", "enc-B64"] () B.ByteString)
-- Left (RecreateEx "enc-B64" ("invalid padding"))
helloB64B64RecoveredErr :: Either RecreateEx (Enc '["enc-B64", "enc-B64"] () B.ByteString)
helloB64B64RecoveredErr = recreateFAll . toEncoding () $ "SGVsbG8gV29ybGQ="



-- * "do-" Encodings

-- |
-- "do-UPPER" (from 'Data.TypedEncoding.Instances.Do.Sample' module) encoding applied to "Hello World"
--
-- Notice a namespace thing going on, "enc-" is encoding, "do-" is some transformation. 
-- These are typically not reversible, some could be recoverable.
--  
-- The same code is used as in "enc-" examples to encode (now transform).
--
-- >>> encAll . toEncoding () $ "Hello World" :: Enc '["do-UPPER"] () T.Text
-- MkEnc Proxy () "HELLO WORLD"
helloUPP :: Enc '["do-UPPER"] () T.Text
helloUPP = encAll . toEncoding () $ "Hello World"

-- | Sample compound transformation 
-- 
-- >>> encAll . toEncoding () $ "HeLLo world" :: Enc '["do-reverse", "do-Title"] () T.Text
-- MkEnc Proxy () "dlroW olleH" 
helloTitleRev :: Enc '["do-reverse", "do-Title"] () T.Text
helloTitleRev = encAll . toEncoding () $ "HeLLo world"



-- * Configuration

-- | Example configuration
newtype Config = Config {
    sizeLimit :: SizeLimit
  } deriving (Show)
exampleConf = Config (SizeLimit 8) 

instance HasA SizeLimit Config where
   has = sizeLimit  

-- | `helloTitle' is needed in following examples
--
helloTitle :: Enc '["do-Title"] Config T.Text
helloTitle = encAll . toEncoding exampleConf $ "hello wOrld"

-- | Configuration can be used to impact the encoding process.
--
-- So far we had used @()@ as configuration of all encodings.
-- But since both "do-reverse", "do-Title" are polymorphic in 
-- configuration we can also do this:
--
-- >>> encAll . toEncoding exampleConf $ "HeLLo world" :: Enc '["do-reverse", "do-Title"] Config T.Text
-- MkEnc Proxy (Config {sizeLimit = SizeLimit {unSizeLimit = 8}}) "dlroW olleH"
--
-- >>> encAll . toEncoding exampleConf $ "HeLlo world" :: Enc '["do-size-limit", "do-reverse", "do-Title"] Config T.Text
-- MkEnc Proxy (Config {sizeLimit = SizeLimit {unSizeLimit = 8}}) "dlroW ol"
--
-- Instead, encode previously defined 'helloTitle' by reversing it and adding size limit
--
-- >>> encPart @'["do-size-limit", "do-reverse"] helloTitle :: Enc '["do-size-limit", "do-reverse", "do-Title"] Config T.Text
-- MkEnc Proxy (Config {sizeLimit = SizeLimit {unSizeLimit = 8}}) "dlroW ol"
helloRevLimit :: Enc '["do-size-limit", "do-reverse", "do-Title"] Config T.Text
helloRevLimit = encPart @'["do-size-limit", "do-reverse"] helloTitle

-- >>> encAll . toEncoding exampleConf $ "HeLlo world" :: Enc '["enc-B64", "do-size-limit"] Config B.ByteString
-- MkEnc Proxy (Config {sizeLimit = SizeLimit {unSizeLimit = 8}}) "SGVMbG8gd28="
helloLimitB64 :: Enc '["enc-B64", "do-size-limit"] Config B.ByteString
helloLimitB64 = encAll . toEncoding exampleConf $ "HeLlo world"

-- | ... and we unwrap the B64 part only
-- 
-- >>> decodePart @'["enc-B64"] $ helloLimitB64
-- MkEnc Proxy (Config {sizeLimit = SizeLimit {unSizeLimit = 8}}) "HeLlo wo"
helloRevLimitParDec :: Enc '["do-size-limit"] Config B.ByteString
helloRevLimitParDec =  decodePart @'["enc-B64"] helloLimitB64




-- * "r-" encodings section

-- | ASCII char set
-- ByteStrings are sequences of Bytes ('Data.Word.Word8'). The type
-- is very permissive, it may contain binary data such as jpeg picture.
--
-- "r-ASCII" encoding acts as partial identity function
-- it does not change any bytes in bytestring but it fails if a byte
-- is outside of ASCII range (in @Either@ monad).
--
-- Note naming thing: "r-" is partial identity ("r-" is from restriction).
--
-- >>>  encFAll . toEncoding () $ "HeLlo world" :: Either EncodeEx (Enc '["r-ASCII"] () B.ByteString) 
-- Right (MkEnc Proxy () "HeLlo world")
helloAscii :: Either EncodeEx (Enc '["r-ASCII"] () B.ByteString)
helloAscii = encFAll . toEncoding () $ "HeLlo world" 

-- | Arguably the type we used for helloB64 was too permissive.
-- a better version is here:
--
-- >>> encFAll . toEncoding () $ "Hello World" :: Either EncodeEx (Enc '["enc-B64", "r-ASCII"] () B.ByteString)
-- Right (MkEnc Proxy () "SGVsbG8gV29ybGQ=") 
helloAsciiB64 :: Either EncodeEx (Enc '["enc-B64", "r-ASCII"] () B.ByteString)
helloAsciiB64 = encFAll . toEncoding () $ "Hello World"

-- |
-- >>> decodePart @'["enc-B64"] <$> helloAsciiB64
-- Right (MkEnc Proxy () "Hello World")
helloAsciiB64PartDec :: Either EncodeEx (Enc '["r-ASCII"] () B.ByteString)
helloAsciiB64PartDec = decodePart @'["enc-B64"] <$> helloAsciiB64 
