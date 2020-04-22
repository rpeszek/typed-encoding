{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- | type-encoding overview examples. 
--
-- This library is concerned with 3 main operations done on strings:
-- __encoding__, __decoding__, and __recovery__.  Examples in this module cover all
-- of these base cases.
--
-- This module uses encoding instances found in 
--
-- * 'Data.Encoding.Instances.Base64' (Data.Encoding.Instances.Base64)
--
-- * 'Data.Encoding.Instances.ASCII' (Data.Encoding.Instances.ASCII)
--
-- * 'Data.Encoding.Instances.Encode.Sample' (Data.Encoding.Instances.Encode.Sample)
--

module Examples.Overview where

import           Data.Encoding
import           Data.Encoding.Instances.Base64
import           Data.Encoding.Instances.ASCII
import           Data.Encoding.Instances.Encode.Sample
 
import           GHC.TypeLits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import           Data.Proxy
import           Data.Functor.Identity


-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds
--
-- This module contains some ghci friently values to play with.
--
-- Each value is documented in a doctest style by including an equivalent ghci ready expression.
-- These documents generate a test suite for this libarary as well.

-- * Basics

-- | "Hello World" encoded as Base64
--
-- >>> helloB64 
-- MkEnc Proxy () "SGVsbG8gV29ybGQ="
--
-- >>> encodeAll . toEncoding () $ "Hello World" :: Enc '["enc-B64"] () B.ByteString
-- MkEnc Proxy () "SGVsbG8gV29ybGQ="
helloB64 :: Enc '["enc-B64"] () B.ByteString
helloB64 = encodeAll . toEncoding () $ "Hello World"

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
-- Left (RecreateEx "\"invalid padding\"")
helloB64Recovered :: Either RecreateEx (Enc '["enc-B64"] () B.ByteString)
helloB64Recovered = recreateFAll . toEncoding () $ "SGVsbG8gV29ybGQ="

-- | "Hello World" double-Base64 encoded.
-- Notice the same code used as in single encoding, the game is played at type level.
--
-- >>> encodeAll . toEncoding () $ "Hello World" :: Enc '["enc-B64","enc-B64"] () B.ByteString  
-- MkEnc Proxy () "U0dWc2JHOGdWMjl5YkdRPQ=="
helloB64B64 :: Enc '["enc-B64","enc-B64"] () B.ByteString
helloB64B64 = encodeAll . toEncoding () $ "Hello World"

-- | Double Base64 encoded "Hello World" with one layer of encoding removed
--
-- >>> decodePart (Proxy :: Proxy '["enc-B64"]) $ helloB64B64 :: Enc '["enc-B64"] () B.ByteString
-- MkEnc Proxy () "SGVsbG8gV29ybGQ="
--
-- >>> helloB64B64PartDecode == helloB64
-- True
helloB64B64PartDecode :: Enc '["enc-B64"] () B.ByteString
helloB64B64PartDecode = decodePart (Proxy :: Proxy '["enc-B64"]) $ helloB64B64

-- | 'helloB64B64' all the way to 'B.ByteString'
--
-- Notice a similar polymorphism in decoding.
--
-- >>> fromEncoding . decodeAll $ helloB64B64 :: B.ByteString 
-- "Hello World"
-- 
-- We can also decode all the parts: 
--
-- >>> fromEncoding . decodePart (Proxy :: Proxy '["enc-B64","enc-B64"]) $ helloB64B64
-- "Hello World"
helloB64B64Decoded :: B.ByteString
helloB64B64Decoded = fromEncoding . decodeAll $ helloB64B64

-- | what happens when we try to recover encoded once text to @Enc '["enc-B64", "enc-B64"]@. 
--
-- Again, notice the same expression is used as in previous recovery.
--
-- >>> recreateFAll . toEncoding () $ "SGVsbG8gV29ybGQ=" :: Either RecreateEx (Enc '["enc-B64", "enc-B64"] () B.ByteString)
-- Left (RecreateEx "\"invalid padding\"")
helloB64B64RecoveredErr :: Either RecreateEx (Enc '["enc-B64", "enc-B64"] () B.ByteString)
helloB64B64RecoveredErr = recreateFAll . toEncoding () $ "SGVsbG8gV29ybGQ="



-- * "do-" Encodings

-- |
-- "do-UPPER" (from 'Data.Encoding.Instances.Encode.Sample' module) encoding applied to "Hello World"
--
-- Notice a namespace thing going on, "enc-" is encoding, "do-" is some tranformation. 
-- These are typically not reversable, some could be recoverable.
--  
-- The same code is used as in "enc-" examples to encode (now transform).
--
-- >>> encodeAll . toEncoding () $ "Hello World" :: Enc '["do-UPPER"] () T.Text
-- MkEnc Proxy () "HELLO WORLD"
helloUPP :: Enc '["do-UPPER"] () T.Text
helloUPP = encodeAll . toEncoding () $ "Hello World"

-- | Sample compound tranformation 
-- 
-- >>> encodeAll . toEncoding () $ "HeLLo world" :: Enc '["do-reverse", "do-Title"] () T.Text
-- MkEnc Proxy () "dlroW olleH" 
helloTitleRev :: Enc '["do-reverse", "do-Title"] () T.Text
helloTitleRev = encodeAll . toEncoding () $ "HeLLo world"



-- * Configuration

-- | Example configuration
data Config = Config {
    sizeLimit :: SizeLimit
  } deriving (Show)
exampleConf = Config (SizeLimit 8) 

instance HasA Config SizeLimit where
   has _ = sizeLimit  

-- | `helloTitle' is needed in following examples
--
helloTitle :: Enc '["do-Title"] Config T.Text
helloTitle = encodeAll . toEncoding exampleConf $ "hello wOrld"

-- | Configuration can be used to impact the encoding process.
--
-- So far we had used @()@ as configuration of all encodings.
-- But since both "do-reverse", "do-Title" are polymorphic in 
-- configuration we can also do this:
--
-- >>> encodeAll . toEncoding exampleConf $ "HeLLo world" :: Enc '["do-reverse", "do-Title"] Config T.Text
-- MkEnc Proxy (Config {sizeLimit = SizeLimit {unSizeLimit = 8}}) "dlroW olleH"
--
-- >>> encodeAll . toEncoding exampleConf $ "HeLlo world" :: Enc '["do-size-limit", "do-reverse", "do-Title"] Config T.Text
-- MkEnc Proxy (Config {sizeLimit = SizeLimit {unSizeLimit = 8}}) "dlroW ol"
--
-- Instead, encode previously defined 'helloTitle' by reversing it and adding size limit
--
-- >>> encodePart (Proxy :: Proxy '["do-size-limit", "do-reverse"]) helloTitle :: Enc '["do-size-limit", "do-reverse", "do-Title"] Config T.Text
-- MkEnc Proxy (Config {sizeLimit = SizeLimit {unSizeLimit = 8}}) "dlroW ol"
helloRevLimit :: Enc '["do-size-limit", "do-reverse", "do-Title"] Config T.Text
helloRevLimit = encodePart (Proxy :: Proxy '["do-size-limit", "do-reverse"]) helloTitle

-- >>> encodeAll . toEncoding exampleConf $ "HeLlo world" :: Enc '["enc-B64", "do-size-limit"] Config B.ByteString
-- MkEnc Proxy (Config {sizeLimit = SizeLimit {unSizeLimit = 8}}) "SGVMbG8gd28="
helloLimitB64 :: Enc '["enc-B64", "do-size-limit"] Config B.ByteString
helloLimitB64 = encodeAll . toEncoding exampleConf $ "HeLlo world"

-- | ... and we unwrap the B64 part only
-- 
-- >>> decodePart (Proxy :: Proxy '["enc-B64"]) $ helloLimitB64
-- MkEnc Proxy (Config {sizeLimit = SizeLimit {unSizeLimit = 8}}) "HeLlo wo"
helloRevLimitParDec :: Enc '["do-size-limit"] Config B.ByteString
helloRevLimitParDec =  decodePart (Proxy :: Proxy '["enc-B64"]) $ helloLimitB64




-- * "r-" encodings section

-- | ASCII char set
-- ByteStrings are sequences of Bytes ('Data.Word.Word8'). The type
-- is very permissive, it may contain binary data such as jpeg picture.
--
-- "r-ASCII" encoding acts as partial identity function
-- it does not change any bytes in bytestream but it fails if a byte
-- is outside of ASCII range (in @Either@ monad).
--
-- Note naming thing: "r-" is paritial identity ("r-" is from restriction).
--
-- >>>  encodeFAll . toEncoding () $ "HeLlo world" :: Either NonAsciiChar (Enc '["r-ASCII"] () B.ByteString) 
-- Right (MkEnc Proxy () "HeLlo world")
helloAscii :: Either NonAsciiChar (Enc '["r-ASCII"] () B.ByteString)
helloAscii = encodeFAll . toEncoding () $ "HeLlo world" 

-- | Arguably the type we used for helloB64 was too permissive.
-- a better version is here:
--
-- >>> encodeFAll . toEncoding () $ "Hello World" :: Either NonAsciiChar (Enc '["enc-B64", "r-ASCII"] () B.ByteString)
-- Right (MkEnc Proxy () "SGVsbG8gV29ybGQ=") 
helloAsciiB64 :: Either NonAsciiChar (Enc '["enc-B64", "r-ASCII"] () B.ByteString)
helloAsciiB64 = encodeFAll . toEncoding () $ "Hello World"

-- |
-- >>> decodePart (Proxy :: Proxy '["enc-B64"]) <$> helloAsciiB64
-- Right (MkEnc Proxy () "Hello World")
helloAsciiB64PartDec :: Either NonAsciiChar (Enc '["r-ASCII"] () B.ByteString)
helloAsciiB64PartDec = decodePart (Proxy :: Proxy '["enc-B64"]) <$> helloAsciiB64 