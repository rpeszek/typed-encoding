{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- | Overview examples showing how this library can be used 
module UsageExamples where

import           Data.Encoding
import           Data.Encoding.Instances.Base64
import           Data.Encoding.Instances.Encode.Sample
 
import           GHC.TypeLits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import           Data.Proxy
import           Data.Functor.Identity

-- | "Hello World" encoded as Base64
--
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds
-- >>> b64Once 
-- MkEnc Proxy () "SGVsbG8gV29ybGQ="
--
-- >>> encodeAll . toEncoding () $ "Hello World" :: Enc '["enc-B64"] () B.ByteString
-- MkEnc Proxy () "SGVsbG8gV29ybGQ="
b64Once :: Enc '["enc-B64"] () B.ByteString
b64Once = encodeAll . toEncoding () $ "Hello World"

-- | Previous text decoded from Base64
--
-- >>> fromEncoding . decodeAll $ b64Once 
-- "Hello World"
b64OnceDecoded :: B.ByteString
b64OnceDecoded = fromEncoding . decodeAll $ b64Once

-- | "Hello World" double Base64 encoded.
-- Notice the same code used as in single encoding, the game is played at type level.
--
-- >>> encodeAll . toEncoding () $ "Hello World" :: Enc '["enc-B64","enc-B64"] () B.ByteString  
-- MkEnc Proxy () "U0dWc2JHOGdWMjl5YkdRPQ=="
b64Twice :: Enc '["enc-B64","enc-B64"] () B.ByteString
b64Twice = encodeAll . toEncoding () $ "Hello World"

-- | Previous text decoded from double Base64 encoding.
-- Notice same code that does it as single decoding.
--
-- >>> fromEncoding . decodeAll $ b64Twice :: B.ByteString 
-- "Hello World"
b64TwiceDecoded' :: B.ByteString
b64TwiceDecoded' = fromEncoding . decodeAll $ b64Twice

-- | Double Base64 encoded "Hello World" with one layer of encoding removed
--
-- >>> decodePart (Proxy :: Proxy '["enc-B64"]) $ b64Twice :: Enc '["enc-B64"] () B.ByteString
-- MkEnc Proxy () "SGVsbG8gV29ybGQ="
--
-- >>> b64PartiallyDecoded == b64Once
-- True
b64PartiallyDecoded :: Enc '["enc-B64"] () B.ByteString
b64PartiallyDecoded = decodePart (Proxy :: Proxy '["enc-B64"]) $ b64Twice

-- | All layers of encoding removed using 'decodePart'
--
-- >>> fromEncoding . decodePart (Proxy :: Proxy '["enc-B64","enc-B64"]) $ b64Twice
-- "Hello World"
b64TwiceDecoded :: B.ByteString
b64TwiceDecoded = fromEncoding . decodePart (Proxy :: Proxy '["enc-B64","enc-B64"]) $ b64Twice


-- | Sample "do-UPPER" encoding applied to "Hello World"
-- Notice a namespace thing going on, "enc-" is encoding, "do-" is some tranformation.  
-- The same code is used as in "enc-" examples to transform.
--
-- >>> encodeAll . toEncoding () $ "Hello World" :: Enc '["do-UPPER"] () T.Text
-- MkEnc Proxy () "HELLO WORLD"
exupper :: Enc '["do-UPPER"] () T.Text
exupper = encodeAll . toEncoding () $ "Hello World"

-- | Sample compound tranformation 
-- 
-- >>> encodeAll . toEncoding () $ "HeLLo world" :: Enc '["do-reverse", "do-Title"] () T.Text
-- MkEnc Proxy () "dlroW olleH" 
extitlerev :: Enc '["do-reverse", "do-Title"] () T.Text
extitlerev = encodeAll . toEncoding () $ "HeLLo world"


data Config = Config {
    sizeLimit :: SizeLimit
  } deriving (Show)
exampleConf = Config (SizeLimit 8) 

instance HasA Config SizeLimit where
   has _ = sizeLimit  

-- | So far we had used '()' as configuration of all encodings.
-- but since both "do-reverse", "do-Title" are polymorphic in 
-- configuration we can also do this:
--
-- >>> encodeAll . toEncoding exampleConf $ "HeLLo world" :: Enc '["do-reverse", "do-Title"] Config T.Text
-- MkEnc Proxy (Config {sizeLimit = SizeLimit {unSizeLimit = 8}}) "dlroW olleH"
extitlerev' :: Enc '["do-reverse", "do-Title"] Config T.Text
extitlerev' = encodeAll . toEncoding exampleConf $ "HeLLo world"

-- | Configuration can now be used
--
-- >>> encodeAll . toEncoding exampleConf $ "HeLlo world" :: Enc '["do-size-limit", "do-reverse", "do-Title"] Config T.Text
-- MkEnc Proxy (Config {sizeLimit = SizeLimit {unSizeLimit = 8}}) "dlroW ol"
exlimit :: Enc '["do-size-limit", "do-reverse", "do-Title"] Config T.Text
exlimit = encodeAll . toEncoding exampleConf $ "HeLlo world"

-- | Needed in following examples, simply applies toTitle to "Hello wOrld"
extitle :: Enc '["do-Title"] Config T.Text
extitle = encodeAll . toEncoding exampleConf $ "hello wOrld"

-- | Putting things together
-- Partialy encode previously defined 'extitle' by reversing it and adding size limit
--
-- >>> encodePart (Proxy :: Proxy '["do-size-limit", "do-reverse"]) extitle :: Enc '["do-size-limit", "do-reverse", "do-Title"] Config T.Text
-- MkEnc Proxy (Config {sizeLimit = SizeLimit {unSizeLimit = 8}}) "dlroW ol"
expartial :: Enc '["do-size-limit", "do-reverse", "do-Title"] Config T.Text
expartial = encodePart (Proxy :: Proxy '["do-size-limit", "do-reverse"]) extitle

-- | Here is parital decode example
-- Start with encoded string
--
-- >>> encodeAll . toEncoding exampleConf $ "HeLlo world" :: Enc '["enc-B64", "do-size-limit"] Config B.ByteString
-- MkEnc Proxy (Config {sizeLimit = SizeLimit {unSizeLimit = 8}}) "SGVMbG8gd28="
exlimitB64 :: Enc '["enc-B64", "do-size-limit"] Config B.ByteString
exlimitB64 = encodeAll . toEncoding exampleConf $ "HeLlo world"

-- | ... And unwrap the B64 part only
-- 
-- >>> decodePart (Proxy :: Proxy '["enc-B64"]) $ exlimitB64
-- MkEnc Proxy (Config {sizeLimit = SizeLimit {unSizeLimit = 8}}) "HeLlo wo"
exlimitParDec :: Enc '["do-size-limit"] Config B.ByteString
exlimitParDec =  decodePart (Proxy :: Proxy '["enc-B64"]) $ exlimitB64

-- Decoding errors, why?

-- Moving between types

-- Restriction types
