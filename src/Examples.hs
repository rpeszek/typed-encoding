{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Examples where

import           Data.Encoding

import           GHC.TypeLits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import           Data.Proxy
import           Data.Functor.Identity

b64Once :: Enc '["B64"] () B.ByteString
b64Once = encodeAll . toEncoding () $ "Hello World"

b64OnceDecoded :: B.ByteString
b64OnceDecoded = fromEncoding . decodeAll $ b64Once

b64Twice :: Enc '["B64","B64"] () B.ByteString
b64Twice = encodeAll . toEncoding () $ "Hello World"

b64TwiceDecoded' :: B.ByteString
b64TwiceDecoded' = fromEncoding . decodeAll $ b64Twice

b64TwiceDecoded :: B.ByteString
b64TwiceDecoded = fromEncoding . decodePart proxyNull $ b64Twice

b64PartiallyDecoded :: Enc '["B64"] () B.ByteString
b64PartiallyDecoded = decodePart (Proxy :: Proxy '["B64"]) $ b64Twice


exupper :: Enc '["UPPER"] () T.Text
exupper = encodeAll . toEncoding () $ "Hello World"


extitlerev :: Enc '["reverse", "Title"] () T.Text
extitlerev = encodeAll . toEncoding () $ "HeLlo world"

extitlerev' :: Enc '["reverse", "Title"] Config T.Text
extitlerev' = encodeAll . toEncoding exampleConf $ "HeLlo world"

data Config = Config {
    sizeLimit :: SizeLimit
  } deriving (Show)
exampleConf = Config (SizeLimit 8) 

instance HasA Config SizeLimit where
   has _ = sizeLimit  

exlimit :: Enc '["size-limit", "reverse", "Title"] Config T.Text
exlimit = encodeAll . toEncoding exampleConf $ "HeLlo world"

extitle :: Enc '["Title"] Config T.Text
extitle = encodeAll . toEncoding exampleConf $ "hello wOrld"

expartial :: Enc '["size-limit", "reverse", "Title"] Config T.Text
expartial = encodePart (Proxy :: Proxy '["Title"])  extitle

-- | this will not work because "B64" instances are for ByteString not Text
-- exlimitB64 :: Enc '["B64", "size-limit", "reverse", "Title"] Config T.Text
-- exlimitB64 = encodeAll . toEncoding exampleConf $ "HeLlo world"
exlimitB64 :: Enc '["B64", "size-limit"] Config B.ByteString
exlimitB64 = encodeAll . toEncoding exampleConf $ "HeLlo world"

exlimitParDec :: Enc '["size-limit"] Config B.ByteString
exlimitParDec =  decodePart (Proxy :: Proxy '["size-limit"]) $ exlimitB64