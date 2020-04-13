{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Data.Encoding (
    module Data.Encoding
    , module Data.Encoding.Internal.Class
    , module Data.Encoding.Instances.Base64
    , module Data.Encoding.Instances.Encode.Simple
    , Enc
    , unsafeGetPayload 
    , fromEncoding
    , toEncoding
 ) where

import           Data.Encoding.Internal.Types (Enc, unsafeGetPayload, toEncoding, fromEncoding)
import           Data.Encoding.Internal.Class
import           Data.Encoding.Instances.Base64
import           Data.Encoding.Instances.Encode.Simple

import           GHC.TypeLits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T


example1 :: Enc '["B64"] () B.ByteString
example1 = encodeAll . toEncoding () $ "Hello World"

example1_ :: B.ByteString
example1_ = fromEncoding . decodeAllLenient $ example1

example11 :: Enc '["B64","B64"] () B.ByteString
example11 = encodeAll . toEncoding () $ "Hello World"

example11_ :: B.ByteString
example11_ = fromEncoding . decodeAllLenient $ example11


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