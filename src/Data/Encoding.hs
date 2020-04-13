{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Data.Encoding (
    module Data.Encoding
    , module Data.Encoding.Internal.Class
    , module Data.Encoding.Instances.Base64
    , Enc
    , unsafeGetPayload 
 ) where

import           Data.Encoding.Internal.Types (Enc, unsafeGetPayload)
import           Data.Encoding.Internal.Class
import           Data.Encoding.Instances.Base64

import           GHC.TypeLits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

example1 :: Enc "B64URL" B.ByteString
example1 = encode "Hello John"

example2 :: Enc "B64" B.ByteString
example2 = encode "Hello John"

example2L :: Enc "B64" BL.ByteString
example2L = encode "Hello John"