{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
--{-# LANGUAGE TypeApplications #-}

-- | 'r-B64' is restricted to values that are valid Base64 encodings of some data.
-- For example, @Enc '["r-B64"] () T.Text@ can contain encoded binary image.
--
-- "enc-B64" can be converted to "r-B64" using @flattenAs@ defined in
-- 'Data.TypedEncoding.Instances.Enc.Base64'.     
-- However, there is no, and there should be no conversion general conversion from "r-B64" back to "enc-B64":
-- @Enc '["r-B64"] () T.Text@ is not B64 encoded text, it is B64 encoded something.
--
-- @since 0.5.1.0
module Data.TypedEncoding.Instances.Restriction.Base64 where

import           Data.TypedEncoding.Instances.Support


import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE 
import qualified Data.Text.Lazy.Encoding as TEL 
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as BL64
import qualified Data.ByteString.Char8 as B8
import qualified Data.TypedEncoding.Instances.Restriction.ASCII as RAscii

-- $setup
-- >>> :set -XScopedTypeVariables -XKindSignatures -XMultiParamTypeClasses -XDataKinds -XPolyKinds -XPartialTypeSignatures -XFlexibleInstances -XTypeApplications



instance Encode (Either EncodeEx) "r-B64" "r-B64" c B.ByteString where
    encoding = encRB64B
  
instance Encode (Either EncodeEx) "r-B64" "r-B64" c BL.ByteString where
    encoding = encRB64BL
  

instance Encode (Either EncodeEx) "r-B64" "r-B64" c T.Text where
    encoding = encRB64T
  
instance Encode (Either EncodeEx) "r-B64" "r-B64" c TL.Text where
    encoding = encRB64TL

instance Encode (Either EncodeEx) "r-B64" "r-B64" c String where
    encoding = encRB64S

-- using lazy decoding to detect errors seems to be the fastest option that is not super hard to code


encRB64B :: Encoding (Either EncodeEx) "r-B64" "r-B64" c B.ByteString
encRB64B = _implEncodingEx (implVerifyR (B64.decode)) 

encRB64BL :: Encoding (Either EncodeEx) "r-B64" "r-B64" c BL.ByteString
encRB64BL = _implEncodingEx (implVerifyR (BL64.decode)) 

-- | Converts text to bytestring using UTF8 decoding and then verify encoding in ByteString
-- This is safe without verifying ASCII, any non-ASCII text will still convert to ByteString
-- but will fail B64.decode (TODO tests would be nice)
encRB64T :: Encoding (Either EncodeEx) "r-B64" "r-B64" c T.Text
encRB64T = _implEncodingEx (implVerifyR (B64.decode . TE.encodeUtf8)) 

encRB64TL :: Encoding (Either EncodeEx) "r-B64" "r-B64" c TL.Text
encRB64TL = _implEncodingEx (implVerifyR (BL64.decode . TEL.encodeUtf8)) 

encRB64S :: Encoding (Either EncodeEx) "r-B64" "r-B64" c String
encRB64S = _implEncodingEx (implVerifyR (fmap (B64.decode . B8.pack) . either (Left . show) Right . RAscii.encImpl)) 

-- -- * Decoding

instance (Applicative f) => Decode f "r-B64" "r-B64" c str where
    decoding = decAnyR

instance (RecreateErr f, Applicative f) =>  Validate f "r-B64" "r-B64" c B.ByteString  where
    validation = validR encRB64B

instance (RecreateErr f, Applicative f) =>  Validate f "r-B64" "r-B64" c BL.ByteString  where
    validation = validR encRB64BL

instance (RecreateErr f, Applicative f) =>  Validate f "r-B64" "r-B64" c T.Text  where
    validation = validR encRB64T

instance (RecreateErr f, Applicative f) =>  Validate f "r-B64" "r-B64" c TL.Text  where
    validation = validR encRB64TL

instance (RecreateErr f, Applicative f) =>  Validate f "r-B64" "r-B64" c String  where
    validation = validR encRB64S

