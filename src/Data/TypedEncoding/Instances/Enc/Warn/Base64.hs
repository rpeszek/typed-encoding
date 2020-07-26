{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Defines /Base64/ encoding for Text
-- This version has poor performance.
--
-- @since 0.5.0.0
module Data.TypedEncoding.Instances.Enc.Warn.Base64 {-# WARNING "Not optimized for performance" #-} where

import           Data.TypedEncoding
import           Data.TypedEncoding.Instances.Support

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE 
import qualified Data.Text.Lazy.Encoding as TEL 

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as BL64

-- | This instance will likely be removed in future versions (performance concerns)
-- (Moved from "Data.TypedEncoding.Instances.Enc.Base64")

-- @since 0.3.0.0
instance Applicative f => Encode f "enc-B64" "enc-B64" c T.Text where
    encoding = endB64T

-- | This function will likely be removed in future versions (performance concerns)
-- (Moved from "Data.TypedEncoding.Instances.Enc.Base64")
--
-- @since 0.3.0.0
endB64T :: Applicative f => Encoding f "enc-B64" "enc-B64" c T.Text
endB64T = _implEncodingP (TE.decodeUtf8 . B64.encode . TE.encodeUtf8)  


-- | 
--
-- @since 0.3.0.0
instance (UnexpectedDecodeErr f, Applicative f) => Decode f "enc-B64" "enc-B64" c T.Text where
    decoding = decB64T

-- |
-- @since 0.3.0.0 
decB64T :: (UnexpectedDecodeErr f, Applicative f) => Decoding f "enc-B64" "enc-B64" c T.Text
decB64T = _implDecodingF (asUnexpected @"enc-B64"  . fmap TE.decodeUtf8 . B64.decode . TE.encodeUtf8) 

-- | 
--
-- @since 0.3.0.0 
instance (UnexpectedDecodeErr f, Applicative f) => Decode f "enc-B64" "enc-B64" c TL.Text where
    decoding = decB64TL

-- |
-- @since 0.3.0.0 
decB64TL :: (UnexpectedDecodeErr f, Applicative f) => Decoding f "enc-B64" "enc-B64" c TL.Text
decB64TL = _implDecodingF (asUnexpected @"enc-B64"  . fmap TEL.decodeUtf8 . BL64.decode . TEL.encodeUtf8) 

-- |
-- @since 0.3.0.0 
instance (RecreateErr f, Applicative f) => Validate f "enc-B64" "enc-B64" c T.Text where
    validation = validFromDec decB64T

-- |
-- @since 0.3.0.0 
instance (RecreateErr f, Applicative f) => Validate f "enc-B64" "enc-B64" c TL.Text where
    validation = validFromDec decB64TL

