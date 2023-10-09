{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Common /restriction/ "r-" instances
--
-- Renamed from @Data.TypedEncoding.Instances.Restriction.Common@ (v0.3)
--
-- @since 0.2.0.0
module Data.TypedEncoding.Instances.Restriction.Misc where

import           Data.Word
-- import           Data.Functor.Identity
import           Data.String
import           Data.Proxy
import           Text.Read

import           Data.TypedEncoding.Common.Class.IsStringR
import           Data.TypedEncoding.Instances.Support

-- $setup
-- >>> import qualified Data.Text as T


instance (IsStringR str) =>  Encode (Either EncodeEx) "r-Word8-decimal" "r-Word8-decimal" c str where
    encoding = encWord8Dec
instance (Applicative f) => Decode f "r-Word8-decimal" "r-Word8-decimal" c str where
    decoding = decAnyR
instance (IsStringR str) =>  Validate (Either RecreateEx) "r-Word8-decimal" "r-Word8-decimal" c str where
    validation = validR encWord8Dec
instance (IsString str, Applicative f) => ToEncString f "r-Word8-decimal" "r-Word8-decimal" Word8 str where
    toEncF  i = pure $ UnsafeMkEnc Proxy () (fromString . show $ i)
instance (IsStringR str, UnexpectedDecodeErr f, Applicative f) => FromEncString f "r-Word8-decimal" "r-Word8-decimal" Word8 str where
    fromEncF  = asUnexpected @"r-Word8-decimal" . readEither . toString . getPayload

encWord8Dec :: (IsStringR str) => Encoding (Either EncodeEx) "r-Word8-decimal" "r-Word8-decimal" c str
encWord8Dec = _implEncodingEx (verifyWithRead @Word8 "Word8-decimal")


instance (IsStringR str) =>  Encode (Either EncodeEx) "r-Int-decimal" "r-Int-decimal" c str where
    encoding = encIntDec
instance (Applicative f) => Decode f "r-Int-decimal" "r-Int-decimal" c str where
    decoding = decAnyR
instance (IsStringR str) =>  Validate (Either RecreateEx) "r-Int-decimal" "r-Int-decimal" c str where
    validation = validR encIntDec
instance (IsString str, Applicative f) => ToEncString f "r-Int-decimal" "r-Int-decimal" Int str where
    toEncF  i = pure $ UnsafeMkEnc Proxy () (fromString . show $ i)

encIntDec :: (IsStringR str) => Encoding (Either EncodeEx) "r-Int-decimal" "r-Int-decimal" c str
encIntDec =  _implEncodingEx (verifyWithRead @Int "Int-decimal")





-- All instances of "r-Word8-decimal" are  @Show@ / @Read@ based
