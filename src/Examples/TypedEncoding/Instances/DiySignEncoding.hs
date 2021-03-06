{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- {-# LANGUAGE PartialTypeSignatures #-}

-- | Simple DIY encoding example that "signs" Text with its length.
--
-- Documentation includes discussion of error handling options. 
--
-- My current thinking: 
--
-- Stronger type level information about encoding provides type safety over decoding process.
-- Decoding cannot fail unless somehow underlying data has been corrupted.
--
-- Such integrity of data should be enforced at boundaries
-- (JSON instances, DB retrievals, etc).  This can be accomplished using provided support for /Validation/ or using 'Data.TypedEncoding.Common.Types.UncheckedEnc.UncheckedEnc'.
-- 
-- This still is user decision, the errors during decoding process are considered unexpected 'UnexpectedDecodeErr'.
-- In particular user can decide to use unsafe operations with the encoded type. See 'Examples.TypedEncoding.Unsafe'.

module Examples.TypedEncoding.Instances.DiySignEncoding where

import           Data.TypedEncoding
import qualified Data.TypedEncoding.Instances.Support as EnT

import qualified Data.Text as T
import           Data.Char
import           Data.Semigroup ((<>))
import           Text.Read (readMaybe)

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds
-- >>> import Test.QuickCheck.Instances.Text()

-- | encoding function, typically should be module private 
encodeSign :: T.Text -> T.Text
encodeSign t = (T.pack . show . T.length $ t) <> ":" <> t


-- | dual purpose decoding and recovery function.
--
-- This typically should be module private.
--
-- >>> decodeSign "3:abc" 
-- Right "abc"
--
-- >>> decodeSign "4:abc" 
-- Left "Corrupted Signature"
decodeSign :: T.Text -> Either String T.Text
decodeSign t = 
    let (sdit, rest) = T.span isDigit $ t
        actsize = T.length rest - 1
        msize = readMaybe . T.unpack $ sdit
        checkDelimit = T.isInfixOf ":" rest
    in if msize == Just actsize && checkDelimit
       then Right $ T.drop 1 rest
       else Left $ "Corrupted Signature"       


-- | Encoded hello world example.
--
-- >>> helloSigned
-- UnsafeMkEnc Proxy () "11:Hello World"
--
-- >>> fromEncoding . decodeAll $ helloSigned 
-- "Hello World"
helloSigned :: Enc '["my-sign"] () T.Text
helloSigned = encodeAll . toEncoding () $ "Hello World"

-- | property checks that 'T.Text' values are expected to decode 
-- without error after encoding.
--
-- prop> \t -> propEncDec
propEncDec :: T.Text -> Bool
propEncDec t = 
    let enc = encodeAll . toEncoding () $ t :: Enc '["my-sign"] () T.Text
    in t == (fromEncoding . decodeAll $ enc)

hacker :: Either RecreateEx (Enc '["my-sign"] () T.Text)
hacker = 
    let payload = getPayload $ helloSigned :: T.Text
        -- | payload is sent over network and get corrupted
        newpay = payload <> " corruption" 
        -- | boundary check recovers the data
        newdata = recreateFAll . toEncoding () $ newpay :: Either RecreateEx (Enc '["my-sign"] () T.Text)
    in newdata    
-- ^ Hacker example
-- The data was transmitted over a network and got corrupted.
--
-- >>> let payload = getPayload $ helloSigned :: T.Text
-- >>> let newpay = payload <> " corruption" 
-- >>> recreateFAll . toEncoding () $ newpay :: Either RecreateEx (Enc '["my-sign"] () T.Text)
-- Left (RecreateEx "my-sign" ("Corrupted Signature"))
--
-- >>> recreateFAll . toEncoding () $ payload :: Either RecreateEx (Enc '["my-sign"] () T.Text)
-- Right (UnsafeMkEnc Proxy () "11:Hello World")


-- | Because encoding function is pure we can create instance of 'Encode' 
-- that is polymorphic in effect @f@. 
--
-- This is done using 'EnT.implTranP' combinator.
instance Applicative f => Encode f "my-sign" "my-sign" c T.Text where
   encoding = EnT._implEncodingP encodeSign    

-- | Decoding allows effectful @f@ to allow for troubleshooting and unsafe payload changes.
--
-- Implementation simply uses 'EnT.implDecodingF' combinator on the 'asUnexpected' composed with decoding function.
--
-- 'UnexpectedDecodeErr' has Identity instance allowing for decoding that assumes errors are not possible.
--
-- For debugging purposes or when unsafe changes to "my-sign" @Error UnexpectedDecodeEx@ instance can be used.
instance (UnexpectedDecodeErr f, Applicative f) => Decode f "my-sign" "my-sign" c T.Text where
    decoding = decMySign 

decMySign :: (UnexpectedDecodeErr f, Applicative f) => Decoding f "my-sign" "my-sign" c T.Text
decMySign = EnT.implDecodingF (asUnexpected @"my-sign" . decodeSign) 

-- | Recreation allows effectful @f@ to check for tampering with data.
--
-- Implementation simply uses 'EnT.validFromDec' combinator on the recovery function.
instance (RecreateErr f, Applicative f) => Validate f "my-sign" "my-sign" c T.Text where
    validation = EnT.validFromDec decMySign

