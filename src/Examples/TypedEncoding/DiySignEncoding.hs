{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
-- (JSON instances, DB retrievals, etc).  This can be accomplished using provided 'RecreateF' typeclass.
-- 
-- This still is user decision, the errors during decoding process are considered unexpected 'UnexpectedDecodeErr'.
-- In particular user can decide to use unsafe operations with the encoded type. See 'Examples.TypedEncoding.Unsafe'.

module Examples.TypedEncoding.DiySignEncoding where

import           Data.TypedEncoding
import qualified Data.TypedEncoding.Instances.Support as EnT

import           Data.Proxy
import           Data.Functor.Identity
import           GHC.TypeLits

import qualified Data.Text as T
import           Data.Char
import           Data.Semigroup ((<>))
import           Control.Arrow
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
-- MkEnc Proxy () "11:Hello World"
--
-- >>> fromEncoding . decodeAll $ helloSigned 
-- "Hello World"
helloSigned :: Enc '["my-sign"] () T.Text
helloSigned = encodeAll . toEncoding () $ "Hello World"

-- | property checks that 'T.Text' values are exected to decode 
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
-- Right (MkEnc Proxy () "11:Hello World")

prxyMySign = Proxy :: Proxy "my-sign"

-- | Because encoding function is pure we can create instance of EncodeF 
-- that is polymorphic in effect @f@. This is done using 'EnT.implTranP' combinator.
instance Applicative f => EncodeF f (Enc xs c T.Text) (Enc ("my-sign" ': xs) c T.Text) where
    encodeF = EnT.implEncodeP encodeSign    

-- | Decoding is effectful to allow for troubleshooting and unsafe payload changes.
--
-- Implementation simply uses 'EnT.implDecodeF' combinator on the 'asUnexpected' composed with decoding function.
-- 'UnexpectedDecodeErr' has Identity instance allowing for decoding that assumes errors are not possible.
-- For debugging purposes or when unsafe changes to "my-sign" @Error UnexpectedDecodeEx@ instance can be used.
instance (UnexpectedDecodeErr f, Applicative f) => DecodeF f (Enc ("my-sign" ': xs) c T.Text) (Enc xs c T.Text) where
    decodeF = EnT.implDecodeF (asUnexpected prxyMySign . decodeSign) 

-- | Recovery is effectful to check for tampering with data.
-- Implementation simply uses 'EnT.implCheckPrevF' combinator on the recovery function.
instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c T.Text) (Enc ("my-sign" ': xs) c T.Text) where   
    checkPrevF = EnT.implCheckPrevF (asRecreateErr prxyMySign . decodeSign) 
