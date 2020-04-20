{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE PartialTypeSignatures #-}

-- | Simple DIY encoding example that 'signs' Text with its length.
--
-- This includes discussion of error handling options. 
--
-- My current thinking: 
--
-- Stronger type level information about encoding provides type safety over decoding process.
-- Decoding cannot fail unless somehow underlying data has been corrupted.
--
-- Such integrity of data should be enforced at boundaries
-- (JSON instances, DB retrievals, etc).  This can be acomplished using provided 'RecreateF' typeclass.

module Examples.DiySignEncoding where

import           Data.Encoding
import qualified Data.Encoding.Instances.Support as EnT

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

-- | Not exposed method that 'encodes' 
encodeSign :: T.Text -> T.Text
encodeSign t = (T.pack . show . T.length $ t) <> ":" <> t


-- | Not exposed method that 'decodes'
-- This method validates the encoding.
-- With type safety in pace, encoding/decoding errors should be only unexpected situations like
-- a hacker somehow figuring out our very secret hash.
--
-- >>> runIdentity $ decodeSign "3:abc" 
-- "abc"
--
-- >>> decodeSign "4:abc" :: Either UnexpectedDecodeEx T.Text
-- Left (UnexpectedDecodeEx "Corrupted Signature")
decodeSign :: (UnexpectedDecodeErr f, Applicative f) => T.Text -> f T.Text
decodeSign t = 
    let (sdit, rest) = T.span isDigit $ t
        actsize = T.length rest - 1
        msize = readMaybe . T.unpack $ sdit
        checkDelimit = T.isInfixOf ":" rest
    in if msize == Just actsize && checkDelimit
       then pure $ T.drop 1 rest
       else unexpectedDecodeErr $ "Corrupted Signature"       


-- | Because encoding function is pure we can create instance of EncodeF 
-- that is polymorphic in effect @f@. This is done using 'EnT.implTranP' combinator.
instance Applicative f => EncodeF f (Enc xs c T.Text) (Enc ("my-sign" ': xs) c T.Text) where
    encodeF = EnT.implTranP encodeSign    

-- | Decoding is effectful to check for tampering with data.
-- Implemenation simply uses 'EnT.implTranF' combinator on the decoding function.
instance (UnexpectedDecodeErr f, Applicative f) => DecodeF f (Enc ("my-sign" ': xs) c T.Text) (Enc xs c T.Text) where
    decodeF = EnT.implTranF decodeSign 

instance (UnexpectedDecodeErr f, Applicative f) => RecreateF f (Enc xs c T.Text) (Enc ("my-sign" ': xs) c T.Text) where   
    checkPrevF = EnT.implTranF decodeSign 

-- | 
-- >>> exEncoded
-- MkEnc Proxy () "11:Hello World"
-- >>> fromEncoding . decodeAll $ exEncoded 
-- "Hello World"
exEncoded :: Enc '["my-sign"] () T.Text
exEncoded = encodeAll . toEncoding () $ "Hello World"

-- | This demonstrates type safety, all 'T.Text' objects are exected to decode 
-- without error after encoding.
--
-- prop> \t -> propEncDec
propEncDec :: T.Text -> Bool
propEncDec t = 
    let enc = encodeAll . toEncoding () $ t :: Enc '["my-sign"] () T.Text
    in t == (fromEncoding . decodeAll $ enc)

-- | Hacker example
-- The data was trasmitted over a network and got corrupted.
--
-- >>> hacker
-- Left (UnexpectedDecodeEx "Corrupted Signature")
hacker :: Either UnexpectedDecodeEx (Enc '["my-sign"] () T.Text)
hacker = 
    let payload = unsafeGetPayload $ exEncoded :: T.Text
        -- | payload is sent over network and get corrupted
        newpay = payload <> " corruption" 
        -- | boundary check recovers the data
        newdata = recreateFAll . toEncoding () $ newpay :: Either UnexpectedDecodeEx (Enc '["my-sign"] () T.Text)
    in newdata    


