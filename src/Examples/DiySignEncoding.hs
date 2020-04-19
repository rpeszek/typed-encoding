{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE PartialTypeSignatures #-}

-- | Simple DIY encoding example that 'signs' Text with its length
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

-- | Not exposed method that 'encodes' 
encodeSign :: T.Text -> T.Text
encodeSign t = (T.pack . show . T.length $ t) <> ":" <> t


-- | Not exposed method that 'decodes'
-- This method validates the encoding.
-- Unless a hacker somehow figures out our very secret hash, we will know our data is corrupted! 
-- (See also 'Examples.ErrorHandling' discussion) 
--
-- >>> decodeSign "3:abc"
-- Right "abc"
-- >>> decodeSign "4:abc"
-- Left CorruptedSignature
decodeSign :: T.Text -> Either CorruptedSignature T.Text
decodeSign t = 
    let (sdit, rest) = T.span isDigit $ t
        actsize = T.length rest - 1
        msize = readMaybe . T.unpack $ sdit
        checkDelimit = T.isInfixOf ":" rest
    in if msize == Just actsize && checkDelimit
       then Right $ T.drop 1 rest
       else Left CorruptedSignature         

-- | Exception type used in this example
data CorruptedSignature = CorruptedSignature deriving Show

-- | Because encoding function is pure we can create instance of EncodeF 
-- that is polymorphic in effect @f@. This is done using 'EnT.implTranP' combinator.
instance Applicative f => EncodeF f (Enc xs c T.Text) (Enc ("my-sign" ': xs) c T.Text) where
    encodeF = EnT.implTranP encodeSign    

-- | Decoding is effectful to check for tampering with data.
-- Implemenation simply uses 'EnT.implTranF' combinator on the decoding function.
instance DecodeF (Either CorruptedSignature) (Enc ("my-sign" ': xs) c T.Text) (Enc xs c T.Text) where
    decodeF = EnT.implTranF decodeSign 

-- | example usage
-- 
-- >>> exEncoded
-- MkEnc Proxy () "11:Hello World"
-- >>> fmap fromEncoding . decodeFAll $ exEncoded :: Either CorruptedSignature T.Text
-- Right "Hello World"
exEncoded :: Enc '["my-sign"] () T.Text
exEncoded = encodeAll . toEncoding () $ "Hello World"






