{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
 

module Examples.TypedEncoding.ToEncString where

import           Data.TypedEncoding
import qualified Data.TypedEncoding.Instances.Support as EnT
import           Data.TypedEncoding.Instances.Restriction.Common
import           Data.TypedEncoding.Instances.ToEncString.Common

import           Data.Word
import qualified Data.Text as T
import           GHC.TypeLits

data IpV4 a = IpV4 {
     oct1 :: a
     , oct2 :: a
     , oct3 :: a
     , oct4 :: a
   } deriving (Show, Functor, Foldable)

tstIp :: IpV4 Word8
tstIp = IpV4 128 1 1 10

instance Displ a => Displ (IpV4 a) where
    displ = show . fmap displ

-- applyToEncString :: IpV4 Word8 -> IpV4 (Enc '["r-Word8-decimal"] () T.Text) 
-- applyToEncString = fmap toEncString

-- toEnc ::  IpV4 (Enc '["r-Word8-decimal"] () T.Text) -> (Enc '["r-IPv4"] () T.Text) 
-- toEnc x = EnT.foldEnc () (\a b-> if b == "" then a else a <> "." <> b) ""  $ x


instance ToEncString "r-IPv4" T.Text (IpV4 Word8) where
    toEncString = reduce . map
      where map :: IpV4 Word8 -> IpV4 (Enc '["r-Word8-decimal"] () T.Text) 
            map = fmap toEncString
            reduce :: IpV4 (Enc '["r-Word8-decimal"] () T.Text) -> (Enc '["r-IPv4"] () T.Text) 
            reduce = EnT.foldEnc' () (\a b-> if b == "" then a else a <> "." <> b) 

-- toEncString tstIp :: Enc '["r-IPv4"] () T.Text                                                                        