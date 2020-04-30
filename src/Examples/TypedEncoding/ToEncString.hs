{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


module Examples.TypedEncoding.ToEncString where

import           Data.TypedEncoding
import qualified Data.TypedEncoding.Instances.Support as EnT
import           Data.TypedEncoding.Instances.Restriction.Common
import           Data.TypedEncoding.Instances.ToEncString.Common
import           Data.TypedEncoding.Instances.Enc.Base64
import           Data.TypedEncoding.Instances.Restriction.ASCII
import           Data.TypedEncoding.Instances.Restriction.UTF8

import           Data.Word
import           Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.ByteString as B
import           GHC.TypeLits
import           Data.Proxy
import           Control.Applicative ((<|>))
import           Data.Maybe



-- $setup
-- >>> :set -XMultiParamTypeClasses -XDataKinds -XPolyKinds -XFlexibleInstances -XTypeApplications

-- * IpV4 example

type IpV4 = IpV4F Word8

data IpV4F a = IpV4F {
     oct1 :: a
     , oct2 :: a
     , oct3 :: a
     , oct4 :: a
   } deriving (Show, Functor, Foldable)

tstIp :: IpV4
tstIp = IpV4F 128 1 1 10



-- |
-- >>> toEncString @"r-IPv4" @T.Text tstIp
-- MkEnc Proxy () "128.1.1.10"
instance ToEncString "r-IPv4" T.Text Identity IpV4 where
    toEncStringF = Identity . reduce . map
      where map :: IpV4F Word8 -> IpV4F (Enc '["r-Word8-decimal"] () T.Text) 
            map = fmap toEncString

            reduce :: IpV4F (Enc '["r-Word8-decimal"] () T.Text) -> (Enc '["r-IPv4"] () T.Text) 
            reduce = EnT.foldEncStr () (\a b-> if b == "" then a else a <> "." <> b) 

-- |
-- >>> let enc = toEncString @"r-IPv4" @T.Text tstIp
-- >>> fromEncString @IpV4 enc
-- IpV4F {oct1 = 128, oct2 = 1, oct3 = 1, oct4 = 10}

instance (UnexpectedDecodeErr f, Applicative f) => FromEncString IpV4 f T.Text "r-IPv4" where   
    fromEncStringF = fmap map . unreduce
      where unreduce :: Enc '["r-IPv4"] () T.Text -> f (IpV4F (Enc '["r-Word8-decimal"] () T.Text))
            unreduce = asUnexpected_ @"r-IPv4" . recover . EnT.splitPayload @ '["r-Word8-decimal"] (T.splitOn ".")
            
            map :: IpV4F (Enc '["r-Word8-decimal"] () T.Text) -> IpV4F Word8 
            map = fmap fromEncString

            recover ::  Show a => [a] -> Either String (IpV4F a)
            recover [o1,o2,o3,o4] = pure $ IpV4F o1 o2 o3 o4
            recover x = Left $ "Invalid Content" ++ show x




-- * Simplified email example


-- | Simplified Part header  
type PartHeader = String

-- | Simplified Email header  
type EmailHeader = String

-- | This section shows a type safe processing of emails.
--
-- This over-simplified email type has parts that can binary and have to be Base 64 encoded or 
-- are text that have UTF8 or ASCII character set and can be optionally can be Base 64 encoded.
--
-- The layout of simplified headers is assumed the same as encoding annotations in this library.
data SimplifiedEmailF a = SimplifiedEmailF {
          emailHeader :: EmailHeader
          , parts :: [a]
      } deriving (Show, Eq, Functor, Foldable, Traversable)

type SimplifiedEmail = SimplifiedEmailF (PartHeader, B.ByteString)

type SimplifiedEmailEncB = SimplifiedEmailF (SomeEnc () B.ByteString)

-- TODO
type SimplifiedEmailEncT = SimplifiedEmailF (SomeEnc () T.Text)

tstEmail :: SimplifiedEmail
tstEmail = SimplifiedEmailF {
      emailHeader = "Some Header"
      , parts = [
        ("enc-B64,image", "U29tZSBBU0NJSSBUZXh0") 
        , ("enc-B64,r-ASCII", "U29tZSBBU0NJSSBUZXh0")
        , ("enc-B64,r-UTF8", "U29tZSBVVEY4IFRleHQ=") 
        , ("r-ASCII", "Some ASCII plain text") 
         ]
  }

-- | 
-- Encodes 'simple email' 
-- 
-- This uses 'unsafeSomeEnc' for simplicity 
recreateEncoding :: SimplifiedEmail -> Either RecreateEx SimplifiedEmailEncB
recreateEncoding = mapM encodefn
  where 
        -- | simplified parse header assumes email has the same layout as encodings
        -- image is ingored, since [enc-B64] annotation on ByteString permits base 64
        -- encoded bytes
        parseHeader :: PartHeader -> SomeAnn
        parseHeader "enc-B64,image" = "[enc-B64]" 
        parseHeader x = "[" ++ x ++ "]"

        encodefn :: (PartHeader, B.ByteString) -> Either RecreateEx (SomeEnc () B.ByteString)
        encodefn (parth, body) = 
          let unchecked = toUnchecked (parseHeader parth) () body
              try1 = (fmap (fmap toSomeEnc)) . EnT.verifyUnchecked' @'["enc-B64","r-UTF8"] $ unchecked
              try2 = (fmap (fmap toSomeEnc)) . EnT.verifyUnchecked' @'["enc-B64","r-ASCII"] $ unchecked
              try3 = (fmap (fmap toSomeEnc)) . EnT.verifyUnchecked' @'["r-ASCII"] $ unchecked
              try4 = (fmap (fmap toSomeEnc)) . EnT.verifyUnchecked' @'["r-UTF8"] $ unchecked
              try5 = (fmap (fmap toSomeEnc)) . EnT.verifyUnchecked' @'["enc-B64"] $ unchecked
              def =  Left $ recreateErrUnknown ("Invalid Header " ++ parth)
          in def `fromMaybe` (try1 <|> try2 <|> try3 <|> try4 <|> try5)    
 


-- | Decode email base 64 encoded text entries but not image entries
-- this provides a type safety over not decoding certain parts of email
--
-- decodeB64ForTextOnly <$> recreateEncoding tstEmail
--
decodeB64ForTextOnly :: SimplifiedEmailEncB -> SimplifiedEmailEncB
decodeB64ForTextOnly = fmap (EnT.maybeTranformations [tryUtf8, tryAscii]) 
  where
    tryUtf8, tryAscii :: SomeEnc c B.ByteString -> Maybe (SomeEnc c B.ByteString)
    tryUtf8 = fmap (toSomeEnc . decodeToUtf8) . fromSomeEnc @ '["enc-B64", "r-UTF8"] 
    tryAscii = fmap (toSomeEnc . decodeToAscii) . fromSomeEnc @ '["enc-B64", "r-ASCII"] 
 
    decodeToUtf8 :: Enc '["enc-B64", "r-UTF8"] c B.ByteString -> _
    decodeToUtf8 = decodePart @'["enc-B64"]

    decodeToAscii :: Enc '["enc-B64", "r-ASCII"] c B.ByteString -> _
    decodeToAscii = decodePart @'["enc-B64"]








-- * Helper instances

instance Displ a => Displ (IpV4F a) where
    displ = show . fmap displ

instance Displ a => Displ (SimplifiedEmailF a) where
    displ = show . fmap displ    
