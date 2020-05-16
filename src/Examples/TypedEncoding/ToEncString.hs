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

-- |
-- This module shows use of 'ToEncString' and 'FromEncString'
-- and demonstrates /composite/ encoding.
--
-- @Show@ and @Read@ classes use a very permissive String type. This often results in 
-- read errors. type-encoding approach provides type safety over decoding process.
--
-- This module includes a simplified email example. This is a non-homogeneous case, 
-- email parts do not have the same encoding. 
--
-- Examples here could be made more type safe with use of dependently typed
-- concepts like @Vect@, @HList@ or variant equivalents of these types.
--
-- Current version of typed-encoding does not have dependencies on such types. 
--
-- These examples use 'CheckedEnc' when untyped version of 'Enc' is needed.
-- Alternatively, an existentially quantified 'SomeEnc' type could have been used.
-- Both are isomorphic.  
module Examples.TypedEncoding.ToEncString where

import           Data.TypedEncoding
import qualified Data.TypedEncoding.Instances.Support as EnT
import           Data.TypedEncoding.Instances.Restriction.Common ()
import           Data.TypedEncoding.Instances.ToEncString.Common ()
import           Data.TypedEncoding.Instances.Enc.Base64 ()
import           Data.TypedEncoding.Instances.Restriction.ASCII ()
import           Data.TypedEncoding.Instances.Restriction.UTF8 ()

import           Data.Word
import           Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.ByteString as B
import           Control.Applicative -- ((<|>))
import           Data.Maybe
import           Data.Semigroup ((<>))



-- $setup
-- >>> :set -XMultiParamTypeClasses -XDataKinds -XPolyKinds -XFlexibleInstances -XTypeApplications -XOverloadedStrings
-- >>> import qualified Data.List as L


-- * IpV4 example

type IpV4 = IpV4F Word8

-- | 
-- In this example all data fields have the same type. 
-- This simplifies encoding work as all fields will be encoded the same way.
-- We use IP address since all fields are single byte size.
data IpV4F a = IpV4F {
     oct1 :: a
     , oct2 :: a
     , oct3 :: a
     , oct4 :: a
   } deriving (Show, Functor, Foldable)

tstIp :: IpV4
tstIp = IpV4F 128 1 1 10



-- |
-- In this example @toEncString@ converts 'IpV4' to @Enc '["r-IPv4"] Text@.
--  
-- This is done with help of existing @"r-Word8-decimal"@ annotation defined
-- in "Data.TypedEncoding.Instances.Restriction.Common"
--
-- >>> toEncString @"r-IPv4" @T.Text tstIp
-- MkEnc Proxy () "128.1.1.10"
--
-- Implementation is a classic map reduce where reduce is done with help of
-- 'EnT.foldEncStr'
--
-- >>> let fn a b = if b == "" then a else a <> "." <> b
-- >>> let reduce = EnT.foldEncStr @'["r-IPv4"] @'["r-Word8-decimal"] () fn
-- >>>  displ . reduce . fmap toEncString $ tstIp
-- "MkEnc '[r-IPv4] () (String 128.1.1.10)"
--
-- Note lack of type safety here, the same code would work just fine if we added
-- 5th field to 'IpV4F' constructor.  
--
-- Using something like a dependently typed
--
-- @
-- Vect 4 (Enc '["r-Word8-decimal"] () T.Text)
-- @ 
-- 
-- would have improved this situation.
-- @HList@ could be used for record types with heterogeneous fields.
--
-- Currently, 'type-encoding' library does not have these types in scope.  
instance ToEncString "r-IPv4" T.Text Identity IpV4 where
    toEncStringF = Identity . reduce . map
      where map :: IpV4F Word8 -> IpV4F (Enc '["r-Word8-decimal"] () T.Text) 
            map = fmap toEncString

            reduce :: IpV4F (Enc '["r-Word8-decimal"] () T.Text) -> Enc '["r-IPv4"] () T.Text 
            reduce = EnT.foldEncStr () (\a b-> if b == "" then a else a <> "." <> b) 

-- |
--
-- >>> let enc = toEncString @"r-IPv4" @T.Text tstIp
-- >>> fromEncString @IpV4 enc
-- IpV4F {oct1 = 128, oct2 = 1, oct3 = 1, oct4 = 10}
--
-- To get 'IpV4' out of the string we need to reverse previous @reduce@.
-- This is currently done using helper 'EnT.splitPayload' combinator. 
--
-- >>> EnT.splitPayload @ '["r-Word8-decimal"] (T.splitOn $ T.pack ".") $ enc 
-- [MkEnc Proxy () "128",MkEnc Proxy () "1",MkEnc Proxy () "1",MkEnc Proxy () "10"]
-- 
-- The conversion of a list to IpV4F needs handle errors but these errors 
-- are considered unexpected.
--
-- Note, again, the error condition exposed by this implementation could have been avoided
-- if 'EnT.splitPayload' returned fixed size @Vect 4@.
instance (UnexpectedDecodeErr f, Applicative f) => FromEncString IpV4 f T.Text "r-IPv4" where   
    fromEncStringF = fmap map . unreduce
      where unreduce :: Enc '["r-IPv4"] () T.Text -> f (IpV4F (Enc '["r-Word8-decimal"] () T.Text))
            unreduce = asUnexpected @"r-IPv4" . recover . EnT.splitPayload @ '["r-Word8-decimal"] (T.splitOn ".")
            
            map :: IpV4F (Enc '["r-Word8-decimal"] () T.Text) -> IpV4F Word8 
            map = fmap fromEncString

            recover ::  Show a => [a] -> Either String (IpV4F a)
            recover [o1,o2,o3,o4] = pure $ IpV4F o1 o2 o3 o4
            recover x = Left $ "Invalid Content" ++ show x




-- * Simplified email example


-- | Simplified Part header  
type PartHeader = [String]

-- | Simplified Email header  
type EmailHeader = String

-- | This section shows a type safe processing of emails.
--
-- 'SimplifiedEmailF' is an over-simplified email type, it has parts that can be either 
-- 
-- * binary and have to be Base 64 encoded or 
-- * are text that have either UTF8 or ASCII character set 
--
-- The text parts can be optionally can be Base 64 encoded but do not have to be.
--
-- For simplicity, the layout of simplified headers is assumed the same as encoding annotations in this library.
data SimplifiedEmailF a = SimplifiedEmailF {
          emailHeader :: EmailHeader
          , parts :: [a]
      } deriving (Show, Eq, Functor, Foldable, Traversable)

type SimplifiedEmail = SimplifiedEmailF (PartHeader, B.ByteString)

type SimplifiedEmailEncB = SimplifiedEmailF (CheckedEnc () B.ByteString)


-- | @tstEmail@ contains some simple data to play with
tstEmail :: SimplifiedEmail
tstEmail = SimplifiedEmailF {
      emailHeader = "Some Header"
      , parts = [
        (["enc-B64","image"], "U29tZSBBU0NJSSBUZXh0") 
        , (["enc-B64","r-ASCII"], "U29tZSBBU0NJSSBUZXh0")
        , (["enc-B64","r-UTF8"], "U29tZSBVVEY4IFRleHQ=") 
        , (["r-ASCII"], "Some ASCII plain text") 
         ]
  }

-- | 
-- This example encodes fields in 'SimplifiedEmailF' into an untyped version of @Enc@ which 
-- stores verified encoded data and encoding information is stored at the value level: 
-- @CheckedEnc () B.ByteString@.
-- 
-- Part of email are first converted to 'UncheckedEnc' (that stores encoding information at the value level as well).
-- 'UncheckedEnc'  that can easily represent parts of the email
--
-- >>> let part = parts tstEmail L.!! 2
-- >>> part
-- (["enc-B64","r-UTF8"],"U29tZSBVVEY4IFRleHQ=")
-- >>> let unchecked = toUncheckedEnc (fst part) () (snd part)
-- >>> unchecked 
-- MkUncheckedEnc ["enc-B64","r-UTF8"] () "U29tZSBVVEY4IFRleHQ="
--
-- We can play 'Alternative' ('<|>') game (we acually use @Maybe@) with final option being a 'RecreateEx' error:
--
-- >>> verifyUncheckedEnc' @'["enc-B64","r-ASCII"] $ unchecked
-- Nothing
-- >>> verifyUncheckedEnc' @'["enc-B64","r-UTF8"] $ unchecked
-- Just (Right (MkEnc Proxy () "U29tZSBVVEY4IFRleHQ="))
--
-- Since the data is heterogeneous (each piece has a different encoding annotation), we need wrap the result in another plain ADT: 'CheckedEnc'.
-- 
-- 'CheckedEnc' is similar to 'UncheckedEnc' with the difference that the only (safe) way to get values of this type is
-- from properly encoded 'Enc' values. 
--
-- Using 'unsafeCheckedEnc' would break type safety here. 
-- 
-- It is important to handle all cases during encoding so decoding errors become impossible.
--
-- Again, use of dependently typed variant types that could enumerate all possible encodings
-- would made this code nicer.
recreateEncoding :: SimplifiedEmail -> Either RecreateEx SimplifiedEmailEncB
recreateEncoding = mapM encodefn
  where 
        -- | simplified parse header assumes email has the same layout as encodings
        -- image is ingored, since [enc-B64] annotation on ByteString permits base 64
        -- encoded bytes
        parseHeader :: PartHeader -> [EncAnn]
        parseHeader ["enc-B64","image"] = ["enc-B64"] 
        parseHeader x = x

        encodefn :: (PartHeader, B.ByteString) -> Either RecreateEx (CheckedEnc () B.ByteString)
        encodefn (parth, body) = 
          runAlternatives' (fromMaybe def) [try1, try2, try3, try4, try5] body
          where
              unchecked = toUncheckedEnc (parseHeader parth) () 
              try1 = fmap (fmap toCheckedEnc) . verifyUncheckedEnc' @'["enc-B64","r-UTF8"] . unchecked
              try2 = fmap (fmap toCheckedEnc) . verifyUncheckedEnc' @'["enc-B64","r-ASCII"] . unchecked
              try3 = fmap (fmap toCheckedEnc) . verifyUncheckedEnc' @'["r-ASCII"] . unchecked
              try4 = fmap (fmap toCheckedEnc) . verifyUncheckedEnc' @'["r-UTF8"] . unchecked
              try5 = fmap (fmap toCheckedEnc) . verifyUncheckedEnc' @'["enc-B64"] . unchecked
              def =  Left $ recreateErrUnknown ("Invalid Header " ++ show parth) 


-- | 
-- Example decodes parts of email that are base 64 encoded text and nothing else.
--
-- This provides a type safety assurance that we do not decode certain parts of email
-- (like trying to decode base 64 on a plain text part).
--
-- >>> decodeB64ForTextOnly <$> recreateEncoding tstEmail
-- Right (SimplifiedEmailF {emailHeader = "Some Header", parts = [MkCheckedEnc ["enc-B64"] () "U29tZSBBU0NJSSBUZXh0",MkCheckedEnc ["r-ASCII"] () "Some ASCII Text",MkCheckedEnc ["r-UTF8"] () "Some UTF8 Text",MkCheckedEnc ["r-ASCII"] () "Some ASCII plain text"]})
--
-- Combinator @fromCheckedEnc \@'["enc-B64", "r-UTF8"]@ acts as a selector and picks only the
-- @["enc-B64", "r-UTF8"]@ values from our 'Traversable' type. 
--
-- We play the ('<|>') game on all the selectors we want picking and decoding right pieces only.
--
-- Imagine this is one of the pieces:
--
-- >>> let piece = unsafeCheckedEnc ["enc-B64","r-ASCII"] () ("U29tZSBBU0NJSSBUZXh0" :: B.ByteString)
-- >>> displ piece
-- "MkCheckedEnc [enc-B64,r-ASCII] () (ByteString U29tZSBBU0NJSSBUZXh0)"
--
-- This code will not pick it up:
--
-- >>> fromCheckedEnc @ '["enc-B64", "r-UTF8"] $ piece
-- Nothing
--
-- But this one will:
--
-- >>> fromCheckedEnc @ '["enc-B64", "r-ASCII"]  $ piece
-- Just (MkEnc Proxy () "U29tZSBBU0NJSSBUZXh0")
--
-- so we can apply the decoding on the selected piece 
--
-- >>> fmap (toCheckedEnc . decodePart @'["enc-B64"]) . fromCheckedEnc @ '["enc-B64", "r-ASCII"] $ piece
-- Just (MkCheckedEnc ["r-ASCII"] () "Some ASCII Text")

decodeB64ForTextOnly :: SimplifiedEmailEncB -> SimplifiedEmailEncB
decodeB64ForTextOnly = fmap (runAlternatives fromMaybe [tryUtf8, tryAscii]) 
  where
    tryUtf8, tryAscii :: CheckedEnc c B.ByteString -> Maybe (CheckedEnc c B.ByteString)
    tryUtf8 = fmap (toCheckedEnc . decodeToUtf8) . fromCheckedEnc @ '["enc-B64", "r-UTF8"] 
    tryAscii = fmap (toCheckedEnc . decodeToAscii) . fromCheckedEnc @ '["enc-B64", "r-ASCII"] 
 
    decodeToUtf8 :: Enc '["enc-B64", "r-UTF8"] c B.ByteString -> _
    decodeToUtf8 = decodePart @'["enc-B64"]

    decodeToAscii :: Enc '["enc-B64", "r-ASCII"] c B.ByteString -> _
    decodeToAscii = decodePart @'["enc-B64"]


-- * Helpers

-- | Provides easy to read encoding information
instance Displ a => Displ (IpV4F a) where
    displ = show . fmap displ

-- | Provides easy to read encoding information
instance Displ a => Displ (SimplifiedEmailF a) where
    displ = show . fmap displ    

runAlternatives' :: Alternative f => (f b -> b) -> [a -> f b] -> a -> b
runAlternatives' defF fns = defF . alternatives fns

runAlternatives :: Alternative f => (a -> f b -> b) -> [a -> f b] -> a -> b
runAlternatives defF fns a = defF a . alternatives fns $ a

alternatives :: Alternative f => [a -> f b] -> a -> f b
alternatives fns a = foldr ((<|>) . ($ a)) empty fns
