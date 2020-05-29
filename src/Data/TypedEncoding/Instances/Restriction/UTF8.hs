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
{-# LANGUAGE BinaryLiterals #-}

-- | 'UTF-8' encoding
--
-- @since 0.1.0.0
module Data.TypedEncoding.Instances.Restriction.UTF8 where

import           Data.TypedEncoding.Instances.Support

import           Data.Proxy

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE 
import qualified Data.Text.Lazy.Encoding as TEL 
import           Data.Either

import           Data.Bits
import           Data.Word
import           Data.Text.Internal.Encoding.Utf8
import qualified Data.Text as T

import Numeric (showHex, showIntAtBase)

import Data.Char (intToDigit)

-- $setup
-- >>> :set -XBinaryLiterals -XOverloadedStrings -XScopedTypeVariables -XKindSignatures -XMultiParamTypeClasses -XDataKinds -XPolyKinds -XPartialTypeSignatures -XFlexibleInstances -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Instances.Text()
-- >>> import Test.QuickCheck.Instances.ByteString()
-- >>> import Data.TypedEncoding
-- >>> import Data.TypedEncoding.Internal.Util (proxiedId)
-- >>> let emptyUTF8B = unsafeSetPayload () "" ::  Enc '["r-UTF8"] () B.ByteString 
-- >>> :{  
-- instance Arbitrary (Enc '["r-UTF8"] () B.ByteString) where 
--      arbitrary =  fmap (fromRight emptyUTF8B) 
--                   . flip suchThat isRight 
--                   . fmap (encodeFAll @'["r-UTF8"] @(Either EncodeEx) @(). toEncoding ()) $ arbitrary 
-- :}



-----------------
-- Encodings  --
-----------------

prxyUtf8 = Proxy :: Proxy "r-UTF8"

-- TODO these may need rethinking (performance)

-- | UTF8 encodings are defined for ByteString only as that would not make much sense for Text
--
-- >>> encodeFAll . toEncoding () $ "\xc3\xb1" :: Either EncodeEx (Enc '["r-UTF8"] () B.ByteString)
-- Right (UnsafeMkEnc Proxy () "\195\177")
--
-- >>> encodeFAll . toEncoding () $ "\xc3\x28" :: Either EncodeEx (Enc '["r-UTF8"] () B.ByteString)
-- Left (EncodeEx "r-UTF8" (Cannot decode byte '\xc3': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream))
--
-- Following test uses 'verEncoding' helper that checks that bytes are encoded as Right iff they are valid UTF8 bytes
--
-- >>> :{ 
-- quickCheck $ \(b :: B.ByteString) -> verEncoding b $ fmap (
--          fromEncoding 
--          . decodeAll @'["r-UTF8"]
--          ) . encodeFAll @'["r-UTF8"] @(Either EncodeEx)
--          . toEncoding () $ b
-- :}
-- +++ OK, passed 100 tests.

instance Encode (Either EncodeEx) "r-UTF8" "r-UTF8" c B.ByteString where
    encoding = encUTF8B
  

instance Encode (Either EncodeEx) "r-UTF8" "r-UTF8" c BL.ByteString where
    encoding = encUTF8BL :: Encoding (Either EncodeEx) "r-UTF8" "r-UTF8" c BL.ByteString

encUTF8B :: Encoding (Either EncodeEx) "r-UTF8" "r-UTF8" c B.ByteString
encUTF8B = _implEncodingEx (fmap TE.encodeUtf8 . TE.decodeUtf8')
{-# WARNING encUTF8B "This method was not optimized for performance." #-}


encUTF8BL :: Encoding (Either EncodeEx) "r-UTF8" "r-UTF8" c BL.ByteString
encUTF8BL = _implEncodingEx (fmap TEL.encodeUtf8 . TEL.decodeUtf8')
{-# WARNING encUTF8BL "This method was not optimized for performance." #-}

-- * Decoding

instance (Applicative f) => Decode f "r-UTF8" "r-UTF8" c str where
    decoding = decAnyR

instance (RecreateErr f, Applicative f) =>  Validate f "r-UTF8" "r-UTF8" c B.ByteString  where
    validation = validR encUTF8B

instance (RecreateErr f, Applicative f) =>  Validate f "r-UTF8" "r-UTF8" c BL.ByteString  where
    validation = validR encUTF8BL



data UTF8Progress = Check | TwoB2 | ThreeB2 | ThreeB2R | ThreeB3 | FourB2 | FourB3 | FourB4 deriving Show

progress :: UTF8Progress -> UTF8Progress
progress ThreeB2 = ThreeB3
progress ThreeB2R = ThreeB3
progress FourB2 = FourB3
progress FourB3 = FourB4
progress _ = Check
{-# INLINE progress #-}

categorize :: UTF8Progress -> Word8 -> UTF8Progress
categorize Check w 
                | w <= 0x7F = Check
                | w <= 0xDF = TwoB2
                | w == 0xED = ThreeB2R
                | w <= 0xEF = ThreeB2
                | w <= 0xF7 = FourB2
                | otherwise = error $ "Check " ++ showHex w ""             
categorize ThreeB2R w 
                | between w 0x80 0xA0 = ThreeB3
                | otherwise = error $ "D76 " ++ showHex w ""   
categorize r w 
                | between w 0x80 0xbf = progress r
                | otherwise = error $ show r ++ " " ++ showHex w ""   
{-# INLINE categorize #-}


between x y z = x >= y && x <= z
{-# INLINE between #-}


checkUtf8 :: B.ByteString -> UTF8Progress
checkUtf8 = B.foldr (flip categorize) Check

tstBs = B.replicate 10000000 120

try1 :: IO ()
try1 = do
    let str = tstBs
        res = checkUtf8 str
    putStrLn $ show res  

try2 :: IO ()
try2 = do
    let str = tstBs
        res = TE.decodeUtf8' str
    case res of 
        Left err -> putStrLn $ show err
        Right x -> putStrLn (show $ T.head x)

-- putStrLn $ showHex 12 "" -- prints "c"
-- putStrLn $ showIntAtBase 2 intToDigit 12 "" -- prints "1100"

-- 0b01111111
-- 0b11011111
-- 0b11101111
-- 0b11110000
-- 0b11110111

-- 0b11101101



-- 0b10000000
-- 0b10111111


-- @"\xd800"@  (@11101101 10100000 10000000@ @ed a0 80@)
-- "\xdfff"    (@11101101 10111111 10111111@ @ed bf bf@)

--- Utilities ---

-- | helper function checks that given ByteString, 
-- if is encoded as Left is must be not Utf8 decodable
-- is is encoded as Right is must be Utf8 encodable 
verEncoding :: B.ByteString -> Either err B.ByteString -> Bool
verEncoding bs (Left _) = isLeft . TE.decodeUtf8' $ bs
verEncoding bs (Right _) = isRight . TE.decodeUtf8' $ bs
