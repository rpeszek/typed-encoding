{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Strings can move to 'Enc "r-ASCII' only if they contain only ascii characters.
-- they always decode back
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds
-- >>> encodeFAll . toEncoding () $ "Hello World" :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
-- Right (MkEnc Proxy () "Hello World")
--
-- >>> encodeFAll . toEncoding () $ "\194\160" :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
-- Left (EncodeEx "r-ASCII" (NonAsciiChar '\194'))
module Data.TypedEncoding.Instances.Restriction.ASCII where

import           Data.TypedEncoding.Instances.Support

import           Data.Proxy
import           Data.Functor.Identity
import           GHC.TypeLits

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE 
import qualified Data.Text.Lazy.Encoding as TEL 

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8

import           Data.Char
import           Data.TypedEncoding.Internal.Util (explainBool)
import           Data.TypedEncoding.Unsafe (withUnsafe)
import           Control.Arrow

-- $setup
-- >>> :set -XDataKinds -XTypeApplications

-----------------
-- Conversions --
-----------------

byteString2TextS :: Enc ("r-ASCII" ': ys) c B.ByteString -> Enc ("r-ASCII" ': ys) c T.Text 
byteString2TextS = withUnsafe (fmap TE.decodeUtf8)

byteString2TextL :: Enc ("r-ASCII" ': ys) c BL.ByteString -> Enc ("r-ASCII" ': ys) c TL.Text 
byteString2TextL = withUnsafe (fmap TEL.decodeUtf8)

text2ByteStringS :: Enc ("r-ASCII" ': ys) c T.Text -> Enc ("r-ASCII" ': ys) c B.ByteString 
text2ByteStringS = withUnsafe (fmap TE.encodeUtf8)

text2ByteStringL  :: Enc ("r-ASCII" ': ys) c TL.Text -> Enc ("r-ASCII" ': ys) c BL.ByteString 
text2ByteStringL  = withUnsafe (fmap TEL.encodeUtf8)


-- | allow to treat ASCII encodings as UTF8 forgetting about B64 encoding
-- 
-- >>> let Right tstAscii = encodeFAll . toEncoding () $ "Hello World" :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
-- >>> displ (inject @ "r-UTF8" tstAscii)
-- "MkEnc '[r-UTF8] () (Text Hello World)"
instance Superset "r-UTF8" "r-ASCII" where

-----------------
-- Encondings  --
-----------------

data NonAsciiChar = NonAsciiChar Char deriving (Eq, Show)

prxyAscii = Proxy :: Proxy "r-ASCII"

instance EncodeF (Either EncodeEx) (Enc xs c Char) (Enc ("r-ASCII" ': xs) c Char) where
    encodeF = implEncodeF prxyAscii (\c -> explainBool NonAsciiChar (c, isAscii c))    
instance Applicative f => DecodeF f (Enc ("r-ASCII" ': xs) c Char) (Enc xs c Char) where
    decodeF = implTranP id 

instance EncodeF (Either EncodeEx) (Enc xs c T.Text) (Enc ("r-ASCII" ': xs) c T.Text) where
    encodeF = implEncodeF prxyAscii (encodeImpl T.partition T.head T.null)
instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c T.Text) (Enc ("r-ASCII" ': xs) c T.Text) where
    checkPrevF = implCheckPrevF (asRecreateErr_ @"r-ASCII" . encodeImpl T.partition T.head T.null)
instance Applicative f => DecodeF f (Enc ("r-ASCII" ': xs) c T.Text) (Enc xs c T.Text) where
    decodeF = implTranP id 

instance EncodeF (Either EncodeEx) (Enc xs c TL.Text) (Enc ("r-ASCII" ': xs) c TL.Text) where
    encodeF = implEncodeF prxyAscii (encodeImpl TL.partition TL.head TL.null)
instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c TL.Text) (Enc ("r-ASCII" ': xs) c TL.Text) where 
    checkPrevF = implCheckPrevF (asRecreateErr_ @"r-ASCII" . encodeImpl TL.partition TL.head TL.null)
instance Applicative f => DecodeF f (Enc ("r-ASCII" ': xs) c TL.Text) (Enc xs c TL.Text) where
    decodeF = implTranP id 

instance EncodeF (Either EncodeEx) (Enc xs c B.ByteString) (Enc ("r-ASCII" ': xs) c B.ByteString) where
    encodeF = implEncodeF prxyAscii (encodeImpl (\p -> B8.filter p &&& B8.filter (not . p)) B8.head B8.null)
instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c B.ByteString) (Enc ("r-ASCII" ': xs) c B.ByteString) where
    checkPrevF = implCheckPrevF (asRecreateErr_ @"r-ASCII" . encodeImpl (\p -> B8.filter p &&& B8.filter (not . p)) B8.head B8.null)
instance Applicative f => DecodeF f (Enc ("r-ASCII" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
    decodeF = implTranP id 

instance EncodeF (Either EncodeEx) (Enc xs c BL.ByteString) (Enc ("r-ASCII" ': xs) c BL.ByteString) where
    encodeF = implEncodeF prxyAscii (encodeImpl (\p -> BL8.filter p &&& BL8.filter (not . p)) BL8.head BL8.null)
instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c BL.ByteString) (Enc ("r-ASCII" ': xs) c BL.ByteString) where
    checkPrevF = implCheckPrevF (asRecreateErr_ @"r-ASCII" . encodeImpl (\p -> BL8.filter p &&& BL8.filter (not . p)) BL8.head BL8.null)
instance Applicative f => DecodeF f (Enc ("r-ASCII" ': xs) c BL.ByteString) (Enc xs c BL.ByteString) where
    decodeF = implTranP id 

encodeImpl :: 
   ((Char -> Bool) -> a -> (a, a))
   -> (a -> Char)
   -> (a -> Bool)
   -> a
   -> Either NonAsciiChar a
encodeImpl partitionf headf nullf t = 
                 let (tascii, nonascii) = partitionf isAscii t 
                 in if nullf nonascii 
                    then Right tascii
                    else Left . NonAsciiChar $ headf nonascii 

-- tst = encodeFAll . toEncoding () $ "Hello World" :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
-- tst2 = encodeFAll . toEncoding () $ "\194\160" :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
-- tst3 = encodeFAll . toEncoding () $ "\194\160" :: Either EncodeEx (Enc '["r-ASCII"] () B.ByteString)
