

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.TypedEncoding.Conv.Text.Encoding where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE    

import           Data.TypedEncoding.Instances.Support
import           Data.TypedEncoding.Instances.Restriction.UTF8 ()
import           Data.TypedEncoding.Instances.Restriction.ASCII ()
import           Data.TypedEncoding.Unsafe (withUnsafe)


-- $setup
-- >>> :set -XScopedTypeVariables -XOverloadedStrings -XDataKinds -XFlexibleContexts -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Instances.Text()
-- >>> import Test.QuickCheck.Instances.ByteString()
-- >>> import qualified Data.ByteString.Char8 as B8
-- >>> import Data.Char
-- >>> import Data.Either
-- >>> import Data.TypedEncoding.Conv.Text
-- >>> let emptyUTF8B = unsafeSetPayload () "" :: Enc '["r-UTF8"] () B.ByteString  
-- >>> :{
-- instance Arbitrary (Enc '["r-UTF8"] () B.ByteString) where 
--      arbitrary =  fmap (fromRight emptyUTF8B) 
--                   . flip suchThat isRight 
--                   . fmap (encodeFAll @(Either EncodeEx) @'["r-UTF8"] @(). toEncoding ()) $ arbitrary 
-- instance Arbitrary (Enc '["r-UTF8"] () T.Text) where 
--      arbitrary =  fmap (unsafeSetPayload ()) 
--                         arbitrary 
-- instance Arbitrary (Enc '["r-ASCII"] () B.ByteString) where 
--      arbitrary =  fmap (unsafeSetPayload ()) 
--                   . flip suchThat (B8.all isAscii) 
--                        $ arbitrary 
-- instance Arbitrary (Enc '["r-ASCII"] () T.Text) where 
--      arbitrary =  fmap (unsafeSetPayload ()) 
--                   . flip suchThat (T.all isAscii) 
--                        $ arbitrary 
-- :}


-- |
-- With given constraints 'decodeUtf8' and 'encodeUtf8' can be used on subsets of @"r-UTF8"@
--
-- >>> displ . decodeUtf8 $ (unsafeSetPayload () "Hello" :: Enc '["r-ASCII"] () B.ByteString)
-- "MkEnc '[r-ASCII] () (Text Hello)"
--
-- "r-UTF8" is redundant:
--
-- >>> displ . utf8Demote . decodeUtf8 $ (unsafeSetPayload () "Hello" :: Enc '["r-UTF8"] () B.ByteString)
-- "MkEnc '[] () (Text Hello)"
--
-- @decodeUtf8@ and  @encodeUtf8@ form isomorphism
-- 
-- prop> \x -> getPayload x == (getPayload . encodeUtf8 . decodeUtf8 @ '["r-UTF8"] @() $ x)
--
-- prop> \x -> getPayload x == (getPayload . decodeUtf8 . encodeUtf8 @ '["r-UTF8"] @() $ x)
--
-- These nicely work as iso's for "r-ASCII" subset
--
-- prop> \x -> getPayload x == (getPayload . encodeUtf8 . decodeUtf8 @ '["r-ASCII"] @() $ x)
-- prop> \x -> getPayload x == (getPayload . decodeUtf8 . encodeUtf8 @ '["r-ASCII"] @() $ x)
decodeUtf8 :: forall xs c t. (LLast xs ~ t, IsSuperset "r-UTF8" t ~ 'True) => Enc xs c B.ByteString -> Enc xs c T.Text 
decodeUtf8 = withUnsafe (fmap TE.decodeUtf8)

-- |
-- >>> displ $ encodeUtf8 $ utf8Promote $ toEncoding () ("text" :: T.Text)
-- "MkEnc '[r-UTF8] () (ByteString text)"
encodeUtf8 :: forall xs c t.  (LLast xs ~ t, IsSuperset "r-UTF8" t ~ 'True) => Enc xs c T.Text -> Enc xs c B.ByteString 
encodeUtf8 = withUnsafe (fmap TE.encodeUtf8)
