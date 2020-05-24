

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- @since 0.2.2.0
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
-- >>> import Data.TypedEncoding
-- >>> import Data.TypedEncoding.Conv.Text
-- >>> let emptyUTF8B = unsafeSetPayload () "" :: Enc '["r-UTF8"] () B.ByteString  
-- >>> :{
-- instance Arbitrary (Enc '["r-UTF8"] () B.ByteString) where 
--      arbitrary =  fmap (fromRight emptyUTF8B) 
--                   . flip suchThat isRight 
--                   . fmap (encodeFAll @'["r-UTF8"] @(Either EncodeEx) @(). toEncoding ()) $ arbitrary 
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
-- "Enc '[r-ASCII] () (Text Hello)"
--
-- "r-UTF8" is redundant:
--
-- >>> displ . utf8Demote . decodeUtf8 $ (unsafeSetPayload () "Hello" :: Enc '["r-UTF8"] () B.ByteString)
-- "Enc '[] () (Text Hello)"
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
--
-- Similarly to 'Data.TypedEncoding.Conv.ByteString.Char8.pack' this function makes unverified assumption
-- that the encoding stack @xs@ does invalidate UTF8 byte layout.  This is safe for any "r-" encoding as well
-- as any of the "enc-" and "do-" encodings that can be currently found in this library. 
-- Future versions of this method are likely to introduce constraints that guarantee better type safety.
--
-- See "Data.TypedEncoding.Conv" for more detailed discussion.
--
-- @since 0.2.2.0
decodeUtf8 :: forall xs c t. (LLast xs ~ t, IsSuperset "r-UTF8" t ~ 'True) => Enc xs c B.ByteString -> Enc xs c T.Text 
decodeUtf8 = withUnsafe (fmap TE.decodeUtf8)

-- |
-- >>> displ $ encodeUtf8 $ utf8Promote $ toEncoding () ("text" :: T.Text)
-- "Enc '[r-UTF8] () (ByteString text)"
--
-- See 'decodeUtf8'.  Similar type safety concerns apply.
--
-- See "Data.TypedEncoding.Conv" for more detailed discussion.
--
-- @since 0.2.2.0
encodeUtf8 :: forall xs c t.  (LLast xs ~ t, IsSuperset "r-UTF8" t ~ 'True) => Enc xs c T.Text -> Enc xs c B.ByteString 
encodeUtf8 = withUnsafe (fmap TE.encodeUtf8)
