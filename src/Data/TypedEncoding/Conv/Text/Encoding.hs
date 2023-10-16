

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- @since 0.2.2.0
module Data.TypedEncoding.Conv.Text.Encoding where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE    

import           Data.TypedEncoding.Instances.Support
import qualified Data.TypedEncoding.Common.Util.TypeLits as Knds
import           Data.TypedEncoding.Instances.Restriction.UTF8 ()
import           Data.TypedEncoding.Instances.Restriction.ASCII ()
import           Data.TypedEncoding.Unsafe (withUnsafe)


-- $setup
-- >>> :set -XScopedTypeVariables -XOverloadedStrings -XDataKinds -XFlexibleContexts -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Instances.Text()
-- >>> import Test.QuickCheck.Instances.ByteString()
-- >>> import Data.TypedEncoding.Instances.Restriction.BoundedAlphaNums()
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
-- Note: For example, the @ByteString@ encoding of @"\xd800"@  (@11101101 10100000 10000000@ @ed a0 80@) is considered invalid /UTF8/ by the 'T.Text' library
-- To be consistent we make the same assumption of also restricting representable Unicode chars as in /Unicode.D76/.
--
-- >>> TE.decodeUtf8 "\237\160\128"
-- "*** Exception: Cannot decode byte '\xed': Data.Text.Encoding: Invalid UTF-8 stream
-- 
-- The "\xdfff" case (@11101101 10111111 10111111@ @ed bf bf@):
-- >>> TE.decodeUtf8 "\237\191\191"
-- "*** Exception: Cannot decode byte '\xed': Data.Text.Encoding: Invalid UTF-8 stream
--
-- >>> displ . decodeUtf8 $ (unsafeSetPayload () "Hello" :: Enc '["r-ASCII"] () B.ByteString)
-- "Enc '[r-ASCII] () (Text Hello)"
--
-- "r-UTF8" is redundant:
--
-- >>> displ . utf8Demote . decodeUtf8 $ (unsafeSetPayload () "Hello" :: Enc '["r-UTF8"] () B.ByteString)
-- "Enc '[] () (Text Hello)"
--
-- @decodeUtf8@ and @encodeUtf8@ now form isomorphism
-- 
-- prop> \x -> getPayload x == (getPayload . encodeUtf8 . decodeUtf8 @'["r-UTF8"] @() $ x)
--
-- prop> \x -> getPayload x == (getPayload . decodeUtf8 . encodeUtf8 @'["r-UTF8"] @() $ x)
--
-- These nicely work as iso's for "r-ASCII" subset
--
-- prop> \x -> getPayload x == (getPayload . encodeUtf8 . decodeUtf8 @'["r-ASCII"] @() $ x)
-- prop> \x -> getPayload x == (getPayload . decodeUtf8 . encodeUtf8 @'["r-ASCII"] @() $ x)
--
-- Similarly to 'Data.TypedEncoding.Conv.ByteString.Char8.pack' this function makes unverified assumption
-- that the encoding stack @xs@ does invalidate UTF8 byte layout.  This is safe for any "r-" encoding as well
-- as any of the "enc-" and "do-" encodings that can be currently found in this library. 
-- Future versions of this method are likely to introduce constraints that guarantee better type safety.
--
--
-- This is technically unsafe (even if we ignore the use of @unsafeSetPayload@) of decodeUtf8 
-- since currently @"r-ban:999"@ does not have @ByteString@ instances and that violates the assumption of matching encoding/decoding stacks
-- on both sides.  
-- >>> displ . decodeUtf8 $ (unsafeSetPayload () "123" :: Enc '["r-ban:999"] () B.ByteString)
-- "Enc '[r-ban:999] () (Text 123)"
--
-- See "Data.TypedEncoding.Conv" for more detailed discussion.
--
-- Note: implementation uses the partial 'TE.decodeUtf8' function but provides type level guarantee that it this function
-- will not error out unless unsafe combinators were used in constructing the encoded input
--
-- @since 0.4.0.0
decodeUtf8 :: forall xs c t y ys encs. (
          Knds.UnSnoc xs ~ '(,) ys y
         , Superset "r-UTF8" y
         , encs ~ RemoveRs ys
         , AllEncodeInto "r-UTF8" encs
        ) => Enc xs c B.ByteString -> Enc xs c T.Text 
decodeUtf8 = withUnsafe (fmap TE.decodeUtf8)

-- | simplified version of @decodeUtf8@ that works on single /r-/ encodings
-- @since 0.5.2.0
decodeUtf8_1 :: (
         Superset "r-UTF8" y 
         ) => Enc '[y] c  B.ByteString -> Enc '[y] c T.Text 
decodeUtf8_1 = decodeUtf8

-- |
-- >>> displ $ encodeUtf8 $ utf8Promote $ toEncoding () ("text" :: T.Text)
-- "Enc '[r-UTF8] () (ByteString text)"
--
-- See 'decodeUtf8'.  Similar type safety concerns apply.
--
-- See "Data.TypedEncoding.Conv" for more detailed discussion.
--
-- @since 0.4.0.0
encodeUtf8 :: forall xs c t y ys encs.   (
          Knds.UnSnoc xs ~ '(,) ys y
         , Superset "r-UTF8" y 
         , encs ~ RemoveRs ys
         , AllEncodeInto "r-UTF8" encs
        ) => Enc xs c T.Text -> Enc xs c B.ByteString 
encodeUtf8 = withUnsafe (fmap TE.encodeUtf8)

-- | simplified version of @decodeUtf8@ that works on single /r-/ encodings
-- @since 0.5.2.0
encodeUtf8_1 :: (
         Superset "r-UTF8" y 
         ) => Enc '[y] c  T.Text -> Enc '[y] c B.ByteString 
encodeUtf8_1 = encodeUtf8