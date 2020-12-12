{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-} -- removes need to annotate kinds as [Symbol]
{-# LANGUAGE FlexibleContexts #-}

-- | Text encoding combinators specific to 'T.Text'
-- @since 0.2.2.0
module Data.TypedEncoding.Conv.Text where

import qualified Data.Text as T
import qualified Data.TypedEncoding.Common.Util.TypeLits as Knds
import           Data.TypedEncoding.Instances.Support


-- $setup
-- >>> :set -XDataKinds -XTypeFamilies -XTypeApplications


-- | This assumes that each of the encodings in @xs@ work work equivalently in @String@ and @Text@.
-- See discussion in "Examples.TypedEncoding.Conversions" and 
-- "Data.TypedEncoding.Conv.ByteString.Char8.pack"
pack :: (
          Knds.UnSnoc xs ~ '(,) ys y
         , Superset "r-UNICODE.D76" y 
         , encs ~ RemoveRs ys
         , AllEncodeInto "r-UNICODE.D76" encs
          ) => Enc xs c String -> Enc xs c T.Text
pack = unsafeChangePayload T.pack

-- | simplified version of @pack@ that works on single /r-/ encodings
-- @since 0.5.2.0
pack1 :: (
         Superset "r-UNICODE.D76" y 
         ) => Enc '[y] c String -> Enc '[y] c T.Text
pack1 = pack

-- | This assumes that each of the encodings in @xs@ work work equivalently in @String@ and @Text@.
-- This is similar to the assumptions made in 'pack'. 
unpack :: (
          Knds.UnSnoc xs ~ '(,) ys y
         , Superset "r-UNICODE.D76" y 
         , encs ~ RemoveRs ys
         , AllEncodeInto "r-UNICODE.D76" encs
          ) => Enc xs c T.Text -> Enc xs c String
unpack = unsafeChangePayload T.unpack 

-- | simplified version of @unpack@ that works on single /r-/ encodings
-- @since 0.5.2.0
unpack1 :: (
         Superset "r-UNICODE.D76" y 
         ) => Enc '[y] c T.Text -> Enc '[y] c String
unpack1 = unpack

-- | 
-- Text is automatically @"r-UTF8"@ encoded
-- 
-- Adding @"r-UTF8"@ annotation simply adds type level interpretion requirement that 'T.Text' is treated
-- as /UTF8/. The internals of 'T.Text' (currently /UTF-16/) are not relevant and @utf8Promote@ is implemented
-- as 'id'.  This is not the same as encoding @Word8@ layouts into 'Char'-s.
-- This, in /typed-encoding/ terminology, would be @"enc-UTF8"@, not @"r-UTF8".
--
-- >>> displ $ utf8Promote $ toEncoding () ("text" :: T.Text)
-- "Enc '[r-UTF8] () (Text text)"
utf8Promote :: Enc xs c T.Text -> Enc (Snoc xs "r-UTF8") c T.Text
utf8Promote = withUnsafeCoerce id

-- | 
-- For 'T.Text' @"r-UTF8"@ is redundant
--
-- >>> displ . utf8Demote $ (unsafeSetPayload () "Hello" :: Enc '["r-UTF8"] () T.Text)
-- "Enc '[] () (Text Hello)"
utf8Demote :: (UnSnoc xs ~ '(,) ys "r-UTF8") => Enc xs c T.Text -> Enc ys c T.Text
utf8Demote = withUnsafeCoerce id


d76Promote :: Enc xs c T.Text -> Enc (Snoc xs "r-UNICODE.D76") c T.Text
d76Promote = withUnsafeCoerce id

d76Demote :: (UnSnoc xs ~ '(,) ys "r-UNICODE.D76") => Enc xs c T.Text -> Enc ys c T.Text
d76Demote = withUnsafeCoerce id
