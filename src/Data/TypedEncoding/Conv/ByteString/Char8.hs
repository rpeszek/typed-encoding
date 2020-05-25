
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-} -- removes need to annotate kinds as [Symbol]

-- | Encoding safe(r) version of "Data.ByteString.Char8"
-- @since 0.2.2.0
module Data.TypedEncoding.Conv.ByteString.Char8 where

import qualified Data.ByteString.Char8 as B8
import qualified Data.TypedEncoding.Common.Util.TypeLits as Knds
import           Data.TypedEncoding.Instances.Support

-- $setup
-- >>> :set -XDataKinds -XTypeApplications -XOverloadedStrings

-- | 
-- Type safer version of 'Data.ByteString.Char8.pack'.
-- 
-- This assumes that each of the encodings in @xs@ work equivalently in @String@ and @ByteString@.
--
-- This function also (currently) does not insist that @xs@ is a valid encoding stack for @ByteString@. 
-- This will be reexamined in the future, possibly offering alternatives with different safety levels.
--
-- Expected encoding stack @xs@ needs to have @"r-" as last element and it needs to be more restrictive 
-- than "r-CHAR8" (String cannot have chars @> 255@).  Otherwise any encodings other than "r-" need to 
-- encode into "r-CHAR8".
--
-- See "Data.TypedEncoding.Conv" for more detailed discussion.
--
-- >>> :t pack (undefined :: Enc '["r-bar", "r-foo"] () String)
-- ...
-- ... error:
-- ... Couldn't match type ...
-- ...
--
-- >>> displ $ pack (unsafeSetPayload () "Hello" :: Enc '["r-bar", "r-ASCII"] () String)
-- "Enc '[r-bar,r-ASCII] () (ByteString Hello)"
--
--  @since 0.4.0.0
pack :: (
    Knds.UnSnoc xs ~ '(,) ys y
    , IsSuperset "r-CHAR8" y ~ 'True
    , encs ~ RemoveRs ys
    , AllEncodeInto "r-CHAR8" encs
    ) => Enc xs c String -> Enc xs c B8.ByteString
pack = unsafeChangePayload B8.pack

-- | @unpack@ on encoded strings.
--
-- See 'pack'
--
-- Similarly to 'pack' this makes assumptions on what the encoding stack is allowed to do. These are not type checked.
-- Again, this is safe with any stack that uses "r-" only encodings. 
-- Future versions of /type-encoding/ are likely 
-- to introduce constraints to guard this aspect of the type safety better. 
--
-- @since 0.4.0.0
unpack :: (
          Knds.UnSnoc xs ~ '(,) ys y
         , IsSuperset "r-CHAR8" y ~ 'True
         , encs ~ RemoveRs ys
         , AllEncodeInto "r-CHAR8" encs
          ) => Enc xs c B8.ByteString -> Enc xs c String
unpack = unsafeChangePayload B8.unpack      

