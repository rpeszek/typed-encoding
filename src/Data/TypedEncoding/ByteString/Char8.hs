
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ByteString encoding combinators that do not belong 
-- in a module specific to a particual encoding.
module Data.TypedEncoding.ByteString.Char8 where

import qualified Data.ByteString.Char8 as B8
import           Data.TypedEncoding.Internal.Types.Enc (Enc, unsafeChangePayload)
import qualified Data.TypedEncoding.Internal.Util.TypeLits as Knds
import           Data.TypedEncoding

-- $setup
-- >>> :set -XDataKinds -XTypeApplications -XOverloadedStrings

-- | 
-- Type safer version of 'Data.ByteString.Char8.pack'.
-- 
-- This assumes that each of the encodings in @xs@ work work equivalently in @String@ and @ByteString@.
--
-- Because of how @ByteString.Char8.pack@ works, the first encoding (last in the list) must restrict character set to a subset of @ASCII@. 
--
-- >>> :t pack (undefined :: Enc '["r-bar", "r-foo"] () String)
-- ...
-- ... No instance for (Superset "r-ASCII" "r-foo")
-- ...
--
-- >>> displ $ pack (unsafeSetPayload () "Hello" :: Enc '["r-bar", "r-ASCII"] () String)
-- "MkEnc '[r-bar,r-ASCII] () (ByteString Hello)"
pack :: (Knds.LLast xs ~ t, Superset "r-ASCII" t) => Enc xs c String -> Enc xs c B8.ByteString
pack = unsafeChangePayload B8.pack

-- | @unpack@ on encoded strings.
--
-- See 'pack'
unpack :: (Knds.LLast xs ~ t, Superset "r-ASCII" t) => Enc xs c B8.ByteString -> Enc xs c String
unpack = unsafeChangePayload B8.unpack      