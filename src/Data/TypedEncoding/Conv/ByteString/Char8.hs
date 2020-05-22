
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-} -- removes need to annotate kinds as [Symbol]

-- | Encoding safe version of "Data.ByteString.Char8"
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
-- This assumes that each of the encodings in @xs@ work work equivalently in @String@ and @ByteString@.
--
-- Because of how @ByteString.Char8.pack@ works, the first encoding (last in the list) must restrict character set to a subset of @ASCII@. 
--
-- >>> :t pack (undefined :: Enc '["r-bar", "r-foo"] () String)
-- ...
-- ... error:
-- ... Couldn't match type ...
-- ... "r-ASCII" "r-foo" ...
-- ...
--
-- >>> displ $ pack (unsafeSetPayload () "Hello" :: Enc '["r-bar", "r-ASCII"] () String)
-- "Enc '[r-bar,r-ASCII] () (ByteString Hello)"
pack :: (Knds.LLast xs ~ t, IsSuperset "r-ASCII" t ~ 'True) => Enc xs c String -> Enc xs c B8.ByteString
pack = unsafeChangePayload B8.pack

-- | @unpack@ on encoded strings.
--
-- See 'pack'
unpack :: (Knds.LLast xs ~ t, IsSuperset "r-ASCII" t ~ 'True) => Enc xs c B8.ByteString -> Enc xs c String
unpack = unsafeChangePayload B8.unpack      