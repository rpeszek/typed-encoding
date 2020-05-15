
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Lazy version of "Data.TypedEncoding.Conv.ByteString.Lazy"
module Data.TypedEncoding.Conv.ByteString.Lazy.Char8 where

import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.TypedEncoding.Internal.Types.Enc (Enc, unsafeChangePayload)
import qualified Data.TypedEncoding.Internal.Util.TypeLits as Knds
import           Data.TypedEncoding

-- TODO superset game?

-- $setup
-- >>> :set -XDataKinds -XTypeApplications -XOverloadedStrings

-- | 
-- Type safe version of 'BL8.pack'.
--
-- :t pack (undefined :: Enc '["r-bar", "r-ASCII"] () String)
-- :t pack (undefined :: Enc '["r-bar", "r-foo"] () String)
pack :: (Knds.LLast xs ~ t, IsSuperset "r-ASCII" t ~ 'True) => Enc xs c String -> Enc xs c BL8.ByteString
pack = unsafeChangePayload BL8.pack

-- | 
-- Type safe version of 'BL8.unpack'.
unpack :: (Knds.LLast xs ~ t, IsSuperset "r-ASCII" t ~ 'True) => Enc xs c BL8.ByteString -> Enc xs c String
unpack = unsafeChangePayload BL8.unpack          