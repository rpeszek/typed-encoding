
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-} -- removes need to annotate kinds as [Symbol]

-- | Lazy version of "Data.TypedEncoding.Conv.ByteString.Char8"
-- @since 0.2.2.0
module Data.TypedEncoding.Conv.ByteString.Lazy.Char8 where

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.TypedEncoding.Common.Util.TypeLits as Knds
import           Data.TypedEncoding.Instances.Support

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