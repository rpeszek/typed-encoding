
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
-- This assumes that each of the encodings in @xs@ work work equivalently in @String@ and @ByteString@.
--
-- This function assumes that encoding stack @xs@ does not shift characters outside of Char8 range.  
-- This is obviously true for all "r-" types
-- it is also true for any "enc-" and "do-" encodings currently available in this package. However, is possible to define a "do-" or "enc-" encodings that violate that. 
-- Future versions of /type-encoding/ are likely 
-- to introduce constraints to guard this aspect of the type safety better. 
--
-- This function also (currently) does not insist that @xs@ is a valid encoding stack for @ByteString@. 
-- This will be reexamined in the future, possibly offering alternatives with different safety levels.
--
-- Currently this uses (an over-conservative) @"r-ASCII"@ superset constraint.
--
-- See "Data.TypedEncoding.Conv" for more detailed discussion.
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
--
-- @since 0.2.2.0
pack :: (Knds.LLast xs ~ t, IsSuperset "r-ASCII" t ~ 'True) => Enc xs c String -> Enc xs c B8.ByteString
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
-- @since 0.2.2.0
unpack :: (Knds.LLast xs ~ t, IsSuperset "r-ASCII" t ~ 'True) => Enc xs c B8.ByteString -> Enc xs c String
unpack = unsafeChangePayload B8.unpack      

-- TODO consider adding "r-CHAR8"