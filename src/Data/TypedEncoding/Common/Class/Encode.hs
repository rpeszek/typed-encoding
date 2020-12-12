{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Type classes accompanying decoding types defined in "Data.TypedEncoding.Common.Types.Enc"
--
-- "Examples.TypedEncoding.Instances.DiySignEncoding" contains an implementation example.
--
-- "Examples.TypedEncoding.Overview" shows decoding usage examples.
-- 
-- This module is re-exported in "Data.TypedEncoding" and it is best not to import it directly.

module Data.TypedEncoding.Common.Class.Encode where

import           Data.TypedEncoding.Common.Types.Enc

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XAllowAmbiguousTypes
-- >>> import qualified Data.ByteString as B
-- >>> import Data.Functor.Identity
-- >>> import Data.TypedEncoding
-- >>> import Data.TypedEncoding.Instances.Enc.Base64 ()

-- | 
-- Allows for polymorphic access to encoding, for example
--
-- >>> displ (runIdentity . _runEncoding encoding $ toEncoding () "Hello" :: Enc '["enc-B64"] () B.ByteString)
-- "Enc '[enc-B64] () (ByteString SGVsbG8=)"
--
-- Using 2 Symbol type variables (@nm@ and @alg@) creates what seems like redundant typing
-- in statically defined instances such as @"r-ASCII"@, however it 
-- provides future flexibility to 
-- constrain @nm@ in some interesting way, different than @AlgNm nm ~ alg@. 
--
-- It also seems to be easier to understand as type variables used in the definition of 'Encoding'
-- match with what is on the typeclass.
--
-- @alg@ is expected to be very statically defined and is needed to support more open instances such as @"r-ban"@.
--
-- @since 0.3.0.0
class Encode f nm alg conf str where
    encoding :: Encoding f nm alg conf str

-- |
-- Allows for polymorphic access to Encodings
-- 
-- For example
--
-- >>> displ (runIdentity . _runEncodings encodings $ toEncoding () "Hello" :: (Enc '["enc-B64", "enc-B64"] () B.ByteString))
-- "Enc '[enc-B64,enc-B64] () (ByteString U0dWc2JHOD0=)"
--
-- You can also use convenience functions like @encodeAll@
-- 
-- @since 0.3.0.0
class EncodeAll f nms algs conf str where
    encodings :: Encodings f nms algs conf str

instance EncodeAll f '[] '[] conf str where  
    encodings = ZeroE  

instance (EncodeAll f nms algs conf str, Encode f nm alg conf str) => EncodeAll f (nm ': nms) (alg ': algs) conf str where  
    encodings = ConsE encoding encodings      




