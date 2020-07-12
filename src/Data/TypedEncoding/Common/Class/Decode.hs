{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}


-- |
-- Type classes accompanying decoding types defined in "Data.TypedEncoding.Common.Types.Decoding"
--
-- "Examples.TypedEncoding.Instances.DiySignEncoding" contains an implementation example.
--
-- "Examples.TypedEncoding.Overview" shows decoding usage examples.
-- 
-- This module is re-exported in "Data.TypedEncoding" and it is best not to import it directly.
module Data.TypedEncoding.Common.Class.Decode where

import           Data.TypedEncoding.Common.Types (UnexpectedDecodeEx(..))
import           Data.TypedEncoding.Common.Types.Decoding
import           Data.Proxy
import           Data.Functor.Identity
import           GHC.TypeLits

-- |
-- @since 0.3.0.0
class Decode f nm alg conf str where
    decoding :: Decoding f nm alg conf str

-- |
-- @since 0.3.0.0
class DecodeAll f nms algs conf str where
    decodings :: Decodings f nms algs conf str

instance DecodeAll f '[] '[] conf str where  
    decodings = ZeroD 

instance (DecodeAll f nms algs conf str, Decode f nm alg conf str) => DecodeAll f (nm ': nms) (alg ': algs) conf str where  
    decodings = ConsD decoding decodings      


-- | With type safety in place decoding errors should be unexpected.
-- This class can be used to provide extra info if decoding could fail
--
-- @since 0.1.0.0
class UnexpectedDecodeErr f where 
    unexpectedDecodeErr :: UnexpectedDecodeEx -> f a

instance UnexpectedDecodeErr Identity where
    unexpectedDecodeErr x = fail $ show x

instance UnexpectedDecodeErr (Either UnexpectedDecodeEx) where
    unexpectedDecodeErr = Left 

-- |
-- @since 0.1.0.0
asUnexpected_ :: (KnownSymbol x, UnexpectedDecodeErr f, Applicative f, Show err) => Proxy x -> Either err a -> f a
asUnexpected_ p (Left err) = unexpectedDecodeErr $ UnexpectedDecodeEx p err
asUnexpected_ _ (Right r) = pure r

-- |
-- @since 0.1.0.0
asUnexpected :: forall x f err a . (KnownSymbol x, UnexpectedDecodeErr f, Applicative f, Show err) => Either err a -> f a
asUnexpected = asUnexpected_ (Proxy :: Proxy x)
