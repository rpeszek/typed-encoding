{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Experimental features, slow to compile when used.
module Data.TypedEncoding.Internal.Class.Encode.Experimental where

import           Data.TypedEncoding.Internal.Types.Enc
import           Data.TypedEncoding.Internal.Common
import           Data.TypedEncoding.Internal.Class.Util -- Append
import           Data.TypedEncoding.Internal.Class.Encode    
import           Data.Functor.Identity
import           GHC.TypeLits

-- * Combinators equivalent to "Data.TypedEncoding.Internal.Class.Encode" that automatically figure out algorithm name.
-- Cause slow compilation time when used

_encF :: forall nm xs f c str alg . (Encode f nm alg c str, alg ~ AlgNm nm) => Enc xs c str -> f (Enc (nm ': xs) c str)
_encF = encF' @(AlgNm nm) @nm

_encFAll :: forall nms f c str algs . (Monad f,  EncodeAll f nms algs c str, algs ~ AlgNmMap nms) =>  
               Enc ('[]::[Symbol]) c str 
               -> f (Enc nms c str)  
_encFAll = encFAll' @(AlgNmMap nms) @nms

_encAll :: forall nms c str algs . (EncodeAll Identity nms algs c str, algs ~ AlgNmMap nms) =>
               Enc ('[]::[Symbol]) c str 
               -> Enc nms c str 
_encAll = encAll' @(AlgNmMap nms) @nms 

_encFPart :: forall xs xsf f c str algs . (Monad f, EncodeAll f xs algs c str, algs ~ AlgNmMap xs) => Enc xsf c str -> f (Enc (Append xs xsf) c str)
_encFPart = encFPart' @(AlgNmMap xs) @xs

_encPart :: forall xs xsf c str algs . (EncodeAll Identity xs algs c str, algs ~ AlgNmMap xs) => Enc xsf c str -> Enc (Append xs xsf) c str   
_encPart = encPart' @(AlgNmMap xs) @xs

