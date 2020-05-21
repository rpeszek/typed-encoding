{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Data.TypedEncoding.Common.Class.Encode where

import           Data.TypedEncoding.Internal.Enc


-- | 
-- Using 2 Symbol type variables (@nm@ and @alg@) creates what seems like redundant typing
-- in statically defined instances such as @"r-ASCII"@, however it 
-- provides future flexibility to 
-- constrain @nm@ in some interesting way, different than @AlgNm nm ~ alg@. 
--
-- It also seems to be easier to understand as type variables used in the definition of 'Encoding'
-- match with what is on the typeclass.
--
-- @alg@ is expected to be very statically defined and is needed to support more open instances such as @"r-ban"@.
class Encode f nm alg conf str where
    encoding :: Encoding f nm alg conf str

class EncodeAll f nms algs conf str where
    encodings :: Encodings f nms algs conf str

instance EncodeAll f '[] '[] conf str where  
    encodings = ZeroE  

instance (EncodeAll f nms algs conf str, Encode f nm alg conf str) => EncodeAll f (nm ': nms) (alg ': algs) conf str where  
    encodings = ConsE encoding encodings      




