{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Data.TypedEncoding.Internal.Class.Encode where

import           Data.TypedEncoding.Internal.Types.Enc
import           Data.TypedEncoding.Internal.Class.Util -- Append
import           Data.TypedEncoding.Internal.Combinators.Common
import           GHC.TypeLits
import           Data.Functor.Identity
import           Data.Proxy

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
    encodings = AppendE encoding encodings      


-- * Convenience combinators which mimic pre-v0.3 type signatures. These assume that @algs@ are the same as @nms@

encF :: forall nm xs f c str . Encode f nm nm c str => Enc xs c str -> f (Enc (nm ': xs) c str)
encF = encF' @nm @nm

encFAll :: forall nms f c str . (Monad f,  EncodeAll f nms nms c str) =>  
               Enc ('[]::[Symbol]) c str 
               -> f (Enc nms c str)  
encFAll = encFAll' @nms @nms

encAll :: forall nms c str . (EncodeAll Identity nms nms c str) =>
               Enc ('[]::[Symbol]) c str 
               -> Enc nms c str 
encAll = encAll' @nms @nms 

encFPart :: forall xs xsf f c str . (Monad f, EncodeAll f xs xs c str) => Enc xsf c str -> f (Enc (Append xs xsf) c str)
encFPart = encFPart' @xs @xs

encPart :: forall xs xsf c str . (EncodeAll Identity xs xs c str) => Enc xsf c str -> Enc (Append xs xsf) c str   
encPart = encPart' @xs @xs


-- * Convenience combinators which mimic pre-v0.3 type signatures. These do not try to figure out @algs@ or assume much about them

encF' :: forall alg nm xs f c str . (Encode f nm alg c str) => Enc xs c str -> f (Enc (nm ': xs) c str)
encF' = runEncoding (encoding @f @nm @alg)

encFAll' :: forall algs nms f c str . (Monad f,  EncodeAll f nms algs c str) =>  
               Enc ('[]::[Symbol]) c str 
               -> f (Enc nms c str)  
encFAll' = runEncodings @algs @nms @f encodings 

-- | 
-- 
encAll' :: forall algs nms c str . (EncodeAll Identity nms algs c str) =>
               Enc ('[]::[Symbol]) c str 
               -> Enc nms c str 
encAll' = runIdentity . encFAll' @algs 

encFPart' :: forall algs xs xsf f c str . (Monad f, EncodeAll f xs algs c str) => Enc xsf c str -> f (Enc (Append xs xsf) c str)
encFPart' = aboveF @xsf @'[] @xs (encFAll' @algs) 
-- encFPart' (MkEnc _ conf str) =  
--     let re :: f (Enc xs c str) = encFAll' @algs $ MkEnc Proxy conf str
--     in  MkEnc Proxy conf . getPayload <$> re

encPart' :: forall algs xs xsf c str . (EncodeAll Identity xs algs c str) => Enc xsf c str -> Enc (Append xs xsf) c str   
encPart' = runIdentity . encFPart' @algs @xs


