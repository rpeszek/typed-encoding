
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Data.TypedEncoding.Combinators.Encode where

import           Data.TypedEncoding.Internal.Enc
import           Data.TypedEncoding.Class.Util -- Append
import           Data.TypedEncoding.Combinators.Common
import           Data.TypedEncoding.Class.Encode
import           GHC.TypeLits
import           Data.Functor.Identity
import           Data.Proxy

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
