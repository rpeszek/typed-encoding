
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

import           Data.TypedEncoding.Common.Types.Enc
import           Data.TypedEncoding.Common.Class.Util -- Append
import           Data.TypedEncoding.Combinators.Common
import           Data.TypedEncoding.Common.Class.Encode
import           GHC.TypeLits
import           Data.Functor.Identity


-- * Convenience combinators which mimic pre-v0.3 type signatures. These assume that @algs@ are the same as @nms@

encodeF :: forall nm xs f c str . Encode f nm nm c str => Enc xs c str -> f (Enc (nm ': xs) c str)
encodeF = encodeF' @nm @nm

encodeFAll :: forall nms f c str . (Monad f,  EncodeAll f nms nms c str) =>  
               Enc ('[]::[Symbol]) c str 
               -> f (Enc nms c str)  
encodeFAll = encodeFAll' @nms @nms

encodeAll :: forall nms c str . (EncodeAll Identity nms nms c str) =>
               Enc ('[]::[Symbol]) c str 
               -> Enc nms c str 
encodeAll = encodeAll' @nms @nms 

encodeFPart :: forall xs xsf f c str . (Monad f, EncodeAll f xs xs c str) => Enc xsf c str -> f (Enc (Append xs xsf) c str)
encodeFPart = encodeFPart' @xs @xs

encodePart :: forall xs xsf c str . (EncodeAll Identity xs xs c str) => Enc xsf c str -> Enc (Append xs xsf) c str   
encodePart = encodePart' @xs @xs


-- * Convenience combinators which mimic pre-v0.3 type signatures. These do not try to figure out @algs@ or assume much about them

encodeF' :: forall alg nm xs f c str . (Encode f nm alg c str) => Enc xs c str -> f (Enc (nm ': xs) c str)
encodeF' = runEncoding' (encoding @f @nm @alg)

encodeFAll' :: forall algs nms f c str . (Monad f,  EncodeAll f nms algs c str) =>  
               Enc ('[]::[Symbol]) c str 
               -> f (Enc nms c str)  
encodeFAll' = runEncodings' @algs @nms @f encodings 

-- | 
-- 
encodeAll' :: forall algs nms c str . (EncodeAll Identity nms algs c str) =>
               Enc ('[]::[Symbol]) c str 
               -> Enc nms c str 
encodeAll' = runIdentity . encodeFAll' @algs 

encodeFPart' :: forall algs xs xsf f c str . (Monad f, EncodeAll f xs algs c str) => Enc xsf c str -> f (Enc (Append xs xsf) c str)
encodeFPart' = aboveF @xsf @'[] @xs (encodeFAll' @algs) 
-- encodeFPart' (UnsafeMkEnc _ conf str) =  
--     let re :: f (Enc xs c str) = encodeFAll' @algs $ UnsafeMkEnc Proxy conf str
--     in  UnsafeMkEnc Proxy conf . getPayload <$> re

encodePart' :: forall algs xs xsf c str . (EncodeAll Identity xs algs c str) => Enc xsf c str -> Enc (Append xs xsf) c str   
encodePart' = runIdentity . encodeFPart' @algs @xs
