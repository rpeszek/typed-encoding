{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Data.TypedEncoding.Internal.Class.Decode where

import           Data.TypedEncoding.Internal.Types (UnexpectedDecodeEx(..))
import           Data.TypedEncoding.Internal.Types.Enc
import           Data.TypedEncoding.Internal.Types.Decoding

import           Data.TypedEncoding.Internal.Class.Util
import           Data.Proxy
import           Data.Functor.Identity
import           GHC.TypeLits


class Decode f nm alg conf str where
    decoding :: Decoding f nm alg conf str

class DecodeAll f nms algs conf str where
    decodings :: Decodings f nms algs conf str

instance DecodeAll f '[] '[] conf str where  
    decodings = ZeroD 

instance (DecodeAll f nms algs conf str, Decode f nm alg conf str) => DecodeAll f (nm ': nms) (alg ': algs) conf str where  
    decodings = AppendD decoding decodings      

-- * Convenience combinators which mimic pre-v0.3 type signatures. These assume that @algs@ are the same as @nms@

decF :: forall nm xs f c str . (Decode f nm nm c str) => Enc (nm ': xs) c str -> f (Enc xs c str)
decF = decF' @nm @nm

decFAll :: forall nms f c str . (Monad f,  DecodeAll f nms nms c str) => 
               Enc nms c str
               -> f (Enc ('[]::[Symbol]) c str)  
decFAll = decFAll' @nms @nms 

-- | 
-- 
decAll :: forall nms c str . (DecodeAll Identity nms nms c str) =>
               Enc nms c str 
               -> Enc ('[]::[Symbol]) c str 
decAll = decAll' @nms @nms  

decFPart :: forall xs xsf f c str . (Monad f, DecodeAll f xs xs c str) => Enc (Append xs xsf) c str -> f (Enc xsf c str)
decFPart = decFPart' @xs @xs 

decPart :: forall xs xsf c str . (DecodeAll Identity xs xs c str) => Enc (Append xs xsf) c str -> Enc xsf c str   
decPart = decPart' @xs @xs  



-- * Convenience combinators which mimic pre-v0.3 type signatures. These do not try to figure out @algs@ or assume much about them

decF' :: forall alg nm xs f c str . (Decode f nm alg c str) => Enc (nm ': xs) c str -> f (Enc xs c str)
decF' = runDecoding (decoding @f @nm @alg)

decFAll' :: forall algs nms f c str . (Monad f,  DecodeAll f nms algs c str) => 
               Enc nms c str
               -> f (Enc ('[]::[Symbol]) c str)  
decFAll' = runDecodings @algs @nms @f decodings 

-- | 
-- 
decAll' :: forall algs nms c str . (DecodeAll Identity nms algs c str) =>
               Enc nms c str 
               -> Enc ('[]::[Symbol]) c str 
decAll' = runIdentity . decFAll' @algs 

decFPart' :: forall algs xs xsf f c str . (Monad f, DecodeAll f xs algs c str) => Enc (Append xs xsf) c str -> f (Enc xsf c str)
decFPart' (MkEnc _ conf str) =   
    let re :: f (Enc '[] c str) = decFAll' @algs @xs $ MkEnc Proxy conf str
    in  MkEnc Proxy conf . getPayload <$> re

decPart' :: forall algs xs xsf c str . (DecodeAll Identity xs algs c str) => Enc (Append xs xsf) c str -> Enc xsf c str   
decPart' = runIdentity . decFPart' @algs @xs



-- | With type safety in place decoding errors should be unexpected.
-- This class can be used to provide extra info if decoding could fail
class UnexpectedDecodeErr f where 
    unexpectedDecodeErr :: UnexpectedDecodeEx -> f a

instance UnexpectedDecodeErr Identity where
    unexpectedDecodeErr x = fail $ show x

instance UnexpectedDecodeErr (Either UnexpectedDecodeEx) where
    unexpectedDecodeErr = Left 

asUnexpected_ :: (KnownSymbol x, UnexpectedDecodeErr f, Applicative f, Show err) => Proxy x -> Either err a -> f a
asUnexpected_ p (Left err) = unexpectedDecodeErr $ UnexpectedDecodeEx p err
asUnexpected_ _ (Right r) = pure r

asUnexpected :: forall x f err a . (KnownSymbol x, UnexpectedDecodeErr f, Applicative f, Show err) => Either err a -> f a
asUnexpected = asUnexpected_ (Proxy :: Proxy x)
