
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

-- | Exports for instance creation.
-- 
-- Contains typical things needed when implementing
-- encoding, decoding, recreate, or type to string conversions.
module Data.TypedEncoding.Instances.Support.Validate where
import           Data.TypedEncoding.Common.Types
import           Data.TypedEncoding.Common.Class 

import           GHC.TypeLits
import           Data.Proxy

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications


-- * Validation

validFromDec :: forall nm f c str . (KnownSymbol nm, RecreateErr f, Applicative f) => Decoding (Either UnexpectedDecodeEx) nm nm c str -> Validation f nm nm c str  
validFromDec = validFromDec' @nm @nm 

validFromDec' :: forall alg nm f c str . (KnownSymbol nm, RecreateErr f, Applicative f) => Decoding (Either UnexpectedDecodeEx) nm alg c str -> Validation f nm alg c str  
validFromDec' (UnsafeMkDecoding p fn) = UnsafeMkValidation p (decAsRecreateErr . fn)
   where 
        decAsRecreateErr :: Either UnexpectedDecodeEx a -> f a
        decAsRecreateErr (Left (UnexpectedDecodeEx p err)) = recoveryErr $ RecreateEx p err
        decAsRecreateErr (Right r) = pure r


validR :: forall nm f c str . (Restriction nm, KnownSymbol nm, RecreateErr f, Applicative f) => Encoding (Either EncodeEx) nm nm c str -> Validation f nm nm c str  
validR = validFromEnc' @nm @nm 

-- | Can cause slow compilation if used
validR' :: forall nm f c str alg . (Restriction nm, Algorithm nm alg, KnownSymbol nm, RecreateErr f, Applicative f) =>  Encoding (Either EncodeEx) nm alg c str -> Validation f nm alg c str  
validR' = validFromEnc' @alg @nm 


validFromEnc' :: forall alg nm f c str . (KnownSymbol nm, RecreateErr f, Applicative f) => Encoding (Either EncodeEx) nm alg c str -> Validation f nm alg c str  
validFromEnc' (UnsafeMkEncoding p fn) = UnsafeMkValidation p (encAsRecreateErr . rfn) 
   where
        encAsRecreateErr :: Either EncodeEx a -> f a
        encAsRecreateErr (Left (EncodeEx p err)) = recoveryErr $ RecreateEx p err
        encAsRecreateErr (Right r) = pure r 
        rfn :: forall (xs :: [Symbol]) . Enc (nm ': xs) c str -> Either EncodeEx (Enc xs c str)
        rfn (MkEnc _ conf str)  =    
            let re = fn $ MkEnc Proxy conf str
            in  MkEnc Proxy conf . getPayload <$> re 



