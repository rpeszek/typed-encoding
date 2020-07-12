
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

-- | 
-- Convenience validation utilities.
module Data.TypedEncoding.Instances.Support.Validate where
import           Data.TypedEncoding.Common.Types
import           Data.TypedEncoding.Common.Class 

import           GHC.TypeLits
import           Data.Proxy

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications


-- * Validation

-- |
-- @since 0.3.0.0
validFromDec :: forall nm f c str . (KnownSymbol nm, RecreateErr f, Applicative f) => Decoding (Either UnexpectedDecodeEx) nm nm c str -> Validation f nm nm c str  
validFromDec = validFromDec' @nm @nm 

-- |
-- @since 0.3.0.0
validFromDec' :: forall alg nm f c str . (KnownSymbol nm, RecreateErr f, Applicative f) => Decoding (Either UnexpectedDecodeEx) nm alg c str -> Validation f nm alg c str  
validFromDec' (UnsafeMkDecoding p fn) = UnsafeMkValidation p (decAsRecreateErr . fn)
   where 
        decAsRecreateErr :: Either UnexpectedDecodeEx a -> f a
        decAsRecreateErr (Left (UnexpectedDecodeEx p err)) = recoveryErr $ RecreateEx p err
        decAsRecreateErr (Right r) = pure r

-- |
-- @since 0.3.0.0
validR :: forall nm f c str . (Restriction nm, KnownSymbol nm, RecreateErr f, Applicative f) => Encoding (Either EncodeEx) nm nm c str -> Validation f nm nm c str  
validR = validRFromEnc' @nm @nm 




-- | Can cause slow compilation if used
--
-- (renamed from validR defined in pre 0.5 versions)
--
-- @since 0.5.0.0
_validR :: forall nm f c str alg . (Restriction nm, Algorithm nm alg, KnownSymbol nm, RecreateErr f, Applicative f) =>  Encoding (Either EncodeEx) nm alg c str -> Validation f nm alg c str  
_validR = validRFromEnc' @alg @nm 



-- |
-- This should be used with "r-" validations only
--
-- @since 0.3.0.0
validFromEnc' :: forall alg nm f c str . (KnownSymbol nm, RecreateErr f, Applicative f) => Encoding (Either EncodeEx) nm alg c str -> Validation f nm alg c str  
validFromEnc' (UnsafeMkEncoding p fn) = UnsafeMkValidation p (encAsRecreateErr . rfn) 
   where
        encAsRecreateErr :: Either EncodeEx a -> f a
        encAsRecreateErr (Left (EncodeEx p err)) = recoveryErr $ RecreateEx p err
        encAsRecreateErr (Right r) = pure r 
        rfn :: forall (xs :: [Symbol]) . Enc (nm ': xs) c str -> Either EncodeEx (Enc xs c str)
        rfn (UnsafeMkEnc _ conf str)  =    
            let re = fn $ UnsafeMkEnc Proxy conf str
            in  UnsafeMkEnc Proxy conf . getPayload <$> re 


{-# DEPRECATED validFromEnc' "Use validR_ instead (valid for r- encodings only)" #-}


validRFromEnc' :: forall alg nm f c str . (KnownSymbol nm, RecreateErr f, Applicative f) => Encoding (Either EncodeEx) nm alg c str -> Validation f nm alg c str  
validRFromEnc' (UnsafeMkEncoding p fn) = UnsafeMkValidation p (encAsRecreateErr . rfn) 
   where
        encAsRecreateErr :: Either EncodeEx a -> f a
        encAsRecreateErr (Left (EncodeEx p err)) = recoveryErr $ RecreateEx p err
        encAsRecreateErr (Right r) = pure r 
        rfn :: forall (xs :: [Symbol]) . Enc (nm ': xs) c str -> Either EncodeEx (Enc xs c str)
        rfn (UnsafeMkEnc _ conf str)  =    
            let re = fn $ UnsafeMkEnc Proxy conf str
            in  UnsafeMkEnc Proxy conf . getPayload <$> re 

