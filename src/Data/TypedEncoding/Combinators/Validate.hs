{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Combinators reexported in Data.TypedEncoding
module Data.TypedEncoding.Combinators.Validate where

import           Data.TypedEncoding.Combinators.Common
import           Data.TypedEncoding.Combinators.Unsafe
import           Data.TypedEncoding.Common.Types
import           Data.TypedEncoding.Common.Class.Validate
import           Data.TypedEncoding.Common.Class.Util (SymbolList, Append)
import           GHC.TypeLits
import           Data.Functor.Identity


-- * Validation

-- | Maybe signals annotation mismatch, effect @f@ is not evaluated unless there is match
checkWithValidations :: forall algs (nms :: [Symbol]) f c str . (
                    Monad f
                    , SymbolList nms
                    ) => Validations f nms algs c str -> UncheckedEnc c str -> Maybe (f (Enc nms c str))
checkWithValidations vers x = 
      case verifyAnn @nms x of
            Left err -> Nothing -- asRecreateErr_ perr $ Left err
            Right (MkUncheckedEnc _ c str) -> Just $ recreateWithValidations vers . toEncoding c $ str

check :: forall (nms :: [Symbol]) f c str . (
            Monad f
            ,  ValidateAll f nms nms c str
            , SymbolList nms
           ) => UncheckedEnc c str -> Maybe (f (Enc nms c str)) 
check = checkWithValidations @nms @nms @f validations 

check' :: forall algs (nms :: [Symbol]) f c str . (
            Monad f
            ,  ValidateAll f nms algs c str
            , SymbolList nms
           ) => UncheckedEnc c str -> Maybe (f (Enc nms c str)) 
check' = checkWithValidations @algs @nms @f validations 


recreateWithValidations :: forall algs nms f c str . (Monad f) => Validations f nms algs c str -> Enc ('[]::[Symbol]) c str -> f (Enc nms c str)
recreateWithValidations vers str@(MkEnc _ _ pay) = 
        let str0 :: Enc nms c str = withUnsafeCoerce id str
        in withUnsafeCoerce (const pay) <$> runValidationChecks vers str0    

-- * v0.2 style recreate functions

recrFAll :: forall nms f c str . (Monad f,  ValidateAll f nms nms c str) =>  
               Enc ('[]::[Symbol]) c str 
               -> f (Enc nms c str)  
recrFAll = recrFAll' @nms @nms 

recrAll :: forall nms c str . (ValidateAll Identity nms nms c str) =>
               Enc ('[]::[Symbol]) c str 
               -> Enc nms c str 
recrAll = recrAll' @nms @nms  

recrFPart :: forall xs xsf f c str . (Monad f, ValidateAll f xs xs c str) => Enc xsf c str -> f (Enc (Append xs xsf) c str)
recrFPart = recrFPart' @xs @xs 

-- * Convenience combinators which mimic pre-v0.3 type signatures. These do not try to figure out @algs@ or assume much about them

recrFAll' :: forall algs nms f c str . (Monad f,  ValidateAll f nms algs c str) =>  
               Enc ('[]::[Symbol]) c str 
               -> f (Enc nms c str)  
recrFAll' = recreateWithValidations @algs @nms @f validations 

recrAll' :: forall algs nms c str . (ValidateAll Identity nms algs c str) =>
               Enc ('[]::[Symbol]) c str 
               -> Enc nms c str 
recrAll' = runIdentity . recrFAll' @algs 

recrFPart' :: forall algs xs xsf f c str . (Monad f, ValidateAll f xs algs c str) => Enc xsf c str -> f (Enc (Append xs xsf) c str)
recrFPart' = aboveF @xsf @'[] @xs (recrFAll' @algs) 

recrPart' :: forall algs xs xsf c str . (ValidateAll Identity xs algs c str) => Enc xsf c str -> Enc (Append xs xsf) c str   
recrPart' = runIdentity . recrFPart' @algs @xs


--------------------------------------------
