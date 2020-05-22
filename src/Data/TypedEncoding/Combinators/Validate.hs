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
recreateWithValidations vers str@(UnsafeMkEnc _ _ pay) = 
        let str0 :: Enc nms c str = withUnsafeCoerce id str
        in withUnsafeCoerce (const pay) <$> runValidationChecks vers str0    

-- * v0.2 style recreate functions

recreateFAll :: forall nms f c str . (Monad f,  ValidateAll f nms nms c str) =>  
               Enc ('[]::[Symbol]) c str 
               -> f (Enc nms c str)  
recreateFAll = recreateFAll' @nms @nms 

recreateAll :: forall nms c str . (ValidateAll Identity nms nms c str) =>
               Enc ('[]::[Symbol]) c str 
               -> Enc nms c str 
recreateAll = recreateAll' @nms @nms  

recreateFPart :: forall xs xsf f c str . (Monad f, ValidateAll f xs xs c str) => Enc xsf c str -> f (Enc (Append xs xsf) c str)
recreateFPart = recreateFPart' @xs @xs 

-- * Convenience combinators which mimic pre-v0.3 type signatures. These do not try to figure out @algs@ or assume much about them

recreateFAll' :: forall algs nms f c str . (Monad f,  ValidateAll f nms algs c str) =>  
               Enc ('[]::[Symbol]) c str 
               -> f (Enc nms c str)  
recreateFAll' = recreateWithValidations @algs @nms @f validations 

recreateAll' :: forall algs nms c str . (ValidateAll Identity nms algs c str) =>
               Enc ('[]::[Symbol]) c str 
               -> Enc nms c str 
recreateAll' = runIdentity . recreateFAll' @algs 

recreateFPart' :: forall algs xs xsf f c str . (Monad f, ValidateAll f xs algs c str) => Enc xsf c str -> f (Enc (Append xs xsf) c str)
recreateFPart' = aboveF @xsf @'[] @xs (recreateFAll' @algs) 

recreatePart' :: forall algs xs xsf c str . (ValidateAll Identity xs algs c str) => Enc xsf c str -> Enc (Append xs xsf) c str   
recreatePart' = runIdentity . recreateFPart' @algs @xs


--------------------------------------------
