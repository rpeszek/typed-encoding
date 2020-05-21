
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.TypedEncoding.Common.Class.Validate where

import           Data.TypedEncoding.Common.Types (RecreateEx(..))
import           Data.TypedEncoding.Common.Types.Validation

import           Data.Proxy
import           GHC.TypeLits


class Validate f nm alg conf str where
    validation :: Validation f nm alg conf str

class ValidateAll f nms algs conf str where
    validations :: Validations f nms algs conf str

instance ValidateAll f '[] '[] conf str where  
    validations = ZeroV

instance (ValidateAll f nms algs conf str, Validate f nm alg conf str) => ValidateAll f (nm ': nms) (alg ': algs) conf str where  
    validations = AppendV validation validations      



-- | Recovery errors are expected unless Recovery allows Identity instance
class RecreateErr f where 
    recoveryErr :: RecreateEx -> f a

instance RecreateErr (Either RecreateEx) where
    recoveryErr = Left  

asRecreateErr_ :: (RecreateErr f, Applicative f, Show err, KnownSymbol x) => Proxy x -> Either err a -> f a
asRecreateErr_ p (Left err) = recoveryErr $ RecreateEx p err
asRecreateErr_ _ (Right r) = pure r


asRecreateErr :: forall x f err a . (RecreateErr f, Applicative f, Show err, KnownSymbol x) => Either err a -> f a
asRecreateErr = asRecreateErr_ (Proxy :: Proxy x)
