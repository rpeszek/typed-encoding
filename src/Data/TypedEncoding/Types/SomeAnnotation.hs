{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

-- | internally used existential type for taking track of annotations
module Data.TypedEncoding.Types.SomeAnnotation where

import           Data.TypedEncoding.Types.Common
import           Data.TypedEncoding.Class.Util
import           Data.TypedEncoding.Internal.Util
import           Data.Proxy
import           GHC.TypeLits


data SomeAnnotation where
    MkSomeAnnotation :: SymbolList xs => Proxy xs -> SomeAnnotation

withSomeAnnotation :: SomeAnnotation -> (forall xs . SymbolList xs => Proxy xs -> r) -> r
withSomeAnnotation (MkSomeAnnotation p) fn = fn p

-- folds over SomeSymbol list using withSomeSymbol and proxyCons
someAnnValue :: [EncAnn] -> SomeAnnotation
someAnnValue xs = 
     foldr (fn . someSymbolVal) (MkSomeAnnotation (Proxy :: Proxy '[])) xs
     where 
         somesymbs = map someSymbolVal xs
         fn ss (MkSomeAnnotation pxs) = withSomeSymbol ss (\px -> MkSomeAnnotation  (px `proxyCons` pxs)) 

proxyCons :: forall (x :: Symbol) (xs :: [Symbol]) . Proxy x -> Proxy xs -> Proxy (x ': xs)
proxyCons _ _ = Proxy