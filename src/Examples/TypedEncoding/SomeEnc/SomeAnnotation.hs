{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

-- | Internally used existential type for tracking of annotations
--
-- This module is re-exported in "Data.TypedEncoding" and it is best not to import it directly.

module Examples.TypedEncoding.SomeEnc.SomeAnnotation where

import           Data.TypedEncoding.Common.Types.Common
import           Data.TypedEncoding.Common.Class.Common
import           Data.TypedEncoding.Common.Util.TypeLits (withSomeSymbol, proxyCons)
import           Data.Proxy
import           GHC.TypeLits

-- |
-- @since 0.2.0.0
data SomeAnnotation where
    MkSomeAnnotation :: SymbolList xs => Proxy xs -> SomeAnnotation

-- |
-- @since 0.2.0.0
withSomeAnnotation :: SomeAnnotation -> (forall xs . SymbolList xs => Proxy xs -> r) -> r
withSomeAnnotation (MkSomeAnnotation p) fn = fn p


-- | folds over SomeSymbol list 
-- @since 0.2.0.0
someAnnValue :: [EncAnn] -> SomeAnnotation
someAnnValue xs = 
     foldr (fn . someSymbolVal) (MkSomeAnnotation (Proxy :: Proxy '[])) xs
     where 
         somesymbs = map someSymbolVal xs
         fn ss (MkSomeAnnotation pxs) = withSomeSymbol ss (\px -> MkSomeAnnotation  (px `proxyCons` pxs)) 


