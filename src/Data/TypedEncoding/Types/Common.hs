{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TypedEncoding.Types.Common where

import           Data.TypedEncoding.Util.TypeLits
import           GHC.TypeLits


-- $setup
-- >>> :set -XScopedTypeVariables -XTypeFamilies -XKindSignatures -XDataKinds


-- | Represents value level (single) annotation.
type EncAnn = String    


-- |
-- Converts encoding name to algorithm name, this assumes the ":" delimiter expected by this library. 
--
-- This allows working with open encoding definitions such as "r-ban" or "r-bool"
-- 
-- >>> :kind! AlgNm "enc-B64"
-- ...
-- = "enc-B64"
--
-- >>> :kind! AlgNm "r-ban:999-99-9999"
-- ...
-- = "r-ban"
--
type family AlgNm (encnm :: Symbol) :: Symbol where
    AlgNm encnm = TakeUntil encnm ":"


type family AlgNmMap (nms :: [Symbol]) :: [Symbol] where
    AlgNmMap '[] = '[]
    AlgNmMap (x ': xs) = AlgNm x ': AlgNmMap xs