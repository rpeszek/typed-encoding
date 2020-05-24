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
{-# LANGUAGE ConstraintKinds #-}

module Data.TypedEncoding.Common.Types.Common where

import           Data.TypedEncoding.Common.Util.TypeLits
import           GHC.TypeLits


-- $setup
-- >>> :set -XScopedTypeVariables -XTypeFamilies -XKindSignatures -XDataKinds


-- | Represents value level (single) annotation.
-- @since 0.2.0.0
type EncAnn = String    

-- | 
-- Contraint for "r-" annotations.
--
-- @since 0.3.0.0
type Restriction s = (KnownSymbol s, IsR s ~ 'True)

-- | 
-- Contraint for algorithm name.
--
-- @since 0.3.0.0
type Algorithm nm alg = AlgNm nm ~ alg


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
-- @since 0.3.0.0
type family AlgNm (encnm :: Symbol) :: Symbol where
    AlgNm encnm = TakeUntil encnm ":"

-- |
-- @since 0.3.0.0
type family AlgNmMap (nms :: [Symbol]) :: [Symbol] where
    AlgNmMap '[] = '[]
    AlgNmMap (x ': xs) = AlgNm x ': AlgNmMap xs

-- |
-- >>> :kind! IsR "r-UPPER"
-- ...
-- ... 'True
--
-- >>> :kind! IsR "do-UPPER"
-- ...
-- = (TypeError ... 
--
-- @since 0.2.1.0
type family IsR (s :: Symbol) :: Bool where
    IsR s = AcceptEq ('Text "Not restriction encoding " ':<>: ShowType s ) (CmpSymbol "r-" (Take 2 s))

-- |
-- @since 0.2.1.0 
type family IsROrEmpty (s :: Symbol) :: Bool where
    IsROrEmpty "" = True
    IsROrEmpty x  = IsR x

