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
type EncAnn = String    

type Restriction s = (KnownSymbol s, IsR s ~ 'True)

type Algorithm nm alg = AlgNm nm ~ alg


-- TODO v3 more to internal

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

-- |
-- >>> :kind! IsR "r-UPPER"
-- ...
-- ... 'True
--
-- >>> :kind! IsR "do-UPPER"
-- ...
-- = (TypeError ... 
type family IsR (s :: Symbol) :: Bool where
    IsR s = AcceptEq ('Text "Not restriction encoding " ':<>: ShowType s ) (CmpSymbol "r-" (Take 2 s))


type family IsROrEmpty (s :: Symbol) :: Bool where
    IsROrEmpty "" = True
    IsROrEmpty x  = IsR x

