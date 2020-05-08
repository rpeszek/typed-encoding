
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE KindSignatures  #-}s
{-# LANGUAGE UndecidableInstances #-}

module Data.TypedEncoding.Combinators.Restriction.Common where 

import           GHC.TypeLits
import           Data.TypedEncoding.Internal.Util.TypeLits

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications

-- |
-- :kind! IsR "r-UPPER"
-- ... 'True
-- :kind! IsR "do-UPPER"
-- ...
-- ... TypeError ... 
type family IsR (s :: Symbol) :: Bool where
    IsR s = AcceptEq ('Text "Not restriction encoding " ':<>: ShowType s ) (CmpSymbol "r-" (Take 2 s))


type family IsROrEmpty (s :: Symbol) :: Bool where
    IsROrEmpty "" = True
    IsROrEmpty x  = IsR x

