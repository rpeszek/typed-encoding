

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | TypeLits relelated utilities.
--
-- Currently this is spread out in different modules
--
-- * "Data.TypedEncoding.Internal.Class.Util"
-- * "Data.TypedEncoding.Internal.Types.SomeAnnotation"
--
-- TODO these will need to get consolidated here
module  Data.TypedEncoding.Internal.Util.TypeLits where

import           GHC.TypeLits

-- $setup
-- >>> :set -XScopedTypeVariables -XTypeFamilies -XKindSignatures -XDataKinds


type family Repeat (n :: Nat) (s :: Symbol) :: Symbol where
    Repeat 0 s = ""
    Repeat n s = AppendSymbol s (Repeat (n - 1) s)