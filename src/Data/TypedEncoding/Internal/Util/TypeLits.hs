

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
import           Data.Symbol.Utils
import           Data.Symbol.Ascii
import           Data.Proxy

-- $setup
-- >>> :set -XScopedTypeVariables -XTypeFamilies -XKindSignatures -XDataKinds


type family Repeat (n :: Nat) (s :: Symbol) :: Symbol where
    Repeat 0 s = ""
    Repeat n s = AppendSymbol s (Repeat (n - 1) s)


-- | :kind! Concat (LDrop 6 (ToList "bool: \"r-ban:ff-ff\" | \"r-ban:ffff\""))
type family Concat (s :: [Symbol]) :: Symbol where
    Concat '[] = ""
    Concat (x ': xs) = AppendSymbol x (Concat xs)

type family Drop (n :: Nat) (s :: Symbol) :: Symbol where
    Drop n s = Concat (LDrop n (ToList s))

-- TODO create TypeList.List
-- | :kind! LDrop 6 (ToList "bool: \"r-ban:ff-ff\" | \"r-ban:ffff\"")
type family LDrop (n :: Nat) (s :: [k]) :: [k] where
    LDrop 0 s = s
    LDrop n '[] = '[]
    LDrop n (x ': xs) = LDrop (n - 1) xs 

-- |
-- :kind! TakeUntil "findme:blah" ":"
type family TakeUntil (s :: Symbol) (stop :: Symbol) :: Symbol where
    TakeUntil s stop = Concat (LTakeUntil (ToList s) stop)

type family LTakeUntil (s :: [Symbol]) (stop :: Symbol) :: [Symbol] where
    LTakeUntil '[] _ = '[]
    LTakeUntil (x ': xs) stop = LTakeUntilHelper (x ': LTakeUntil xs stop) (CmpSymbol x stop)

type family LTakeUntilHelper (s :: [Symbol]) (o :: Ordering) :: [Symbol] where
    LTakeUntilHelper '[] _ = '[]
    LTakeUntilHelper (x ': xs) 'EQ = '[]
    LTakeUntilHelper (x ': xs) _ = (x ': xs)


type family Length (s :: Symbol) :: Nat where  
    Length x = LLengh (ToList x)

type family LLengh (s :: [k]) :: Nat where
    LLengh '[] = 0
    LLengh (x ': xs) = 1 + LLengh xs


