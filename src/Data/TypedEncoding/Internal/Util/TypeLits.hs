

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


-- | TypeLits related utilities.
--
-- Lots of this could be avoided by adding @singletons@ as dependency.
--
-- Uses @symbols@ library for its ToList type family.
--
-- Currently this is spread out in different modules
--
-- * "Data.TypedEncoding.Internal.Class.Util"
-- * "Data.TypedEncoding.Internal.Types.SomeAnnotation"
--
-- TODO these will need to get consolidated here
module  Data.TypedEncoding.Internal.Util.TypeLits where

import           GHC.TypeLits
-- import           Data.Symbol.Utils
import           Data.Symbol.Ascii
-- import           Data.Proxy

-- $setup
-- >>> :set -XScopedTypeVariables -XTypeFamilies -XKindSignatures -XDataKinds

type family AcceptEq (msg :: ErrorMessage) (c :: Ordering) :: Bool where
    AcceptEq _  EQ = True
    AcceptEq msg _ =  TypeError msg

type family And (b1 :: Bool) (b2 :: Bool) :: Bool where
    And 'True 'True = 'True
    And _ _ = 'False

type family Or (b1 :: Bool) (b2 :: Bool) :: Bool where
    Or 'False 'False = 'False
    Or _ _ = 'True

type family Repeat (n :: Nat) (s :: Symbol) :: Symbol where
    Repeat 0 s = ""
    Repeat n s = AppendSymbol s (Repeat (n - 1) s)

type family Fst (s :: (k,h)) :: k where
   Fst ('(,) a _) = a

type family Dupl (s :: k) :: (k,k) where
   Dupl a = '(,) a a

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


-- type family DropVerify (n :: Nat) (s :: Symbol) (v :: Symbol) :: Symbol where
--     Drop n s v = Concat (LDrop n (ToList s))

-- type family DropVerify (n :: Nat) (s :: Symbol) (v :: Symbol) :: Symbol where
--     Drop n s v = Concat (LDrop n (ToList s))

-- | 
-- :kind! Take 3 "123456"
type family Take (n :: Nat) (s :: Symbol) :: Symbol where
    Take n s = Concat (LTake n (ToList s))

-- TODO create TypeList.List
-- | :kind! LTake 3 (ToList "123456")
type family LTake (n :: Nat) (s :: [k]) :: [k] where
    LTake 0 s = '[]
    LTake n '[] = '[]
    LTake n (x ': xs) = x ': LTake (n - 1) xs 

-- 
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
