

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
-- Currently this is spread out to different modules including
--
-- * "Data.TypedEncoding.Common.Class.Util"
--
-- (TODO) these will need to get consolidated.
module  Data.TypedEncoding.Common.Util.TypeLits where

import           GHC.TypeLits
import           Data.Symbol.Ascii


-- $setup
-- >>> :set -XScopedTypeVariables -XTypeFamilies -XKindSignatures -XDataKinds

-- !
-- @since 0.2.1.0
type family AcceptEq (msg :: ErrorMessage) (c :: Ordering) :: Bool where
    AcceptEq _  EQ = True
    AcceptEq msg _ =  TypeError msg

-- !
-- @since 0.4.0.0
type family OrdBool (c :: Ordering) :: Bool where
    OrdBool EQ = 'True
    OrdBool _  =  'False


-- !
-- @since 0.2.1.0
type family And (b1 :: Bool) (b2 :: Bool) :: Bool where
    And 'True 'True = 'True
    And _ _ = 'False

-- !
-- @since 0.2.1.0
type family Or (b1 :: Bool) (b2 :: Bool) :: Bool where
    Or 'False 'False = 'False
    Or _ _ = 'True

-- !
-- @since 0.2.1.0
type family If (b1 :: Bool) (a :: k) (b :: k) :: k where
    If 'True a _ = a
    If 'False _ b = b

-- !
-- @since 0.2.1.0
type family Repeat (n :: Nat) (s :: Symbol) :: Symbol where
    Repeat 0 s = ""
    Repeat n s = AppendSymbol s (Repeat (n - 1) s)

-- !
-- @since 0.2.1.0
type family Fst (s :: (k,h)) :: k where
   Fst ('(,) a _) = a

type family Dupl (s :: k) :: (k,k) where
   Dupl a = '(,) a a

-- | 
-- >>> :kind! Concat (LDrop 6 (ToList "bool: \"r-ban:ff-ff\" | \"r-ban:ffff\""))
-- ...
-- = "\"r-ban:ff-ff\" | \"r-ban:ffff\""
-- 
-- @since 0.2.1.0
type family Concat (s :: [Symbol]) :: Symbol where
    Concat '[] = ""
    Concat (x ': xs) = AppendSymbol x (Concat xs)

-- !
-- @since 0.2.1.0
type family Drop (n :: Nat) (s :: Symbol) :: Symbol where
    Drop n s = Concat (LDrop n (ToList s))

-- TODO create TypeList.List module ?

-- :kind! LDrop 6 (ToList "bool: \"r-ban:ff-ff\" | \"r-ban:ffff\"")

-- | 
-- 
-- @since 0.2.1.0
type family LDrop (n :: Nat) (s :: [k]) :: [k] where
    LDrop 0 s = s
    LDrop n '[] = '[]
    LDrop n (x ': xs) = LDrop (n - 1) xs 


-- type family DropVerify (n :: Nat) (s :: Symbol) (v :: Symbol) :: Symbol where
--     Drop n s v = Concat (LDrop n (ToList s))

-- type family DropVerify (n :: Nat) (s :: Symbol) (v :: Symbol) :: Symbol where
--     Drop n s v = Concat (LDrop n (ToList s))

-- | 
-- >>> :kind! Take 3 "123456"
-- ...
-- = "123"
-- 
-- @since 0.2.1.0
type family Take (n :: Nat) (s :: Symbol) :: Symbol where
    Take n s = Concat (LTake n (ToList s))

-- :kind! LTake 3 (ToList "123456")

-- | 
-- 
-- @since 0.2.1.0
type family LTake (n :: Nat) (s :: [k]) :: [k] where
    LTake 0 s = '[]
    LTake n '[] = '[]
    LTake n (x ': xs) = x ': LTake (n - 1) xs 

-- 
-- >>> kind! TakeUntil "findme:blah" ":"
-- ...
-- = "findme"
--
-- @since 0.2.1.0
type family TakeUntil (s :: Symbol) (stop :: Symbol) :: Symbol where
    TakeUntil s stop = Concat (LTakeUntil (ToList s) stop)

-- | 
-- 
-- @since 0.2.1.0
type family LTakeUntil (s :: [Symbol]) (stop :: Symbol) :: [Symbol] where
    LTakeUntil '[] _ = '[]
    LTakeUntil (x ': xs) stop = LTakeUntilHelper (x ': LTakeUntil xs stop) (CmpSymbol x stop)

type family LTakeUntilHelper (s :: [Symbol]) (o :: Ordering) :: [Symbol] where
    LTakeUntilHelper '[] _ = '[]
    LTakeUntilHelper (x ': xs) 'EQ = '[]
    LTakeUntilHelper (x ': xs) _ = (x ': xs)

-- | 
-- 
-- @since 0.2.1.0
type family Length (s :: Symbol) :: Nat where  
    Length x = LLengh (ToList x)

-- | 
-- 
-- @since 0.2.1.0
type family LLengh (s :: [k]) :: Nat where
    LLengh '[] = 0
    LLengh (x ': xs) = 1 + LLengh xs


-- |
-- >>> :kind! LLast '["1","2","3"]
-- ...
-- = "3"
--
-- @since 0.2.2.0
type family LLast (s :: [Symbol]) :: Symbol where
    LLast '[] = TypeError ('Text "Empty Symbol list not allowed")
    LLast '[x] = x
    LLast (_ ': xs) = LLast xs



-- |
-- >>> :kind! Concat (Snoc '["1","2","3"] "4") 
-- ...
-- = "1234"
--
-- @since 0.2.2.0
type family Snoc (s :: [k]) (t :: k) :: [k] where
    Snoc '[] x = '[x]
    Snoc (x ': xs) y = x ': Snoc xs y


-- |
-- :kind! UnSnoc '["1","2","3"]  
-- ...
-- = '( (':) Symbol "1" ((':) Symbol "2" ('[] Symbol)), "3")
--
-- @since 0.2.2.0  
type family UnSnoc (s :: [k]) :: ([k], k) where
    UnSnoc '[] = TypeError ('Text "Empty list, no last element")     
    UnSnoc '[x] = '(,) '[] x
    UnSnoc (x ': xs) = UnSnocHelper x (UnSnoc xs)


type family UnSnocHelper (s :: k) (t :: ([k], k)) :: ([k], k) where 
   UnSnocHelper y ('(,) xs x) = '(,) (y ': xs) x   

   