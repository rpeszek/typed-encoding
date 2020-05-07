{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE UndecidableInstances #-}
-- {-# OPTIONS_GHC -fprint-potential-instances #-}

module Data.TypedEncoding.Instances.Restriction.Bool where 


import           GHC.TypeLits
import           Data.Type.Bool -- ((||), (&&))
import           Data.Type.Equality -- ((==))
import qualified Data.List as L
import           Data.Char
import           Data.Proxy
import           Data.Either
import qualified Data.Text as T

import           Data.Symbol.Utils
import           Data.Symbol.Ascii

import           Data.TypedEncoding.Internal.Class.IsStringR
import           Data.TypedEncoding.Instances.Support
import           Data.TypedEncoding.Internal.Util.TypeLits

import           Data.TypedEncoding.Instances.Restriction.BoundedAlphaNums 
import           Data.TypedEncoding.Internal.Types.Enc

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications
-- >>> import qualified Data.Text as T

-- better compilation errors?
type IsBool s =
    "bool:" == s ||
    (CmpSymbol "bool:" s == LT && CmpSymbol "bool;" s == GT)    

-- white space significant?
-- tst :: Enc '["bool-or: (r-ban:ff-ff) (r-ban:ffff)"] () T.Text

-- tst :: Enc '["bool: \"r-ban:ff-ff\" | \"r-ban:ffff\""] () T.Text
-- tst = unsafeLoadPayload () "ab-1e"


-- instance (IsStringR str, KnownSymbol s, Drop 4 s ~ "boo") =>  EncodeF (Either EncodeEx) (Enc xs c str) (Enc (s ': xs) c str) where
--     encodeF = undefined

-- TODOs
-- create Shared module with universal Decode instance based on "r-" prefix

-- DOES NOT WANT TO DISAMBIGUATE
-- encodeFAll . toEncoding () $ "c59f9fb7-4621-44d9-9020-ce37bf6e2bd1" :: Either EncodeEx (Enc '["bool:r-ban:ffffffff-ffff-ffff-ffff-ffffffffffff"] () T.Text)
-- instance {-# OVERLAPPING #-} (IsBool s ~ 'True, Drop 5 s ~ t, EncodeF f (Enc xs c str) (Enc (t ': xs) c str), Functor f) => EncodeF f (Enc xs c str) (Enc (s ': xs) c str) where
--    encodeF = implChangeAnn (encodeF @f @(Enc xs c str) @(Enc (t ': xs) c str))
-- instance (IsStringR str, KnownSymbol s, IsBan s ~ 'True) =>  EncodeF (Either EncodeEx) (Enc xs c str) (Enc (s ': xs) c str) where
--     encodeF = implEncodeF @s (verifyBoundedAlphaNum (Proxy :: Proxy s))

encodeBool :: forall s xs f c str t . (IsBool s ~ 'True, Drop 5 s ~ t, EncodeF f (Enc xs c str) (Enc (t ': xs) c str), Functor f) =>
    Enc xs c str -> f (Enc (s ': xs) c str)  
encodeBool =  implChangeAnn (encodeF @f @(Enc xs c str) @(Enc (t ': xs) c str))       

-- TODO displ tst1 not compiling
tst1 = encodeBool . toEncoding () $ "c59f9fb7-4621-44d9-9020-ce37bf6e2bd1" :: Either EncodeEx (Enc '["bool:r-ban:ffffffff-ffff-ffff-ffff-ffffffffffff"] () T.Text)
tst = encodeFAll . toEncoding () $ "c59f9fb7-4621-44d9-9020-ce37bf6e2bd1" :: Either EncodeEx (Enc '["r-ban:ffffffff-ffff-ffff-ffff-ffffffffffff"] () T.Text)


-- * Type family based parser 

-- dropLast . takeFstParen . parenCnt $ "((AGA)(bcd))(123)" 
-- :kind! FirstTerm "(agag)(222)"
type family FirstTerm (s :: Symbol) :: Symbol where
    FirstTerm s = Concat (LDropLast( LTakeFstParen (LParenCnt (ToList s))))

-- dropLast . takeSndParen 0 . parenCnt $ "((agag)(sagg))(agaga)"
-- :kind! SecondTerm "(agag)(222)"
--
type family SecondTerm (s :: Symbol) :: Symbol where
    SecondTerm s = Concat (LDropLast (LTakeSndParen 0 (LParenCnt (ToList s))))


type family LDropLast (s :: [Symbol]) :: [Symbol] where
    LDropLast '[] = '[]
    LDropLast '[x] = '[]
    LDropLast (x ': xs) = x ': LDropLast xs

-- parent count is slow
type family LParenCnt (s :: [Symbol]) :: [(Symbol, Nat)] where
    LParenCnt '[] = '[]
    LParenCnt ("(" ': xs) = LParenCntHelper ('(,) "(" 'Decr) (LParenCnt xs) -- '(,) "(" (LParenCntFstCnt (LParenCnt xs) - 1) ': LParenCnt xs
    LParenCnt (")" ': xs) = LParenCntHelper ('(,) ")" 'Incr) (LParenCnt xs)
    LParenCnt (x ': xs) = LParenCntHelper ('(,) x 'NoChng) (LParenCnt xs)

-- parenCnt :: String -> [(Char, Int)]
-- parenCnt [] = []
-- parenCnt ('(' : xs) = parenCntHelper ('(', -1) (parenCnt xs) --('(', parenCntFstCnt (parenCnt xs) - 1) : parenCnt xs
-- parenCnt (')' : xs) = parenCntHelper (')', 1) (parenCnt xs)  -- (')', parenCntFstCnt (parenCnt xs) + 1) : parenCnt xs
-- parenCnt (x : xs) = parenCntHelper (x, 0) (parenCnt xs) -- (x, parenCntFstCnt (parenCnt xs) ) : parenCnt xs


-- type family LParenCntFstCnt (si :: [(Symbol, Nat)]) :: Nat where
--     LParenCntFstCnt '[] = 0
--     LParenCntFstCnt ('(,) _ i  ': _) = i


data Adjust = Incr | Decr | NoChng

type family AdjHelper (a :: Adjust) (n :: Nat) :: Nat where
   AdjHelper 'Incr n = n + 1
   AdjHelper 'Decr 0 = 0
   AdjHelper 'Decr n = n - 1
   AdjHelper 'NoChng n = n

type family LParenCntHelper (s :: (Symbol, Adjust)) (sx :: [(Symbol, Nat)]) :: [(Symbol, Nat)] where
    LParenCntHelper ('(,) x k) '[] = '(,) x (AdjHelper k 0) ': '[]
    LParenCntHelper ('(,) x k) ('(,) c i : xs) = '(,) x (AdjHelper k i) ': '(,) c i ': xs


type family LTakeFstParen (si :: [(Symbol, Nat)]) :: [Symbol] where 
    LTakeFstParen '[] = '[]
    LTakeFstParen ('(,) _ 0 ': xs) = LTakeFstParen xs
    LTakeFstParen ('(,) ")" 1 ': _) = '[")"]
    LTakeFstParen ('(,) a p ': xs) = a ': LTakeFstParen xs


type family LTakeSndParen (n :: Nat)  (si :: [(Symbol, Nat)]) :: [Symbol] where
    LTakeSndParen _ '[] = '[]
    LTakeSndParen 0 ('(,) ")" 1 ': xs) = LTakeSndParen 1 xs
    LTakeSndParen 1 ('(,) _ 0 : xs) = LTakeSndParen 1 xs
    LTakeSndParen 0 ('(,) _ _ ': xs) = LTakeSndParen 0 xs
    LTakeSndParen 1 ('(,) a _ ': xs) = a : LTakeSndParen 1 xs
    LTakeSndParen n _ = '[]

-- * (Example) Value level equivalend of type family parsers

-- map snd . parenCnt $ "((AGA)(bcd))(123) "
-- dropLast . takeFstParen . parenCnt $ "((AGA)(bcd))(123)" 
-- dropLast . takeSndParen 0 . parenCnt $ "((AGA)(bcd))(123)" 

-- parenCnt1 :: String -> [(Char, Int)]
-- parenCnt1 [] = []
-- parenCnt1 ('(' : xs) = ('(', parenCntFstCnt (parenCnt1 xs) - 1) : parenCnt1 xs
-- parenCnt1 (')' : xs) = (')', parenCntFstCnt (parenCnt1 xs) + 1) : parenCnt1 xs
-- parenCnt1 (x : xs) = (x, parenCntFstCnt (parenCnt1 xs) ) : parenCnt1 xs

parenCnt :: String -> [(Char, Int)]
parenCnt [] = []
parenCnt ('(' : xs) = parenCntHelper ('(', -1) (parenCnt xs) --('(', parenCntFstCnt (parenCnt xs) - 1) : parenCnt xs
parenCnt (')' : xs) = parenCntHelper (')', 1) (parenCnt xs)  -- (')', parenCntFstCnt (parenCnt xs) + 1) : parenCnt xs
parenCnt (x : xs) = parenCntHelper (x, 0) (parenCnt xs) -- (x, parenCntFstCnt (parenCnt xs) ) : parenCnt xs


parenCntHelper :: (Char, Int) -> [(Char, Int)]  -> [(Char, Int)]
parenCntHelper (x, k) [] = [(x,k)]
parenCntHelper (x, k) ((c,i) : xs) = (x, i + k): (c,i) : xs


takeFstParen :: [(Char, Int)] -> [Char]
takeFstParen [] = []
takeFstParen ((_, 0) : xs) = takeFstParen xs
takeFstParen ((')', 1) : _) = [')']
takeFstParen ((a, p) : xs) = a : takeFstParen xs

takeSndParen :: Int -> [(Char, Int)] -> [Char]
takeSndParen _ [] = []
takeSndParen 0 ((')', 1) : xs) = takeSndParen 1 xs
takeSndParen 1 ((_, 0) : xs) = takeSndParen 1 xs
takeSndParen 0 ((_, _) : xs) = takeSndParen 0 xs
takeSndParen 1 ((a, _) : xs) = a : takeSndParen 1 xs
takeSndParen _ _ = []

dropLast :: [Char] -> [Char]
dropLast [] = []
dropLast [x] = []
dropLast (x:xs) = x : dropLast xs
             
