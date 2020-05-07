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
import           Data.Either (either)

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications
-- >>> import qualified Data.Text as T

-- | Faster that @BoolOp s ~ "or"@ constraint
type IsBoolOr s =
    "boolOr:" == s ||
    (CmpSymbol "boolOr:"  s == LT && CmpSymbol "boolOr;" s == GT)    

-- | Faster that @BoolOp s ~ "and"@ constraint
type IsBoolAnd s =
    "boolAnd:" == s ||
    (CmpSymbol "boolAnd:"  s == LT && CmpSymbol "boolAnd;" s == GT)    


-- TODO error handling!
encodeBoolOr :: forall s xs f c str t1 t2 . (
    -- BoolOp s ~ "or" 
    IsBoolOr s ~ 'True
    , f ~ Either EncodeEx
    , FirstTerm s ~ t1
    , SecondTerm s ~ t2
    , EncodeF f (Enc xs c str) (Enc (t1 ': xs) c str) 
    , EncodeF f (Enc xs c str) (Enc (t2 ': xs) c str) 
    ) => Enc xs c str -> f (Enc (s ': xs) c str)  
encodeBoolOr en0 =  
       let 
           ent1 :: f (Enc (t1 ': xs) c str) = encodeF en0
           ent2 :: f (Enc (t2 ': xs) c str) = encodeF en0
       in 
           either (const (withUnsafeCoerce id <$> ent1)) (Right . withUnsafeCoerce id) ent2 :: f (Enc (s ': xs) c str)

-- TODO displ tst1 not compiling
tst1, tst2, tst3 :: Either EncodeEx (Enc '["boolOr:(r-ban:999-999-9999)(r-ban:(999) 999-9999)"] () T.Text)
tst1 = encodeBoolOr . toEncoding () $ "212-222-3333" 
tst2 = encodeBoolOr . toEncoding () $ "(212) 222-3333" 
tst3 = encodeBoolOr . toEncoding () $ "212 222 3333"

-- TODO error handling!
encodeBoolAnd :: forall s xs f c str t1 t2 . (
    -- BoolOp s ~ "and" 
    IsBoolAnd s ~ 'True
    , f ~ Either EncodeEx
    , FirstTerm s ~ t1
    , SecondTerm s ~ t2
    , EncodeF f (Enc xs c str) (Enc (t1 ': xs) c str) 
    , EncodeF f (Enc xs c str) (Enc (t2 ': xs) c str) 
    ) => Enc xs c str -> f (Enc (s ': xs) c str)  
encodeBoolAnd en0 =  
       let 
           ent1 :: f (Enc (t1 ': xs) c str) = encodeF en0
           ent2 :: f (Enc (t2 ': xs) c str) = encodeF en0
       in 
           undefined

-- * Type family based parser 

-- | 
-- This works fast with @!kind@ but is much slower in declaration 
-- :kind! BoolOp "boolOr:()()"
type family BoolOp (s :: Symbol) :: Symbol where
    BoolOp s = ToLower (TakeUntil (Drop 4 s) ":")

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
             
