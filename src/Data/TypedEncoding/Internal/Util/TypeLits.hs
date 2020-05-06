

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
{-# LANGUAGE TypeApplications #-}

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


type family ParentCnt (s :: [Symbol]) :: [(Symbol, Nat)] where
    ParentCnt '[] = '[]
    ParentCnt ("(" ': xs) = '(,) "(" (ParenCntFstCnt (ParentCnt xs) - 1) ': ParentCnt xs
    ParentCnt (")" ': xs) = '(,)  ")" (ParenCntFstCnt (ParentCnt xs) + 1) ': ParentCnt xs
    ParentCnt (x ': xs) = '(,) x (ParenCntFstCnt (ParentCnt xs) ) ': ParentCnt xs

type family ParenCntFstCnt (si :: [(Symbol, Nat)]) :: Nat where
    ParenCntFstCnt '[] = 0
    ParenCntFstCnt ('(,) c i  ': xs) = i

type family TakeFrstParen (si :: [(Symbol, Nat)]) :: [Symbol] where 
    TakeFrstParen '[] = '[]
    TakeFrstParen ('(,) x 0 ': xs) = '[]
    TakeFrstParen ('(,) a p ': xs) = a ': TakeFrstParen xs

type family DropLast (s :: [k]) :: [k] where
    DropLast '[] = '[]
    DropLast '[x] = '[]
    DropLast (x ': xs) = x ': DropLast xs

-- need DropAfterParen to replace LDrop 1 use it for first and second 
-- :kind! FirstParen "(agag)(222)"
type family FirstParen (s :: Symbol) :: Symbol where
    FirstParen s = Concat (DropLast( TakeFrstParen (LDrop 1 (ParentCnt (ToList s)))))


type family LLengh (s :: [k]) :: Nat where
    LLengh '[] = 0
    LLengh (x ': xs) = 1 + LLengh xs

type family Length (s :: Symbol) :: Nat where  
    Length x = LLengh (ToList x)

    

-- map fst . dropWhile ((0< ) . snd) . drop 1 . parenCnt $ "((agag)(sagg))(agaga) "
-- "(agaga) "
-- map fst . takeWhile ((0< ) . snd) . drop 1 . parenCnt $ "((agag)(sagg))(agaga) "
-- "(agag)(sagg))"
-- dropLast . takeFrstParen . drop 1 . parenCnt $ "((agag)(sagg))(agaga)"

parenCnt :: String -> [(Char, Int)]
parenCnt [] = []
parenCnt ('(' : xs) = ('(', parenCntFstCnt (parenCnt xs) - 1) : parenCnt xs
parenCnt (')' : xs) = (')', parenCntFstCnt (parenCnt xs) + 1) : parenCnt xs
parenCnt (x : xs) = (x, parenCntFstCnt (parenCnt xs) ) : parenCnt xs

parenCntFstCnt :: [(Char, Int)] -> Int
parenCntFstCnt [] = 0
parenCntFstCnt ((c,i) : _) = i

takeFrstParen :: [(Char, Int)] -> [Char]
takeFrstParen [] = []
takeFrstParen ((_, 0) : _) = []
takeFrstParen ((a, p) : xs) = a : takeFrstParen xs

dropLast :: [Char] -> [Char]
dropLast [] = []
dropLast [x] = []
dropLast (x:xs) = x : dropLast xs
             

