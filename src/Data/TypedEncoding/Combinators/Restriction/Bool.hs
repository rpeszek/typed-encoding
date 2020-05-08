
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Boolean algebra on encodings
-- 
-- == Grammar
-- 
-- Simple grammar requires boolean terms to be included in parentheses
--
-- @
-- bool[BinaryOp]:(leftTerm)(rightTerm)
-- bool[UnaryOp]:(term)
-- @
--
-- Expected behavior is described next to corresponding combinator.
--
-- Typeclass encoding is not used to avoid instance overlapping.
--
-- Currently only encoding and some decoding combinators are defined.
-- 
-- This is very much in beta state.
--
-- @since 0.2.1.0
module Data.TypedEncoding.Combinators.Restriction.Bool where 


import           GHC.TypeLits
import           Data.Proxy
import           Data.Symbol.Ascii

import           Data.TypedEncoding
import           Data.TypedEncoding.Instances.Support
import           Data.TypedEncoding.Internal.Util.TypeLits
import           Data.TypedEncoding.Combinators.Restriction.Common


-- import qualified Data.Text as T
-- import           Data.TypedEncoding.Instances.Restriction.BoundedAlphaNums ()
-- import           Data.TypedEncoding.Instances.Restriction.Common()


-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications
-- >>> import qualified Data.Text as T
-- >>> import           Data.TypedEncoding.Instances.Restriction.BoundedAlphaNums ()
-- >>> import           Data.TypedEncoding.Instances.Restriction.Common()

-- |
-- See examples in 'encodeBoolOrRt'
encodeBoolOrLt :: forall f s t xs c str . (
    BoolOpIs s "or" ~ 'True
    -- IsBoolOr s ~ 'True
    , Functor f
    , LeftTerm s ~ t
    , EncodeF f (Enc xs c str) (Enc (t ': xs) c str) 
    ) => Enc xs c str -> f (Enc (s ': xs) c str)  
encodeBoolOrLt = implChangeAnn (encodeF @f @(Enc xs c str) @(Enc (t ': xs) c str)) 

-- |
-- 
-- >>> :{ 
-- let tst1, tst2, tst3 :: Either EncodeEx (Enc '["boolOr:(r-ban:999-999-9999)(r-ban:(999) 999-9999)"] () T.Text)
--     tst1 = encodeBoolOrLt . toEncoding () $ "212-222-3333" 
--     tst2 = encodeBoolOrRt . toEncoding () $ "(212) 222-3333" 
--     tst3 = encodeBoolOrRt . toEncoding () $ "212 222 3333"
-- :}
-- 
-- >>> tst1 
-- Right (MkEnc Proxy () "212-222-3333")
-- >>> tst2
-- Right (MkEnc Proxy () "(212) 222-3333")
-- >>> tst3
-- Left (EncodeEx "r-ban:(999) 999-9999" ("Input list has wrong size expecting 14 but length \"212 222 3333\" == 12"))
encodeBoolOrRt :: forall f s t xs c str . (
    BoolOpIs s "or" ~ 'True
    -- IsBoolOr s ~ 'True
    , Functor f
    , RightTerm s ~ t
    , EncodeF f (Enc xs c str) (Enc (t ': xs) c str) 
    ) => Enc xs c str -> f (Enc (s ': xs) c str)  
encodeBoolOrRt = implChangeAnn (encodeF @f @(Enc xs c str) @(Enc (t ': xs) c str)) 

-- |
-- @"boolOr:(enc1)(enc2)"@ contains strings that encode the same way under both encodings.
-- for example  @"boolOr:(r-UPPER)(r-lower)"@ valid elements would include @"123-34"@ but not @"abc"@
--
-- >>> :{
-- let tst1, tst2 :: Either EncodeEx (Enc '["boolAnd:(r-ban:255)(r-Word8-decimal)"] () T.Text)
--     tst1 = encodeBoolAnd . toEncoding () $ "234"
--     tst2 = encodeBoolAnd . toEncoding () $ "127"
-- :}
-- 
-- >>> tst1
-- Right (MkEnc Proxy () "234")
-- >>> tst2
-- Left (EncodeEx "r-ban:255" ("'7' not boulded by '5'"))
encodeBoolAnd :: forall f s t1 t2 xs c str . (
    BoolOpIs s "and" ~ 'True 
    , KnownSymbol s
    -- IsBoolAnd s ~ 'True
    , f ~ Either EncodeEx
    , Eq str
    , LeftTerm s ~ t1
    , RightTerm s ~ t2
    , EncodeF f (Enc xs c str) (Enc (t1 ': xs) c str) 
    , EncodeF f (Enc xs c str) (Enc (t2 ': xs) c str) 
    ) => Enc xs c str -> f (Enc (s ': xs) c str)  
encodeBoolAnd en0 =  
       let 
           eent1 :: f (Enc (t1 ': xs) c str) = encodeF en0
           eent2 :: f (Enc (t2 ': xs) c str) = encodeF en0
           p = (Proxy :: Proxy s)
       in 
           case (eent1, eent2) of
               (Right ent1, Right ent2) -> 
                   if getPayload ent1 == getPayload ent2
                   then Right . withUnsafeCoerce id $ ent1 
                   else Left $ EncodeEx p "Left - right encoding do not match"                   
               (_, _) -> mergeErrs (emptyEncErr p) (mergeEncodeEx p) eent1 eent2



-- tst1, tst2 :: Either EncodeEx (Enc '["boolNot:(r-Word8-decimal)"] () T.Text)
-- tst1 = encodeBoolNot . toEncoding () $ "334"
-- tst2 = encodeBoolNot . toEncoding () $ "127"

-- |
-- >>> :{
-- let tst1, tst2 :: Either EncodeEx (Enc '["boolNot:(r-Word8-decimal)"] () T.Text)
--     tst1 = encodeBoolNot . toEncoding () $ "334"
--     tst2 = encodeBoolNot . toEncoding () $ "127"
-- :}
--
-- >>> tst1
-- Right (MkEnc Proxy () "334")
-- >>> tst2
-- Left (EncodeEx "boolNot:(r-Word8-decimal)" ("Encoding r-Word8-decimal succeeded"))
encodeBoolNot :: forall f s t xs c str . (
    BoolOpIs s "not" ~ 'True
    , KnownSymbol s
    , f ~ Either EncodeEx
    , FirstTerm s ~ t
    , KnownSymbol t
    , IsR t ~ 'True
    , EncodeF f (Enc xs c str) (Enc (t ': xs) c str) 
    ) => Enc xs c str -> f (Enc (s ': xs) c str)  
encodeBoolNot en0 = 
        let 
           een :: f (Enc (t ': xs) c str) = encodeF en0
           p = (Proxy :: Proxy s)
           pt = (Proxy :: Proxy t)
        in 
           case een of
               Left _ -> Right . withUnsafeCoerce id $ en0
               Right _ -> Left $ EncodeEx p $ "Encoding " ++ symbolVal pt ++ " succeeded"  
 

-- | 
-- Decode both @not@ of r- encoding
decodeBoolNotR :: forall f xs t s c str . (
    BoolOpIs s "not" ~ 'True
    , Applicative f
    , FirstTerm s ~ t
    , IsR t ~ 'True
    , DecodeF f (Enc (t ': xs) c str) (Enc xs c str) 
    ) => Enc (s ': xs) c str -> f (Enc xs c str)  

decodeBoolNotR = implTranP id 

-- | 
-- Decode both @and@ and @or@ of r- encodings
decodeBoolBinR :: forall f xs t1 t2 s c str . (
    IsBool s ~ 'True
    , Applicative f
    , LeftTerm s ~ t1
    , RightTerm s ~ t2
    , IsR t1 ~ 'True
    , IsR t2 ~ 'True
    , DecodeF f (Enc (t1 ': xs) c str) (Enc xs c str) 
    , DecodeF f (Enc (t2 ': xs) c str) (Enc xs c str) 
    ) => Enc (s ': xs) c str -> f (Enc xs c str)  

decodeBoolBinR = implTranP id 


-- * Type family based parser 

-- |
-- 
type family BoolOpIs (s :: Symbol) (op :: Symbol) :: Bool where
    BoolOpIs s op = AcceptEq ('Text "Invalid bool encoding " ':<>: ShowType s ) (CmpSymbol (BoolOp s) op)

-- | 
-- This works fast with @!kind@ but is much slower in declaration 
-- :kind! BoolOp "boolOr:()()"
type family BoolOp (s :: Symbol) :: Symbol where
    BoolOp s = Fst (BoolOpHelper (Dupl s))
        -- ToLower (TakeUntil (Drop 4 s) ":")

type family BoolOpHelper (x :: (Symbol, Symbol)) :: (Symbol, Bool) where
    BoolOpHelper ('(,) s1 s2) = '(,) (ToLower (TakeUntil (Drop 4 s1) ":")) ( AcceptEq ('Text "Invalid bool encoding " ':<>: ShowType s2 ) (CmpSymbol "bool" (Take 4 s2)))


type family IsBool (s :: Symbol) :: Bool where
    IsR s = AcceptEq ('Text "Not boolean encoding " ':<>: ShowType s ) (CmpSymbol "bool" (Take 4 s))

-- 
type family FirstTerm (s :: Symbol) :: Symbol where
    FirstTerm s = LeftTerm s

-- | 
-- returns "" for uniary operator
type family SecondTerm (s :: Symbol) :: Symbol where
    FirstTerm s = RightTerm s

-- dropLast . takeFstParen . parenCnt $ "((AGA)(bcd))(123)" 
-- :kind! LeftTerm "(agag)(222)"
type family LeftTerm (s :: Symbol) :: Symbol where
    LeftTerm s = Concat (LDropLast( LTakeFstParen (LParenCnt (ToList s))))

-- dropLast . takeSndParen 0 . parenCnt $ "((agag)(sagg))(agaga)"
-- :kind! RightTerm "(agag)(222)"
--
type family RightTerm (s :: Symbol) :: Symbol where
    RightTerm s = Concat (LDropLast (LTakeSndParen 0 (LParenCnt (ToList s))))


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
             
