
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Boolean algebra on encodings
--
-- @since 0.2.1.0
-- 
-- /(Experimental)/ This module was not converted to v0.3.
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
-- This is very much in beta state.
module Data.TypedEncoding.Instances.Restriction.Bool where 


import           GHC.TypeLits
import           Data.Proxy
import           Data.Symbol.Ascii

import           Data.TypedEncoding
import           Data.TypedEncoding.Instances.Support
import           Data.TypedEncoding.Instances.Support.Deprecated



-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications
-- >>> import qualified Data.Text as T
-- >>> import           Data.TypedEncoding.Instances.Restriction.Misc()


-- |
-- See examples in 'encBoolOrRight''
encBoolOrLeft :: forall f s t xs c str . (
    BoolOpIs s "or" ~ 'True
    -- IsBoolOr s ~ 'True
    , Functor f
    , LeftTerm s ~ t
    ) => (Enc xs c str -> f (Enc (t ': xs) c str)) -> Enc xs c str -> f (Enc (s ': xs) c str)  
encBoolOrLeft = implChangeAnn 

-- |
-- See examples in 'encBoolOrRight''
encBoolOrLeft' :: forall f s t xs c str . (
    BoolOpIs s "or" ~ 'True
    -- IsBoolOr s ~ 'True
    , Functor f
    , LeftTerm s ~ t
    , Encode f t t c str
    ) => Enc xs c str -> f (Enc (s ': xs) c str)  
encBoolOrLeft' = encBoolOrLeft'' @t

encBoolOrLeft'' :: forall alg f s t xs c str . (
    BoolOpIs s "or" ~ 'True
    -- IsBoolOr s ~ 'True
    , Functor f
    , LeftTerm s ~ t
    , Encode f t alg c str
    ) => Enc xs c str -> f (Enc (s ': xs) c str)  
encBoolOrLeft'' = encBoolOrLeft (encodeF' @alg @t @xs @f)

-- |
-- 
encBoolOrRight :: forall f s t xs c str . (
    BoolOpIs s "or" ~ 'True
    -- IsBoolOr s ~ 'True
    , Functor f
    , RightTerm s ~ t
    ) => (Enc xs c str -> f (Enc (t ': xs) c str)) -> Enc xs c str -> f (Enc (s ': xs) c str)  
encBoolOrRight = implChangeAnn 

-- |
-- >>> :{ 
-- let tst1, tst2, tst3 :: Either EncodeEx (Enc '["boolOr:(r-Word8-decimal)(r-Int-decimal)"] () T.Text)
--     tst1 = encBoolOrLeft' . toEncoding () $ "212" 
--     tst2 = encBoolOrRight' . toEncoding () $ "1000000" 
--     tst3 = encBoolOrLeft' . toEncoding () $ "1000000"
-- :}
-- 
-- >>> tst1 
-- Right (MkEnc Proxy () "212")
--
-- >>> tst2
-- Right (MkEnc Proxy () "1000000")
--
-- >>> tst3
-- Left (EncodeEx "r-Word8-decimal" ("Payload does not satisfy format Word8-decimal: 1000000"))
encBoolOrRight' :: forall f s t xs c str . (
    BoolOpIs s "or" ~ 'True
    -- IsBoolOr s ~ 'True
    , Functor f
    , RightTerm s ~ t
    , Encode f t t c str
    ) => Enc xs c str -> f (Enc (s ': xs) c str)  
encBoolOrRight' = encBoolOrRight'' @t

encBoolOrRight'' :: forall alg f s t xs c str . (
    BoolOpIs s "or" ~ 'True
    -- IsBoolOr s ~ 'True
    , Functor f
    , RightTerm s ~ t
    , Encode f t alg c str
    ) => Enc xs c str -> f (Enc (s ': xs) c str)  
encBoolOrRight'' = encBoolOrRight (encodeF' @alg @t @xs @f) 

encBoolAnd :: forall f s t1 t2 xs c str . (
    BoolOpIs s "and" ~ 'True 
    , KnownSymbol s
    -- IsBoolAnd s ~ 'True
    , f ~ Either EncodeEx
    , Eq str
    , LeftTerm s ~ t1
    , RightTerm s ~ t2
    ) => 
    (Enc xs c str -> f (Enc (t1 ': xs) c str)) 
    -> (Enc xs c str -> f (Enc (t2 : xs) c str)) 
    -> Enc xs c str -> f (Enc (s ': xs) c str)  
encBoolAnd fnl fnr en0 =  
       let 
           eent1 = fnl en0
           eent2 = fnr en0
           p = (Proxy :: Proxy s)
       in 
           case (eent1, eent2) of
               (Right ent1, Right ent2) -> 
                   if getPayload ent1 == getPayload ent2
                   then Right . withUnsafeCoerce id $ ent1 
                   else Left $ EncodeEx p "Left - right encoding do not match"                   
               (_, _) -> mergeErrs (emptyEncErr p) (mergeEncodeEx p) eent1 eent2

-- |
-- @"boolOr:(enc1)(enc2)"@ contains strings that encode the same way under both encodings.
-- for example  @"boolOr:(r-UPPER)(r-lower)"@ valid elements would include @"123-34"@ but not @"abc"@
--
-- >>> :{
-- let tst1, tst2 :: Either EncodeEx (Enc '["boolAnd:(r-Word8-decimal)(r-Int-decimal)"] () T.Text)
--     tst1 = encBoolAnd' . toEncoding () $ "234"
--     tst2 = encBoolAnd' . toEncoding () $ "100000"
-- :}
-- 
-- >>> tst1
-- Right (MkEnc Proxy () "234")
-- >>> tst2
-- Left (EncodeEx "r-Word8-decimal" ("Payload does not satisfy format Word8-decimal: 100000"))
encBoolAnd' :: forall s t1 t2 xs c str . (
    BoolOpIs s "and" ~ 'True 
    , KnownSymbol s
    -- IsBoolAnd s ~ 'True
    , Eq str
    , LeftTerm s ~ t1
    , RightTerm s ~ t2
    , Encode (Either EncodeEx) t1 t1 c str
    , Encode (Either EncodeEx) t2 t2 c str
    ) => Enc xs c str -> Either EncodeEx (Enc (s ': xs) c str)  
encBoolAnd'  = encBoolAnd'' @t1 @t2

encBoolAnd'' :: forall al1 al2 s t1 t2 xs c str . (
    BoolOpIs s "and" ~ 'True 
    , KnownSymbol s
    -- IsBoolAnd s ~ 'True
    , Eq str
    , LeftTerm s ~ t1
    , RightTerm s ~ t2
    , Encode (Either EncodeEx) t1 al1 c str
    , Encode (Either EncodeEx) t2 al2 c str
    ) => Enc xs c str -> Either EncodeEx (Enc (s ': xs) c str)  
encBoolAnd''  = encBoolAnd (encodeF' @al1 @t1 @xs) (encodeF' @al2 @t2 @xs) 


-- tst1, tst2 :: Either EncodeEx (Enc '["boolNot:(r-Word8-decimal)"] () T.Text)
-- tst1 = encBoolNot' . toEncoding () $ "334"
-- tst2 = encBoolNot' . toEncoding () $ "127"

encBoolNot :: forall s t xs c str . (
    BoolOpIs s "not" ~ 'True
    , KnownSymbol s
    , FirstTerm s ~ t
    , Restriction t 
    ) => (Enc xs c str -> Either EncodeEx (Enc (t ': xs) c str)) -> Enc xs c str -> Either EncodeEx (Enc (s ': xs) c str)  
encBoolNot fn en0 = 
        let 
           een = fn en0
           p = (Proxy :: Proxy s)
           pt = (Proxy :: Proxy t)
        in 
           case een of
               Left _ -> Right . withUnsafeCoerce id $ en0
               Right _ -> Left $ EncodeEx p $ "Encoding " ++ symbolVal pt ++ " succeeded"  
 

-- |
-- >>> :{
-- let tst1, tst2 :: Either EncodeEx (Enc '["boolNot:(r-Word8-decimal)"] () T.Text)
--     tst1 = encBoolNot' . toEncoding () $ "334"
--     tst2 = encBoolNot' . toEncoding () $ "127"
-- :}
--
-- >>> tst1
-- Right (MkEnc Proxy () "334")
-- >>> tst2
-- Left (EncodeEx "boolNot:(r-Word8-decimal)" ("Encoding r-Word8-decimal succeeded"))
encBoolNot' :: forall s t xs c str . (
    BoolOpIs s "not" ~ 'True
    , KnownSymbol s
    , FirstTerm s ~ t
    , KnownSymbol t
    , Restriction t 
    , Encode (Either EncodeEx) t t c str
    ) => Enc xs c str -> Either EncodeEx (Enc (s ': xs) c str)  
encBoolNot' = encBoolNot'' @t

encBoolNot'' :: forall alg s t xs c str . (
    BoolOpIs s "not" ~ 'True
    , KnownSymbol s
    , FirstTerm s ~ t
    , Restriction t  
    , Encode (Either EncodeEx) t alg c str
    ) => Enc xs c str -> Either EncodeEx (Enc (s ': xs) c str)  
encBoolNot'' = encBoolNot (encodeF' @alg @t @xs)

-- | 
-- Decodes boolean expression if all leaves are @"r-"@
decBoolR :: forall f xs t s c str . (
    NestedR s  ~ 'True
    , Applicative f
    ) => Enc (s ': xs) c str -> f (Enc xs c str)  

decBoolR = implTranP id 


-- | needs to be converted to v0.3 style
recWithEncBoolR :: forall (s :: Symbol) xs c str . (NestedR s ~ 'True) 
                       => (Enc xs c str -> Either EncodeEx (Enc (s ': xs) c str)) 
                       -> Enc xs c str -> Either RecreateEx (Enc (s ': xs) c str)
recWithEncBoolR = unsafeRecWithEncR

unsafeRecWithEncR :: forall (s :: Symbol) xs c str .
                       (Enc xs c str -> Either EncodeEx (Enc (s ': xs) c str)) 
                       -> Enc xs c str -> Either RecreateEx (Enc (s ': xs) c str)
unsafeRecWithEncR fn = either (Left . encToRecrEx) Right . fn

-- * Type family based parser 

-- |
-- >>> :kind! BoolOpIs "boolAnd:(someenc)(otherenc)" "and"
-- ...
-- = 'True
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
    IsBool s = AcceptEq ('Text "Not boolean encoding " ':<>: ShowType s ) (CmpSymbol "bool" (Take 4 s))

-- |
-- 
-- >>> :kind! NestedR "boolOr:(r-abc)(r-cd)"
-- ...
-- = 'True
--
-- >>> :kind! NestedR "boolOr:(boolAnd:(r-ab)(r-ac))(boolNot:(r-cd))"
-- ...
-- = 'True
--
-- >>> :kind! NestedR "boolOr:(boolAnd:(r-ab)(ac))(boolNot:(r-cd))"
-- ...
-- ... (TypeError ...)
-- ...
type family NestedR (s :: Symbol) :: Bool where
    NestedR "" = 'True -- RightTerm, LeftTerm return "" on "r-"
    NestedR s = Or (IsROrEmpty s) 
                   (And (IsBool s) (And (NestedR (LeftTerm s)) (NestedR (RightTerm s)))) 

-- Value level check
--
-- nestedR :: String -> Bool
-- nestedR s = isROrEmpty s || isBool s && (nestedR (leftTerm s) && nestedR (rightTerm s))

-- isBool ('b' :'o' : 'o' : _) = True
-- isBool _ = False

-- isROrEmpty "" = True
-- isROrEmpty ('r' : '-' : _) = True
-- isROrEmpty _ = False


type family FirstTerm (s :: Symbol) :: Symbol where
    FirstTerm s = LeftTerm s

-- | 
-- returns "" for unary operator
type family SecondTerm (s :: Symbol) :: Symbol where
    SecondTerm s = RightTerm s


-- |
-- >>> :kind! LeftTerm "boolSomeOp:(agag)(222)"
-- ...
-- = "agag"
--
-- >>> :kind! LeftTerm "r-Int-decimal"
-- ...
-- = ""
type family LeftTerm (s :: Symbol) :: Symbol where
    LeftTerm s = Concat (LDropLast( LTakeFstParen (LParenCnt (ToList s))))

-- |
-- >>> :kind! RightTerm "boolSomeOp:(agag)(222)"
-- ...
-- = "222"
--
-- >>> :kind! RightTerm "r-Int-decimal"
-- ...
-- = ""
type family RightTerm (s :: Symbol) :: Symbol where
    RightTerm s = Concat (LDropLast (LTakeSndParen 0 (LParenCnt (ToList s))))


type family LDropLast (s :: [Symbol]) :: [Symbol] where
    LDropLast '[] = '[]
    LDropLast '[x] = '[]
    LDropLast (x ': xs) = x ': LDropLast xs


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

-- -- * (Example) Value level equivalend of type family parsers


-- -- map snd . parenCnt $ "((AGA)(bcd))(123) "
-- -- dropLast . takeFstParen . parenCnt $ "((AGA)(bcd))(123)" 
-- -- dropLast . takeSndParen 0 . parenCnt $ "((AGA)(bcd))(123)" 

-- leftTerm :: String -> String
-- leftTerm = dropLast . takeFstParen . parenCnt

-- rightTerm :: String -> String
-- rightTerm = dropLast . takeSndParen 0 . parenCnt

-- parenCnt :: String -> [(Char, Int)]
-- parenCnt [] = []
-- parenCnt ('(' : xs) = parenCntHelper ('(', -1) (parenCnt xs) --('(', parenCntFstCnt (parenCnt xs) - 1) : parenCnt xs
-- parenCnt (')' : xs) = parenCntHelper (')', 1) (parenCnt xs)  -- (')', parenCntFstCnt (parenCnt xs) + 1) : parenCnt xs
-- parenCnt (x : xs) = parenCntHelper (x, 0) (parenCnt xs) -- (x, parenCntFstCnt (parenCnt xs) ) : parenCnt xs


-- parenCntHelper :: (Char, Int) -> [(Char, Int)]  -> [(Char, Int)]
-- parenCntHelper (x, k) [] = [(x,k)]
-- parenCntHelper (x, k) ((c,i) : xs) = (x, i + k): (c,i) : xs


-- takeFstParen :: [(Char, Int)] -> [Char]
-- takeFstParen [] = []
-- takeFstParen ((_, 0) : xs) = takeFstParen xs
-- takeFstParen ((')', 1) : _) = [')']
-- takeFstParen ((a, p) : xs) = a : takeFstParen xs

-- takeSndParen :: Int -> [(Char, Int)] -> [Char]
-- takeSndParen _ [] = []
-- takeSndParen 0 ((')', 1) : xs) = takeSndParen 1 xs
-- takeSndParen 1 ((_, 0) : xs) = takeSndParen 1 xs
-- takeSndParen 0 ((_, _) : xs) = takeSndParen 0 xs
-- takeSndParen 1 ((a, _) : xs) = a : takeSndParen 1 xs
-- takeSndParen _ _ = []

-- dropLast :: [Char] -> [Char]
-- dropLast [] = []
-- dropLast [x] = []
-- dropLast (x:xs) = x : dropLast xs
             
