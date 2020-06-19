
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Combinators for creating encoding using existing encodings.
-- 
-- @since 0.4.2.0
module Data.TypedEncoding.Instances.Support.Bool where

import           Data.TypedEncoding.Combinators.Unsafe
import           Data.TypedEncoding.Common.Types.Enc
import           Data.Proxy
import           Data.TypedEncoding.Common.Types
import           GHC.TypeLits

-- import           Data.TypedEncoding.Instances.Restriction.BoundedAlphaNums (encFBan)
-- import           Data.TypedEncoding

-- $setup
-- >>> :set -XDataKinds -XFlexibleInstances -XFlexibleContexts -XOverloadedStrings -XTypeApplications -XScopedTypeVariables
-- >>> import           Data.TypedEncoding
-- >>> import           Data.TypedEncoding.Instances.Restriction.BoundedAlphaNums (encFBan)

-- | Defines new encoding by specifying 2 encodings, one needs to succeed.
--  
-- @since 0.4.2.0
implEncOr' :: forall alg alg1 alg2 nm nm1 nm2 c str . (KnownSymbol nm) =>
               Encoding (Either EncodeEx) nm1 alg1 c str 
               -> Encoding (Either EncodeEx) nm2 alg2 c str 
               -> Encoding (Either EncodeEx) nm alg c str 
implEncOr' enc1 enc2 = UnsafeMkEncoding Proxy f
   where 
       f :: forall xs . Enc xs c str -> Either EncodeEx (Enc (nm ': xs) c str)
       f enc = 
           case runEncoding' @alg1 @nm1 enc1 enc of
               Right r -> Right $ withUnsafeCoerce id r
               Left (EncodeEx _ err1) -> 
                   case runEncoding' @alg2 @nm2 enc2 enc of 
                       Right r -> Right $ withUnsafeCoerce id r
                       Left (EncodeEx _ err2) -> Left $ EncodeEx (Proxy :: Proxy nm) (err1, err2)

implEncOr :: forall nm nm1 nm2 c str . (KnownSymbol nm) =>
               Encoding (Either EncodeEx) nm1 nm1 c str 
               -> Encoding (Either EncodeEx) nm2 nm2 c str 
               -> Encoding (Either EncodeEx) nm nm c str 
implEncOr = implEncOr' @nm @nm1 @nm2             



-- |
-- 
-- >>> let tst = _implEncOr @"r-tst:999(9)" @"r-ban:9999" @"r-ban:999" @() @String encFBan encFBan
--
-- >>> fmap displ $ _runEncoding tst $ toEncoding () "123"
-- Right "Enc '[r-tst:999(9)] () (String 123)"
--
-- >>> fmap displ $ _runEncoding tst $ toEncoding () "1234"
-- Right "Enc '[r-tst:999(9)] () (String 1234)"
--
-- >>> fmap displ $ _runEncoding tst $ toEncoding () "12345"
-- Left (EncodeEx "r-tst:999(9)" (("Input list has wrong size expecting 4 but length \"12345\" == 5","Input list has wrong size expecting 3 but length \"12345\" == 5")))
--
-- @since 0.4.2.0
_implEncOr :: forall nm nm1 nm2 c str alg alg1 alg2. 
              (
                KnownSymbol nm
              , Algorithm nm alg
              , Algorithm nm1 alg1
              , Algorithm nm2 alg2
               ) =>
               Encoding (Either EncodeEx) nm1 alg1 c str 
               -> Encoding (Either EncodeEx) nm2 alg2 c str 
               -> Encoding (Either EncodeEx) nm alg c str 
_implEncOr = implEncOr' @alg @alg1 @alg2      


-- | Defines new encoding by specifying 2 encodings, both needs to succeed and produce the same payload.
--  
-- @since 0.4.2.0
implEncAnd' :: forall alg alg1 alg2 nm nm1 nm2 c str . (KnownSymbol nm, Eq str) =>
               Encoding (Either EncodeEx) nm1 alg1 c str 
               -> Encoding (Either EncodeEx) nm2 alg2 c str 
               -> Encoding (Either EncodeEx) nm alg c str 
implEncAnd' enc1 enc2 = UnsafeMkEncoding Proxy f
   where 
       f :: forall xs . Enc xs c str -> Either EncodeEx (Enc (nm ': xs) c str)
       f enc = 
            case (runEncoding' @alg1 @nm1 enc1 enc, runEncoding' @alg2 @nm2 enc2 enc) of
                (Right r1, Right r2) -> if getPayload r1 == getPayload r2 
                                       then Right $ withUnsafeCoerce id r1
                                       else Left $ EncodeEx (Proxy :: Proxy nm) "Non-matching encodings"
                (Left (EncodeEx _ err1), Left (EncodeEx _ err2)) -> Left $ EncodeEx (Proxy :: Proxy nm) (err1, err2)
                (Left (EncodeEx _ err), _) -> Left $ EncodeEx (Proxy :: Proxy nm) (err, ())
                (_, Left (EncodeEx _ err)) -> Left $ EncodeEx (Proxy :: Proxy nm) ((), err)

implEncAnd :: forall nm nm1 nm2 c str . (KnownSymbol nm, Eq str) =>
               Encoding (Either EncodeEx) nm1 nm1 c str 
               -> Encoding (Either EncodeEx) nm2 nm2 c str 
               -> Encoding (Either EncodeEx) nm nm c str 
implEncAnd = implEncAnd' @nm @nm1 @nm2             

-- | 
--
-- >>> let tst2 = _implEncAnd @"r-tst:99" @"r-ban:9Z" @"r-ban:Z9" @() @String encFBan encFBan
--
-- >>> fmap displ $ _runEncoding tst2 $ toEncoding () "99"
-- Right "Enc '[r-tst:99] () (String 99)"
--
-- >>> fmap displ $ _runEncoding tst2 $ toEncoding () "AB"
-- Left (EncodeEx "r-tst:99" (("'A' not bounded by '9'","'B' not bounded by '9'")))
--
-- @since 0.4.2.0
_implEncAnd :: forall nm nm1 nm2 c str alg alg1 alg2. 
              (
                KnownSymbol nm
              , Eq str
              , Algorithm nm alg
              , Algorithm nm1 alg1
              , Algorithm nm2 alg2
               ) =>
               Encoding (Either EncodeEx) nm1 alg1 c str 
               -> Encoding (Either EncodeEx) nm2 alg2 c str 
               -> Encoding (Either EncodeEx) nm alg c str 
_implEncAnd = implEncAnd' @alg @alg1 @alg2

-- | Defines new encoding which succeeds only if specified encoding fails.
-- It that happens, it applies given transformation function.    
--  
-- @since 0.4.2.0
implEncNot' :: forall alg alg1 nm nm1 c str . (KnownSymbol nm) =>
               (str -> str)
               -> Encoding (Either EncodeEx) nm1 alg1 c str 
               -> Encoding (Either EncodeEx) nm alg c str 
implEncNot' fn enc1 = UnsafeMkEncoding Proxy f
   where 
       f :: forall xs . Enc xs c str -> Either EncodeEx (Enc (nm ': xs) c str)
       f enc = 
           case runEncoding' @alg1 @nm1 enc1 enc of
               Left _ -> Right $ withUnsafeCoerce fn enc
               Right _ ->  Left $ EncodeEx (Proxy :: Proxy nm) "Negated encoding succeeded"

implEncNot :: forall nm nm1 c str . (KnownSymbol nm) =>
               (str -> str)
               -> Encoding (Either EncodeEx) nm1 nm1 c str 
               -> Encoding (Either EncodeEx) nm nm c str 
implEncNot = implEncNot' @nm @nm1             

_implEncNot :: forall nm nm1 c str alg alg1 . 
              (
                KnownSymbol nm
              , Algorithm nm alg
              , Algorithm nm1 alg1
               ) =>
               (str -> str)
               -> Encoding (Either EncodeEx) nm1 alg1 c str 
               -> Encoding (Either EncodeEx) nm alg c str 
_implEncNot = implEncNot' @alg @alg1

-- | Defines restriction encoding that succeeds when specified encoding fails
--  
--
-- >>> let tst3 = _implREncNot @"r-tstnot:99" @"r-ban:99" @() @String encFBan 
--
-- >>> fmap displ $ _runEncoding tst3 $ toEncoding () "AA"
-- Right "Enc '[r-tstnot:99] () (String AA)"
--
-- >>> fmap displ $ _runEncoding tst3 $ toEncoding () "99"
-- Left (EncodeEx "r-tstnot:99" ("Negated encoding succeeded"))
--
-- @since 0.4.2.0
_implREncNot :: forall nm nm1 c str alg alg1 . 
              (
                KnownSymbol nm
              , Algorithm nm alg
              , Algorithm nm1 alg1
               ) =>
               Encoding (Either EncodeEx) nm1 alg1 c str 
               -> Encoding (Either EncodeEx) nm alg c str 
_implREncNot = implEncNot' @alg @alg1 id