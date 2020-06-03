
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

import           Data.TypedEncoding.Instances.Restriction.BoundedAlphaNums (encFBan)

-- $setup
-- >>> :set -XDataKinds -XFlexibleInstances -XFlexibleContexts -XOverloadedStrings -XTypeApplications -XScopedTypeVariables
-- import           Data.TypedEncoding
-- import           Data.TypedEncoding.Instances.Restriction.BoundedAlphaNums ()

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


tst = _implEncOr @"r-tst:999(9)" @"r-ban:9999" @"r-ban:999" @() @String encFBan encFBan

-- |
-- 
-- >>> let tst = _implEncOr @"r-tst:999(9)" @"r-ban:9999" @"r-ban:999" @() @String encFBan encFBan
--
-- >>> fmap displ $ _runEncoding tst $ toEncoding () "123"
-- Right "Enc '[r-tst:999(9)] () (String 123)"
--
-- >>> fmap displ $ _runEncoding tst $ toEncoding () "1234"
-- fmap displ $ _runEncoding tst $ toEncoding () "1234"
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