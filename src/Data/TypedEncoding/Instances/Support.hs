
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Exports for instance creation.
-- 
-- Contains typical things needed when implementing
-- encoding, decoding, recreate, or type to string conversions.
module Data.TypedEncoding.Instances.Support (
    module Data.TypedEncoding.Instances.Support
    -- * Types
    , module Data.TypedEncoding.Internal.Types
    -- * Classes
    , module Data.TypedEncoding.Internal.Class
    -- * Combinators
    , module Data.TypedEncoding.Internal.Instances.Combinators
    -- * Type level conveniences
    , module Data.TypedEncoding.Internal.Util.TypeLits
   ) where
import           Data.TypedEncoding.Internal.Types
import           Data.TypedEncoding.Internal.Class 
import           Data.TypedEncoding.Internal.Instances.Combinators 
import           Data.TypedEncoding.Internal.Util.TypeLits

import           GHC.TypeLits

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications


-- | Universal decode for all "r-" types
decFR :: (IsR s ~ 'True, Applicative f) => 
            Enc (s ': xs) c str -> f (Enc xs c str) 
decFR = implTranP id 


-- | 
-- Manual recreate step combinator converting @"r-"@ encode function to a recreate step.
--
-- For "r-" encoding recreate and encode are the same other than the exception type used. 
--
-- The convention in @typed-encoding@ is to implement encode and convert it to recreate.
recWithEncR :: forall (s :: Symbol) xs c str . (IsR s ~ 'True) 
                       => (Enc xs c str -> Either EncodeEx (Enc (s ': xs) c str)) 
                       -> Enc xs c str -> Either RecreateEx (Enc (s ': xs) c str)
recWithEncR = unsafeRecWithEncR


unsafeRecWithEncR :: forall (s :: Symbol) xs c str .
                       (Enc xs c str -> Either EncodeEx (Enc (s ': xs) c str)) 
                       -> Enc xs c str -> Either RecreateEx (Enc (s ': xs) c str)
unsafeRecWithEncR fn = either (Left . encToRecrEx) Right . fn

-- |
-- >>> :kind! IsR "r-UPPER"
-- ...
-- ... 'True
--
-- >>> :kind! IsR "do-UPPER"
-- ...
-- = (TypeError ... 
type family IsR (s :: Symbol) :: Bool where
    IsR s = AcceptEq ('Text "Not restriction encoding " ':<>: ShowType s ) (CmpSymbol "r-" (Take 2 s))


type family IsROrEmpty (s :: Symbol) :: Bool where
    IsROrEmpty "" = True
    IsROrEmpty x  = IsR x
