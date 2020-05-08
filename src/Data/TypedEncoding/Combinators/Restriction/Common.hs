
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE KindSignatures  #-}s
{-# LANGUAGE UndecidableInstances #-}

module Data.TypedEncoding.Combinators.Restriction.Common where 

import           GHC.TypeLits
import           Data.TypedEncoding.Internal.Util.TypeLits
import           Data.TypedEncoding.Instances.Support

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications


-- | Universal decode for all "r-" types
decodeFR :: (IsR s ~ 'True, Applicative f) => 
            Enc (s ': xs) c str -> f (Enc xs c str) 
decodeFR = implTranP id 


-- | 
-- Manual recreate step compbinator converting typical encode function to a recreate step
recreateWithEncode :: (Enc xs c str -> Either EncodeEx (Enc (s ': xs) c str)) -> Enc xs c str -> Either RecreateEx (Enc (s ': xs) c str)
recreateWithEncode fn = either (Left . encToRecrEx) Right . fn

-- |
-- :kind! IsR "r-UPPER"
-- ... 'True
-- :kind! IsR "do-UPPER"
-- ...
-- ... TypeError ... 
type family IsR (s :: Symbol) :: Bool where
    IsR s = AcceptEq ('Text "Not restriction encoding " ':<>: ShowType s ) (CmpSymbol "r-" (Take 2 s))


type family IsROrEmpty (s :: Symbol) :: Bool where
    IsROrEmpty "" = True
    IsROrEmpty x  = IsR x

