{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TypeApplications #-}

module Data.TypedEncoding.Instances.Restriction.Bool where 


import           GHC.TypeLits
import           Data.Type.Bool -- ((||), (&&))
import           Data.Type.Equality -- ((==))
import qualified Data.List as L
import           Data.Char
import           Data.Proxy
import           Data.Either
import qualified Data.Text as T

import           Data.TypedEncoding.Internal.Class.IsStringR
import           Data.TypedEncoding.Instances.Support
import           Data.TypedEncoding.Internal.Util.TypeLits

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