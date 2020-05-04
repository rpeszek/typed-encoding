
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

-- | 
-- Restrictions @"r-ban:"@ allow fixed size strings defined by restricted
-- characters.  Alphanumeric chars are ordered: @0-9@ followed by 
-- @a-z@ followed by @A-Z@. Annotation uses these chars as upper bound. 
-- Any other character is considered fixed delimiter
-- that needs to be present exactly.
-- For example @"r-ban:999-99-9999"@ could be used to describe SSN numbers,
-- @"r-ban:ffff" would describe strings consisting of 4 hex digits.

module Data.TypedEncoding.Instances.Restriction.BoundedAlphaNums where 


import           GHC.TypeLits
import           Data.Type.Bool -- ((||), (&&))
import           Data.Type.Equality -- ((==))


import           Data.TypedEncoding.Internal.Class.IsStringR
import           Data.TypedEncoding.Internal.Instances.Combinators
import           Data.TypedEncoding.Instances.Support

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications
-- >>> import qualified Data.Text as T

-- better compilation errors?
type IsBan s =
    "r-ban:" == s ||
    (CmpSymbol "r-ban:" s == LT && CmpSymbol "r-ban;" s == GT)    


-- encodeFAll . toEncoding () $ "aga" :: Either EncodeEx (Enc '["r-ban:x"] () T.Text)
-- encodeFAll . toEncoding () $ "aga" :: Either EncodeEx (Enc '["r-banx"] () T.Text)
instance (IsStringR str, KnownSymbol s, IsBan s ~ 'True) =>  EncodeF (Either EncodeEx) (Enc xs c str) (Enc (s ': xs) c str) where
    encodeF = undefined -- implEncodeF @"r-Word8-decimal" (verifyWithRead @Word8 "Word8-decimal")
