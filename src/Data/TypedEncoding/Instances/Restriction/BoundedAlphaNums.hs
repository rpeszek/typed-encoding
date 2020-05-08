
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

-- | 
-- DEPRECATED in favor of  
-- "Data.TypedEncoding.Combinators.Restriction.BoundedAlphaNums" 
--
-- This is likely to cause duplicate instance issues if more than one construction
-- of this sort is introduced.
--
-- Restrictions @"r-ban:"@ cover commonly used fixed (short) size strings with restricted
-- characters such as GUID, credit card numbers, etc.  
-- 
-- Alphanumeric chars are ordered: @0-9@ followed by 
-- @a-z@ followed by @A-Z@. Annotation specifies upper character bound. 
-- Any non alpha numeric characters are considered fixed delimiters
-- and need to be present exactly as specified.
-- For example @"r-ban:999-99-9999"@ could be used to describe SSN numbers,
-- @"r-ban:ffff" would describe strings consisting of 4 hex digits.
--
-- This is a simple implementation that converts to @String@, should be used
-- only with short length data.

module Data.TypedEncoding.Instances.Restriction.BoundedAlphaNums where 


import           GHC.TypeLits
import           Data.Type.Bool -- ((||), (&&))
import           Data.Type.Equality -- ((==))
import           Data.Proxy

import           Data.TypedEncoding.Internal.Class.IsStringR
import           Data.TypedEncoding.Instances.Support
import           Data.TypedEncoding.Combinators.Restriction.BoundedAlphaNums (verifyBoundedAlphaNum)

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications
-- >>> import qualified Data.Text as T

type IsBan s =
    "r-ban:" == s ||
    (CmpSymbol "r-ban:" s == LT && CmpSymbol "r-ban;" s == GT)    


-- |
-- >>> encodeFAll . toEncoding () $ "c59f9fb7-4621-44d9-9020-ce37bf6e2bd1" :: Either EncodeEx (Enc '["r-ban:ffffffff-ffff-ffff-ffff-ffffffffffff"] () T.Text)
-- Right (MkEnc Proxy () "c59f9fb7-4621-44d9-9020-ce37bf6e2bd1")
instance {-# OVERLAPS #-} (IsStringR str, KnownSymbol s, IsBan s ~ 'True) =>  EncodeF (Either EncodeEx) (Enc xs c str) (Enc (s ': xs) c str) where
    encodeF = implEncodeF @s (verifyBoundedAlphaNum (Proxy :: Proxy s))

-- |
-- >>> recreateFAll . toEncoding () $ "211-22-9934" :: Either RecreateEx (Enc '["r-ban:999-99-9999"] () T.Text)
-- Right (MkEnc Proxy () "211-22-9934")
instance (IsStringR str, KnownSymbol s, IsBan s ~ 'True, RecreateErr f, Applicative f) => RecreateF f (Enc xs c str) (Enc (s ': xs) c str) where
    checkPrevF = implCheckPrevF (asRecreateErr @s . verifyBoundedAlphaNum (Proxy :: Proxy s))
instance (IsStringR str, KnownSymbol s, IsBan s ~ 'True, Applicative f) => DecodeF f (Enc (s ': xs) c str) (Enc xs c str) where
    decodeF = implTranP id 



