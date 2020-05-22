
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

-- | 
-- Restrictions @"r-ban:"@ cover commonly used fixed (short) size strings with restricted
-- characters such as GUID, credit card numbers, etc.  
-- 
-- Alphanumeric chars are ordered: @0-9@ followed by @A-Z@,
-- followed by @a-z@. Annotation specifies upper character bound. 
-- Any non alpha numeric characters are considered fixed delimiters
-- and need to be present exactly as specified.
-- For example @"r-ban:999-99-9999"@ could be used to describe SSN numbers,
-- @"r-ban:FFFF" would describe strings consisting of 4 hex digits.
--
-- This is a simple implementation that converts to @String@, should be used
-- only with short length data.
--
--
-- @since 0.2.1.0
module Data.TypedEncoding.Instances.Restriction.BoundedAlphaNums where 


import           GHC.TypeLits
import qualified Data.List as L
import           Data.Char
import           Data.Proxy
import           Data.Either

import           Data.TypedEncoding.Common.Util.TypeLits
import           Data.TypedEncoding.Common.Class.IsStringR
import           Data.TypedEncoding.Instances.Support

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications
-- >>> import qualified Data.Text as T
-- >>> import           Data.TypedEncoding

-- better compilation errors?
type family IsBan (s :: Symbol) :: Bool where
    IsBan s = AcceptEq ('Text "Not ban restriction encoding " ':<>: ShowType s ) (CmpSymbol (TakeUntil s ":") "r-ban")

type Ban s = (KnownSymbol s, IsBan s ~ 'True)

type instance IsSupersetOpen "r-ASCII" "r-ban" xs = 'True


instance (Ban s, Algorithm s "r-ban", IsStringR str) => Encode (Either EncodeEx) s "r-ban" c str where
    encoding = encFBan


-- |
-- >>> runEncoding' encFBan . toEncoding () $ "C59F9FB7-4621-44D9-9020-CE37BF6E2BD1" :: Either EncodeEx (Enc '["r-ban:FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF"] () T.Text)
-- Right (UnsafeMkEnc Proxy () "C59F9FB7-4621-44D9-9020-CE37BF6E2BD1")
-- 
-- >>> recreateFAll' @'["r-ban"] . toEncoding () $ "211-22-9934" :: Either RecreateEx (Enc '["r-ban:999-99-9999"] () T.Text)
-- Right (UnsafeMkEnc Proxy () "211-22-9934")
encFBan :: forall s c str .
              (
                IsStringR str
              , Ban s
              , Algorithm s "r-ban"
              ) => 
              Encoding (Either EncodeEx) s "r-ban" c str
encFBan = _implEncodingEx @s (verifyBoundedAlphaNum (Proxy :: Proxy s))              



-- * Decoding

instance (KnownSymbol s, Restriction s, Algorithm s "r-ban", Applicative f) => Decode f s "r-ban" c str where
    decoding = decAnyR_


-- * Validation

instance (KnownSymbol s , Ban s, Algorithm s "r-ban", IsStringR str, RecreateErr f, Applicative f) => Validate f s "r-ban" c str where
    validation = validFromEnc' @"r-ban" encFBan


-- * Implementation

-- |
-- >>> verifyBoundedAlphaNum (Proxy :: Proxy "r-ban:FF-FF") (T.pack "12-3E")
-- Right "12-3E"
-- >>> verifyBoundedAlphaNum (Proxy :: Proxy "r-ban:FF-FF") (T.pack "1G-3E")
-- Left "'G' not boulded by 'F'"
-- >>> verifyBoundedAlphaNum (Proxy :: Proxy "r-ban:FF-FF") (T.pack "13G3E")
-- Left "'G' not matching '-'"
-- >>> verifyBoundedAlphaNum (Proxy :: Proxy "r-ban:FF-FF") (T.pack "13-234")
-- Left "Input list has wrong size expecting 5 but length \"13-234\" == 6"
verifyBoundedAlphaNum :: forall s str . (KnownSymbol s, IsStringR str) => Proxy s -> str -> Either String str
verifyBoundedAlphaNum p str = 
    if pattl == inpl 
    then case lefts match of
        (e: _) -> Left e
        _ -> Right str
    else Left $ "Input list has wrong size expecting " ++ show pattl ++ " but length " ++ show input ++ " == " ++ show inpl   
    where 
        patt = L.drop (L.length ("r-ban:" :: String)) . symbolVal $ p
        input = toString str
        pattl = L.length patt
        inpl = L.length input 
        
        match = L.zipWith fn input patt


        fn ci cp = case (isAlphaNum ci, isAlphaNum cp, ci <= cp, ci == cp) of
            (True, True, True, _) -> Right ()
            (_, _, _, True) -> Right ()
            (_, True, _, False) -> Left $ show ci ++ " not boulded by " ++ show cp
            (_, False, _, False) -> Left $ show ci ++ " not matching " ++ show cp
