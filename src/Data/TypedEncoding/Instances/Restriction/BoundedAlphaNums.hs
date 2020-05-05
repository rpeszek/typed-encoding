
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
import qualified Data.List as L
import           Data.Char
import           Data.Proxy
import           Data.Either


import           Data.TypedEncoding.Internal.Class.IsStringR
import           Data.TypedEncoding.Instances.Support

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications
-- >>> import qualified Data.Text as T

-- better compilation errors?
type IsBan s =
    "r-ban:" == s ||
    (CmpSymbol "r-ban:" s == LT && CmpSymbol "r-ban;" s == GT)    


-- |
-- >>> encodeFAll . toEncoding () $ "c59f9fb7-4621-44d9-9020-ce37bf6e2bd1" :: Either EncodeEx (Enc '["r-ban:ffffffff-ffff-ffff-ffff-ffffffffffff"] () T.Text)
-- Right (MkEnc Proxy () "c59f9fb7-4621-44d9-9020-ce37bf6e2bd1")
instance (IsStringR str, KnownSymbol s, IsBan s ~ 'True) =>  EncodeF (Either EncodeEx) (Enc xs c str) (Enc (s ': xs) c str) where
    encodeF = implEncodeF @s (verifyBoundedAlphaNum (Proxy :: Proxy s))

-- |
-- >>> recreateFAll . toEncoding () $ "211-22-9934" :: Either RecreateEx (Enc '["r-ban:999-99-9999"] () T.Text)
-- Right (MkEnc Proxy () "211-22-9934")
instance (IsStringR str, KnownSymbol s, IsBan s ~ 'True, RecreateErr f, Applicative f) => RecreateF f (Enc xs c str) (Enc (s ': xs) c str) where
    checkPrevF = implCheckPrevF (asRecreateErr @s . verifyBoundedAlphaNum (Proxy :: Proxy s))
instance (IsStringR str, KnownSymbol s, IsBan s ~ 'True, Applicative f) => DecodeF f (Enc (s ': xs) c str) (Enc xs c str) where
    decodeF = implTranP id 



-- |
-- >>> verifyBoundedAlphaNum (Proxy :: Proxy "r-ban:ff-ff") (T.pack "12-3e")
-- Right "12-3e"
-- verifyBoundedAlphaNum (Proxy :: Proxy "r-ban:ff-ff") (T.pack "1g-3e")
-- Left "'g' not boulded by 'f'"
-- >>> verifyBoundedAlphaNum (Proxy :: Proxy "r-ban:ff-ff") (T.pack "13g3e")
-- Left "'g' not matching '-'"
-- >>> verifyBoundedAlphaNum (Proxy :: Proxy "r-ban:ff-ff") (T.pack "13-234")
-- Left "Input list has wrong size expecting 5 but length \"13-234\" == 6"
verifyBoundedAlphaNum :: forall s a str . (KnownSymbol s, IsStringR str) => Proxy s -> str -> Either String str
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
