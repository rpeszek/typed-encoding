
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | This module should be merged with 
-- "Data.TypedEncoding.Common.Util.TypeLits"
--
-- Since both provide type level helpers
module Data.TypedEncoding.Common.Class.Util where

import           Data.TypedEncoding.Common.Types.Common

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Data.Proxy
import qualified Data.List as L
import           GHC.TypeLits

-- $setup
-- >>> :set -XScopedTypeVariables -XTypeApplications -XAllowAmbiguousTypes -XDataKinds

-- * Symbol List

-- |
-- @since 0.2.0.0
class SymbolList (xs::[Symbol]) where 
    symbolVals :: [String]

instance SymbolList '[] where
    symbolVals = []

-- |
-- >>> symbolVals @ '["FIRST", "SECOND"]
-- ["FIRST","SECOND"]
instance (SymbolList xs, KnownSymbol x) => SymbolList (x ': xs) where
    symbolVals =  symbolVal (Proxy :: Proxy x) : symbolVals @xs
 
symbolVals_ :: forall xs . SymbolList xs => Proxy xs -> [String]
symbolVals_ _ = symbolVals @xs

-- * Display 

-- | Human friendly version of Show
--
-- @since 0.2.0.0
class Displ x where 
    displ :: x -> String


instance Displ [EncAnn] where 
    displ x = "[" ++ L.intercalate "," x ++ "]"
instance Displ T.Text where
    displ x = "(Text " ++ T.unpack x ++ ")"
instance Displ TL.Text where
    displ x = "(TL.Text " ++ TL.unpack x ++ ")"
instance Displ B.ByteString where
    displ x = "(ByteString " ++ B.unpack x ++ ")" 
instance Displ BL.ByteString where
    displ x = "(ByteString " ++ BL.unpack x ++ ")"   
instance Displ String where
    displ x = "(String " ++ x ++ ")" 



-- |
-- >>> displ (Proxy :: Proxy ["FIRST", "SECOND"])
-- "[FIRST,SECOND]"
instance (SymbolList xs) => Displ (Proxy xs) where
    displ _ = displ $  symbolVals @ xs
        -- "[" ++ (L.intercalate "," $ map displ $ symbolVals @ xs) ++ "]"


-- * Other

-- TODO should this be imported from somewhere?

-- |
-- Type level list append
-- 
-- @since 0.1.0.0
type family Append (xs :: [k]) (ys :: [k]) :: [k] where
    Append '[] xs = xs
    Append (y ': ys) xs = y ': Append ys xs

-- | Polymorphic data payloads used to encode/decode
--
-- This class is intended for example use only and will be moved to Example modules.
-- 
-- Use your favorite polymorphic records / ad-hock product polymorphism library.
--
-- @since 0.1.0.0
class HasA a c where
    has :: c -> a

instance HasA () c where
    has = const ()
