
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.TypedEncoding.Internal.Class.Util where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Data.Proxy
import qualified Data.List as L
import           GHC.TypeLits


-- | TODO use singletons definition instead?
type family Append (xs :: [k]) (ys :: [k]) :: [k] where
    Append '[] xs = xs
    Append (y ': ys) xs = y ': (Append ys xs)


-- | Polymorphic data payloads used to encode/decode
class HasA c a where
    has :: Proxy a -> c -> a

instance HasA a () where
    has _ = const ()


-- * Display 

-- | Human friendly version of Show
class Displ x where 
    displ :: x -> String

instance Displ String where
    displ = id    
instance Displ T.Text where
    displ x = "(Text " ++ T.unpack x ++ ")"
instance Displ TL.Text where
    displ x = "(TL.Text " ++ TL.unpack x ++ ")"
instance Displ B.ByteString where
    displ x = "(ByteString " ++ B.unpack x ++ ")" 
instance Displ BL.ByteString where
    displ x = "(ByteString " ++ BL.unpack x ++ ")" 


instance Displ (Proxy '[]) where
    displ _ = ""

-- |
-- >>> displ (Proxy :: Proxy ["FIRST", "SECOND"])
-- "FIRST,SECOND"
instance (pxs ~ Proxy xs, Displ pxs, KnownSymbol x) => Displ (Proxy (x ': xs)) where
    displ _ =  L.dropWhileEnd (',' ==) $  symbolVal (Proxy :: Proxy x) ++ "," ++ displ (Proxy :: Proxy xs)



