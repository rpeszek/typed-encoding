
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

module Data.TypedEncoding.Internal.Class.Util where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Data.Proxy
import qualified Data.List as L
import           GHC.TypeLits

-- $setup
-- >>> :set -XScopedTypeVariables -XTypeApplications -XAllowAmbiguousTypes

-- | TODO use singletons definition instead?
type family Append (xs :: [k]) (ys :: [k]) :: [k] where
    Append '[] xs = xs
    Append (y ': ys) xs = y ': (Append ys xs)


-- | Polymorphic data payloads used to encode/decode
class HasA a c where
    has :: c -> a

instance HasA () c where
    has = const ()

-- * Some Annotation

-- TODO move out to central place
type SomeAnn = String

class KnownAnnotation (xs::[Symbol]) where 
    knownAnn :: SomeAnn

instance KnownAnnotation '[] where
    knownAnn = ""

-- |
-- >>> knownAnn @ '["FIRST", "SECOND"]
-- "FIRST,SECOND"
instance (KnownAnnotation xs, KnownSymbol x) => KnownAnnotation (x ': xs) where
    knownAnn =  L.dropWhileEnd (',' ==) $  symbolVal (Proxy :: Proxy x) ++ "," ++ knownAnn @xs


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



-- |
-- >>> displ (Proxy :: Proxy ["FIRST", "SECOND"])
-- "FIRST,SECOND"
instance (KnownAnnotation xs) => Displ (Proxy xs) where
    displ _ = knownAnn @ xs



