
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.TypedEncoding.Internal.Class.Superset where

import           Data.TypedEncoding.Internal.Util.TypeLits
--import           Data.TypedEncoding.Internal.Class.Util (displ)

import           Data.TypedEncoding.Internal.Types (Enc(..) 
                                                   , withUnsafeCoerce
                                                   --, unsafeSetPayload
                                                   )
import           GHC.TypeLits
import           Data.Symbol.Ascii
-- import           Data.Proxy

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications
-- >>> import           Data.TypedEncoding.Internal.Class.Util (displ)
-- >>> import           Data.TypedEncoding.Internal.Types (unsafeSetPayload)
-- >>> import           Data.Text as T


-- | Subsets are useful for restriction encodings
-- like r-UFT8 but should not be used for other encodings.
--
-- This would be dangerous, it would, for example, permit converting encoded binary 
-- @"Enc '["enc-"] c ByteString@ to @"Enc '["enc-"] c Text@, decoding which
-- could result in rutime errors.
--
-- The requirement is that that the decoding in the superset
-- can replace the decoding from injected subset.
--
-- @
-- instance Superset "r-ASCII" "enc-B64" where -- DANGEROUS
-- @
--
-- 'inject' is identity on payloads
--
-- @Superset bigger smaller@ reads as @bigger@ is a superset of @smaller@
class Superset (y :: Symbol) (x :: Symbol) where
    inject :: Enc (x ': xs) c str ->  Enc (y ': xs) c str
    inject = withUnsafeCoerce id

instance Superset x x where

-- | more permissive than class
type family IsSuperset (y :: Symbol) (x :: Symbol) :: Bool where
    IsSuperset "r-ASCII" "r-ASCII" = 'True
    IsSuperset "r-UTF8"  "r-ASCII" = 'True
    IsSuperset "r-UTF8"  "r-UTF8" = 'True
    IsSuperset y x = IsSupersetOpen y (TakeUntil x ":") (ToList x)

type family IsSupersetOpen (y :: Symbol) (x :: Symbol) (xs :: [Symbol]) :: Bool

injectInto :: forall y x xs c str . (IsSuperset y x ~ 'True) => Enc (x ': xs) c str ->  Enc (y ': xs) c str
injectInto = withUnsafeCoerce id

-- | remove redundant superset right after the top (at second last encoding position)
--
-- >>> displ $ demoteFlattenTop (unsafeSetPayload () "" :: Enc '["r-ASCII", "r-UTF8", "r-boo"] () T.Text)
-- "MkEnc '[r-ASCII,r-boo] () (Text )"
demoteFlattenTop :: forall y x xs c str . (IsSuperset y x ~ 'True) => Enc (x ': y ': xs) c str ->  Enc (x ': xs) c str
demoteFlattenTop = withUnsafeCoerce id

-- | add redundant superset right after
-- displ $ promoteUnFlattenTop @"r-UTF8" (unsafeSetPayload () "" :: Enc '["r-ASCII", "r-boo"] () T.Text)
-- "MkEnc '[r-ASCII,r-UTF8,r-boo] () (Text )"
promoteUnFlattenTop :: forall y x xs c str . (IsSuperset y x ~ 'True) => Enc (x ': xs) c str -> Enc (x ': y ': xs) c str
promoteUnFlattenTop = withUnsafeCoerce id

-- | remove redunant superset from the top (at last applied encoding position)
-- >>> displ $ demoteRemoveTop (unsafeSetPayload () "" :: Enc '["r-UTF8", "r-ASCII", "r-boo"] () T.Text)
-- "MkEnc '[r-ASCII,r-boo] () (Text )"
demoteRemoveTop :: forall y x xs c str . (IsSuperset y x ~ 'True) => Enc (y ': x ' : xs) c str ->  Enc (x ': xs) c str
demoteRemoveTop = withUnsafeCoerce id

-- | add redundant superset at the top
-- >>> displ $ promoteAddTop @"r-UTF8" (unsafeSetPayload () "" :: Enc '["r-ASCII", "r-boo"] () T.Text)
-- "MkEnc '[r-UTF8,r-ASCII,r-boo] () (Text )"
promoteAddTop :: forall y x xs c str . (IsSuperset y x ~ 'True) => Enc (x ': xs) c str -> Enc (y ': x ' : xs) c str 
promoteAddTop = withUnsafeCoerce id

-- | remove redundant superset at bottom (first encoding) position
-- >>> displ $ demoteRemoveBot (unsafeSetPayload () "" :: Enc '["r-boo", "r-ASCII", "r-UTF8"] () T.Text)
-- "MkEnc '[r-boo,r-ASCII] () (Text )"
demoteRemoveBot :: (UnSnoc xs ~ '(,) ys y, UnSnoc ys ~ '(,) zs x, IsSuperset y x ~ 'True) => Enc xs c str -> Enc ys c str
demoteRemoveBot = withUnsafeCoerce id

-- | add redundant superset at bottom (first encoding) position
-- >>> displ $ promoteAddBot @"r-UTF8" (unsafeSetPayload () "" :: Enc '["r-boo", "r-ASCII"] () T.Text)
-- "MkEnc '[r-boo,r-ASCII,r-UTF8] () (Text )"
promoteAddBot :: forall y x xs c str ys . (UnSnoc xs ~ '(,) ys x,  IsSuperset y x ~ 'True) => Enc xs c str -> Enc (Snoc xs y) c str
promoteAddBot = withUnsafeCoerce id

-- | remove redundant superset at second bottom (second encoding) position
-- >>> displ $ demoteFlattenBot (unsafeSetPayload () "" :: Enc '["r-boo", "r-UTF8", "r-ASCII"] () T.Text)
-- "MkEnc '[r-boo,r-ASCII] () (Text )"
demoteFlattenBot :: (UnSnoc xs ~ '(,) ys x, UnSnoc ys ~ '(,) zs y, IsSuperset y x ~ 'True) => Enc xs c str -> Enc (Snoc zs x) c str
demoteFlattenBot = withUnsafeCoerce id

-- | add redundant superset at second bottom (second encoding) position
-- >>> displ $ promoteUnFlattenBot @"r-UTF8" (unsafeSetPayload () "" :: Enc '["r-boo", "r-ASCII"] () T.Text)
-- "MkEnc '[r-boo,r-UTF8,r-ASCII] () (Text )"
promoteUnFlattenBot :: forall y x xs c str ys . (UnSnoc xs ~ '(,) ys x,  IsSuperset y x ~ 'True) => Enc xs c str -> Enc (Snoc (Snoc ys y) x) c str
promoteUnFlattenBot = withUnsafeCoerce id

-- prop_Superset :: forall y x xs c str . (Superset y x, Eq str) => Enc (x ': xs) c str -> Bool
-- prop_Superset x = getPayload x == (getPayload . inject @y @x $ x)

