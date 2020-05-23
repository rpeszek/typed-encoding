

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

-- | Promote and demote combinators.
module Data.TypedEncoding.Combinators.Promotion where

import           Data.TypedEncoding.Common.Class
import           Data.TypedEncoding.Common.Util.TypeLits

import           Data.TypedEncoding.Common.Types (Enc(..) )
import           Data.TypedEncoding.Combinators.Unsafe (withUnsafeCoerce)


-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications
-- >>> import           Data.TypedEncoding.Common.Class.Util (displ)
-- >>> import           Data.TypedEncoding.Combinators.Unsafe (unsafeSetPayload)
-- >>> import           Data.TypedEncoding.Instances.Restriction.UTF8 ()
-- >>> import           Data.TypedEncoding.Instances.Restriction.ASCII ()
-- >>> import           Data.Text as T

-- | Remove redundant superset right after the top (at second last encoding position)
--
-- >>> displ $ demoteFlattenTop (unsafeSetPayload () "" :: Enc '["r-ASCII", "r-UTF8", "r-boo"] () T.Text)
-- "Enc '[r-ASCII,r-boo] () (Text )"
--
-- @since 0.2.2.0 (moved)
demoteFlattenTop :: forall y x xs c str . (IsSuperset y x ~ 'True) => Enc (x ': y ': xs) c str ->  Enc (x ': xs) c str
demoteFlattenTop = withUnsafeCoerce id

-- | add redundant superset right after
--
-- >>> displ $ promoteUnFlattenTop @"r-UTF8" (unsafeSetPayload () "" :: Enc '["r-ASCII", "r-boo"] () T.Text)
-- "Enc '[r-ASCII,r-UTF8,r-boo] () (Text )"
--
-- @since 0.2.2.0 (moved)
promoteUnFlattenTop :: forall y x xs c str . (IsSuperset y x ~ 'True) => Enc (x ': xs) c str -> Enc (x ': y ': xs) c str
promoteUnFlattenTop = withUnsafeCoerce id

-- | remove redunant superset from the top (at last applied encoding position)
--
-- >>> displ $ demoteRemoveTop (unsafeSetPayload () "" :: Enc '["r-UTF8", "r-ASCII", "r-boo"] () T.Text)
-- "Enc '[r-ASCII,r-boo] () (Text )"
--
-- @since 0.2.2.0 (moved)
demoteRemoveTop :: forall y x xs c str . (IsSuperset y x ~ 'True) => Enc (y ': x ' : xs) c str ->  Enc (x ': xs) c str
demoteRemoveTop = withUnsafeCoerce id

-- | add redundant superset at the top
--
-- >>> displ $ promoteAddTop @"r-UTF8" (unsafeSetPayload () "" :: Enc '["r-ASCII", "r-boo"] () T.Text)
-- "Enc '[r-UTF8,r-ASCII,r-boo] () (Text )"
--
-- @since 0.2.2.0 (moved)
promoteAddTop :: forall y x xs c str . (IsSuperset y x ~ 'True) => Enc (x ': xs) c str -> Enc (y ': x ' : xs) c str 
promoteAddTop = withUnsafeCoerce id

-- | remove redundant superset at bottom (first encoding) position
--
-- >>> displ $ demoteRemoveBot (unsafeSetPayload () "" :: Enc '["r-boo", "r-ASCII", "r-UTF8"] () T.Text)
-- "Enc '[r-boo,r-ASCII] () (Text )"
--
-- @since 0.2.2.0 (moved)
demoteRemoveBot :: (UnSnoc xs ~ '(,) ys y, UnSnoc ys ~ '(,) zs x, IsSuperset y x ~ 'True) => Enc xs c str -> Enc ys c str
demoteRemoveBot = withUnsafeCoerce id

-- | add redundant superset at bottom (first encoding) position
--
-- >>> displ $ promoteAddBot @"r-UTF8" (unsafeSetPayload () "" :: Enc '["r-boo", "r-ASCII"] () T.Text)
-- "Enc '[r-boo,r-ASCII,r-UTF8] () (Text )"
promoteAddBot :: forall y x xs c str ys . (UnSnoc xs ~ '(,) ys x,  IsSuperset y x ~ 'True) => Enc xs c str -> Enc (Snoc xs y) c str
promoteAddBot = withUnsafeCoerce id

-- | remove redundant superset at second bottom (second encoding) position
--
-- >>> displ $ demoteFlattenBot (unsafeSetPayload () "" :: Enc '["r-boo", "r-UTF8", "r-ASCII"] () T.Text)
-- "Enc '[r-boo,r-ASCII] () (Text )"
--
-- @since 0.2.2.0 (moved)
demoteFlattenBot :: (UnSnoc xs ~ '(,) ys x, UnSnoc ys ~ '(,) zs y, IsSuperset y x ~ 'True) => Enc xs c str -> Enc (Snoc zs x) c str
demoteFlattenBot = withUnsafeCoerce id

-- | add redundant superset at second bottom (second encoding) position
--
-- >>> displ $ promoteUnFlattenBot @"r-UTF8" (unsafeSetPayload () "" :: Enc '["r-boo", "r-ASCII"] () T.Text)
-- "Enc '[r-boo,r-UTF8,r-ASCII] () (Text )"
--
-- @since 0.2.2.0 (moved)
promoteUnFlattenBot :: forall y x xs c str ys . (UnSnoc xs ~ '(,) ys x,  IsSuperset y x ~ 'True) => Enc xs c str -> Enc (Snoc (Snoc ys y) x) c str
promoteUnFlattenBot = withUnsafeCoerce id
