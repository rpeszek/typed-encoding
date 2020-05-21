{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE RankNTypes #-}

-- |
-- Internal definition of types

module Data.TypedEncoding.Common.Types (
        module Data.TypedEncoding.Common.Types
        -- * Main encoding type and basic combinators.
        , module Data.TypedEncoding.Internal.Enc
        , module Data.TypedEncoding.Common.Types.Decoding
        , module Data.TypedEncoding.Common.Types.Validation
        -- * Untyped version and existentially quantified versions of Enc
        , module Data.TypedEncoding.Common.Types.CheckedEnc
        -- * Not verified encoded data
        , module Data.TypedEncoding.Common.Types.UncheckedEnc
        -- * Commmon types
        , module Data.TypedEncoding.Common.Types.Common
        , module Data.TypedEncoding.Common.Types.Exceptions
    ) where

import           Data.TypedEncoding.Internal.Enc
import           Data.TypedEncoding.Common.Types.Decoding
import           Data.TypedEncoding.Common.Types.Validation
import           Data.TypedEncoding.Common.Types.CheckedEnc
import           Data.TypedEncoding.Common.Types.UncheckedEnc
import           Data.TypedEncoding.Common.Types.Common
import           Data.TypedEncoding.Common.Types.Exceptions

