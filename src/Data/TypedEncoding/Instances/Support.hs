
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

-- | Exports for instance creation.
-- 
-- Contains typical things needed when implementing
-- encoding, decoding, recreate, or type to string conversions.
module Data.TypedEncoding.Instances.Support (
    module Data.TypedEncoding.Instances.Support
    -- * Types
    , module Data.TypedEncoding.Common.Types
    -- * Classes
    , module Data.TypedEncoding.Common.Class
    -- * Combinators
    , module Data.TypedEncoding.Instances.Support.Common
    , module Data.TypedEncoding.Instances.Support.Helpers
    , module Data.TypedEncoding.Instances.Support.Encode
    , module Data.TypedEncoding.Instances.Support.Decode
    , module Data.TypedEncoding.Instances.Support.Validate
    -- * Type level conveniences
    , module Data.TypedEncoding.Common.Util.TypeLits
    , module Data.TypedEncoding.Combinators.Unsafe
    --, module Data.TypedEncoding.Instances.Support.Deprecated
   ) where

import           Data.TypedEncoding.Instances.Support.Common 
import           Data.TypedEncoding.Instances.Support.Helpers 
import           Data.TypedEncoding.Instances.Support.Encode 
import           Data.TypedEncoding.Instances.Support.Decode 
import           Data.TypedEncoding.Instances.Support.Validate 
import           Data.TypedEncoding.Common.Types
import           Data.TypedEncoding.Common.Class 
import           Data.TypedEncoding.Common.Util.TypeLits
import           Data.TypedEncoding.Combinators.Unsafe 
-- import           Data.TypedEncoding.Instances.Support.Deprecated
