
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.TypedEncoding.Internal.Combinators where

import           Data.String
import           Data.Proxy
import           Text.Read
import           Data.TypedEncoding.Internal.Types
import           Data.TypedEncoding.Internal.Class.IsStringR
import           Data.TypedEncoding.Internal.Class.Recreate
import           Data.TypedEncoding.Internal.Class.Util (KnownAnnotation)
import qualified Data.List as L
import           GHC.TypeLits
import           Data.Maybe

-- $setup
-- >>> :set -XTypeApplications
-- >>> import qualified Data.Text as T
-- >>> import           Data.Word



-- * unchecked to encoding

-- | Maybe signals annotation mismatch, effect @f@ is not evaluated unless there is match
verifyUnchecked :: forall (xs :: [Symbol]) f c str . (
                     RecreateFAll f xs c str
                     , RecreateErr f
                     , Applicative f
                     , KnownAnnotation xs
                   ) 
                   =>
                   Unchecked c str
                   ->  Maybe (f (Enc xs c str))

verifyUnchecked x = 
    -- let perr = Proxy :: Proxy "e-mismatch"
    --in  
      case verifyAnn @xs x of
            Left err -> Nothing -- asRecreateErr_ perr $ Left err
            Right (MkUnchecked _ c str) -> Just $ recreateFAll . toEncoding c $ str


verifyUnchecked' :: forall (xs :: [Symbol]) c str . (
                     RecreateFAll (Either RecreateEx) xs c str
                     , KnownAnnotation xs
                   ) 
                   =>
                   Unchecked c str
                   ->  Maybe (Either RecreateEx (Enc xs c str))
verifyUnchecked' = verifyUnchecked

