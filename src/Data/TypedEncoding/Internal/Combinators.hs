
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.TypedEncoding.Internal.Combinators where

import           Data.TypedEncoding.Internal.Types
import           Data.TypedEncoding.Internal.Class.Recreate
import           Data.TypedEncoding.Internal.Class.Util (SymbolList)
import           GHC.TypeLits

-- $setup
-- >>> :set -XTypeApplications
-- >>> import qualified Data.Text as T
-- >>> import           Data.Word

-- * Converting 'UncheckedEnc' to 'Enc'

-- | Maybe signals annotation mismatch, effect @f@ is not evaluated unless there is match
verifyUncheckedEnc :: forall (xs :: [Symbol]) f c str . (
                     RecreateFAll f xs c str
                     , RecreateErr f
                     , Applicative f
                     , SymbolList xs
                   ) 
                   =>
                   UncheckedEnc c str
                   ->  Maybe (f (Enc xs c str))

verifyUncheckedEnc x = 
    -- let perr = Proxy :: Proxy "e-mismatch"
    --in  
      case verifyAnn @xs x of
            Left err -> Nothing -- asRecreateErr_ perr $ Left err
            Right (MkUncheckedEnc _ c str) -> Just $ recreateFAll . toEncoding c $ str


verifyUncheckedEnc' :: forall (xs :: [Symbol]) c str . (
                     RecreateFAll (Either RecreateEx) xs c str
                     , SymbolList xs
                   ) 
                   =>
                   UncheckedEnc c str
                   ->  Maybe (Either RecreateEx (Enc xs c str))
verifyUncheckedEnc' = verifyUncheckedEnc
