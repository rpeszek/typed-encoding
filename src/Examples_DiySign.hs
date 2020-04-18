{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | TODO
module Examples_DiySign where

import           Data.Encoding.Internal.Types
import           Data.Encoding.Internal.Class
import           Data.Proxy
import           Data.Functor.Identity
import           GHC.TypeLits

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE 

import           Data.Char
import           Data.Encoding.Internal.Utils (explainBool)
import           Data.Encoding.Internal.Unsafe (withUnsafe)
import           Control.Arrow


