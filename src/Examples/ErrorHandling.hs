{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Discuss error handling options 
--
-- Look at 'Examples.DiySignEncoding' first.
--
-- My current thinking: 
-- Prefer providing @EncodeF (Either err)@ instace
-- Keep DecodeF instance polymorphic in the effect.
-- 
-- However handling errors during decoding stage
-- (defining @DecodeF (Either err)@ instance) is important
-- for any situation where decoding needs to do tamper proofing,
-- data corruption detection, etc.
--
-- I believe, when in doubt, the decision should lie with the
-- caller, and not with encoding implemenation.

module Examples.ErrorHandling where


import           Data.Proxy
import           Data.Functor.Identity
import           GHC.TypeLits

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE 

import           Data.Char
import           Control.Arrow

-- trustDecode
