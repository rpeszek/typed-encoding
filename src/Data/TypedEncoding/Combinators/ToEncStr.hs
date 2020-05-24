
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

-- | 
-- v0.2 like (backward compatible) combinators for covering to and from encoded string.
--
-- @since 0.3.0.0

module Data.TypedEncoding.Combinators.ToEncStr where

import           Data.TypedEncoding.Common.Class
import           Data.TypedEncoding.Common.Types (Enc(..) )

import           Data.Functor.Identity

toEncStringF :: forall nm f a str  . (ToEncString f nm nm a str) => a -> f (Enc '[nm] () str)
toEncStringF = toEncStringF' @nm @nm

toEncStringF' :: forall alg nm f a str  . (ToEncString f nm alg a str) => a -> f (Enc '[nm] () str)
toEncStringF' = toEncF @f @nm @alg

toEncString :: forall nm a str  . (ToEncString Identity nm nm a str) => a -> Enc '[nm] () str
toEncString = toEncString' @nm @nm

toEncString' :: forall alg nm a str  . (ToEncString Identity nm alg a str) => a -> Enc '[nm] () str
toEncString' = runIdentity . toEncF @Identity @nm @alg



-- backward compatible v0.2 like combinators
fromEncStringF :: forall nm f a str  . (FromEncString f nm nm a str) => Enc '[nm] () str -> f a
fromEncStringF = fromEncStringF' @nm @nm

fromEncStringF' :: forall alg nm f a str  . (FromEncString f nm alg a str) => Enc '[nm] () str -> f a
fromEncStringF' = fromEncF @f @nm @alg

fromEncString :: forall nm a str  . (FromEncString Identity nm nm a str) => Enc '[nm] () str -> a
fromEncString = fromEncString' @nm @nm

fromEncString' :: forall alg nm a str  . (FromEncString Identity nm alg a str) => Enc '[nm] () str -> a
fromEncString' = runIdentity . fromEncF @Identity @nm @alg
