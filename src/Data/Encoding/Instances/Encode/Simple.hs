
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Encoding.Instances.Encode.Simple where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL


import           Data.Encoding.Internal.Types
import           Data.Encoding.Internal.Class
import           Data.Proxy
import           GHC.TypeLits
import qualified Data.List as L


instance Encode (Enc xs T.Text) (Enc ("UPPER" ': xs) T.Text) where
    encode = unPriv . pure . T.toUpper . getPayload
instance Encode (Enc xs TL.Text) (Enc ("UPPER" ': xs) TL.Text) where
    encode = unPriv . pure . TL.toUpper . getPayload

instance Encode (Enc xs T.Text) (Enc ("lower" ': xs) T.Text) where
    encode = unPriv . pure . T.toLower . getPayload    
instance Encode (Enc xs TL.Text) (Enc ("lower" ': xs) TL.Text) where
    encode = unPriv . pure . TL.toLower . getPayload    

instance Encode (Enc xs T.Text) (Enc ("Title" ': xs) T.Text) where
    encode = unPriv . pure . T.toTitle . getPayload      
instance Encode (Enc xs TL.Text) (Enc ("Title" ': xs) TL.Text) where
    encode = unPriv . pure . TL.toTitle . getPayload      

instance Encode (Enc xs T.Text) (Enc ("reverse" ': xs) T.Text) where
    encode = unPriv . pure . T.reverse . getPayload
instance Encode (Enc xs TL.Text) (Enc ("reverse" ': xs) TL.Text) where
    encode = unPriv . pure . TL.reverse . getPayload    

-- instance (KnownSymbol s, KnownSymbol t, t ~ AppendSymbol "limit " s) => Encode (Enc xs T.Text) (Enc (t ': xs) T.Text) where    
--     encode txt = 
--         let p = Proxy :: Proxy t
--             str = symbolVal p
--             nums = L.drop (L.length "limit ") str
--             num :: Int = read nums
--         in     
--             unPriv . pure . T.take num . getPayload $ txt