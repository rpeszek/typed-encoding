{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Text encoding combinators specific to 'T.Text'
module Data.TypedEncoding.Conv.Text where

import qualified Data.Text as T
import           Data.TypedEncoding.Instances.Support


-- $setup
-- >>> :set -XDataKinds -XTypeFamilies -XTypeApplications


-- | This assumes that each of the encodings in @xs@ work work equivalently in @String@ and @Text@. 
pack :: Enc xs c String -> Enc xs c T.Text
pack = unsafeChangePayload T.pack

-- | This assumes that each of the encodings in @xs@ work work equivalently in @String@ and @Text@. 
unpack :: Enc xs c T.Text -> Enc xs c String
unpack = unsafeChangePayload T.unpack 

-- | Text is automatically @"r-UTF8"@ encoded
-- 
-- >>> displ $ utf8Promote $ toEncoding () ("text" :: T.Text)
-- "MkEnc '[r-UTF8] () (Text text)"
utf8Promote :: Enc xs c T.Text -> Enc (Snoc xs "r-UTF8") c T.Text
utf8Promote = withUnsafeCoerce id

-- | For 'Text' @"r-UTF8"@ is redundant
--
-- >>> displ . utf8Demote $ (unsafeSetPayload () "Hello" :: Enc '["r-UTF8"] () T.Text)
-- "MkEnc '[] () (Text Hello)"
utf8Demote :: (UnSnoc xs ~ '(,) ys "r-UTF8") => Enc xs c T.Text -> Enc ys c T.Text
utf8Demote = withUnsafeCoerce id