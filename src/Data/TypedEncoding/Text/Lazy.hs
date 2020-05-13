
-- | Lazy version of "Data.TypedEncoding.Text"
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.TypedEncoding.Text.Lazy where

import qualified Data.Text.Lazy as TL
import           Data.TypedEncoding.Instances.Support

pack :: Enc xs c String -> Enc xs c TL.Text
pack = unsafeChangePayload TL.pack

unpack :: Enc xs c TL.Text -> Enc xs c String
unpack = unsafeChangePayload TL.unpack    

-- | Text is automatically @"r-UTF8"@ encoded
utf8Promote :: Enc xs c TL.Text -> Enc (Snoc xs "r-UTF8") c TL.Text
utf8Promote = withUnsafeCoerce id

-- | For 'Text' @"r-UTF8"@ is redundant
utf8Demote :: (UnSnoc xs ~ '(,) ys "r-UTF8") => Enc xs c TL.Text -> Enc ys c TL.Text
utf8Demote = withUnsafeCoerce id