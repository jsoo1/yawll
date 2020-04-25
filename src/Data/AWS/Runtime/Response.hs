{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.AWS.Runtime.Response where

import Data.Text (Text)
import qualified Data.Text

data Response =
  forall a. ToText a => Response a

class ToText a where
  toText :: a -> Text
