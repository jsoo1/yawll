{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.AWS.Error where

import qualified Data.AWS.Runtime.Response as Response
import Data.Aeson
import Data.Aeson.Text
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)

class ToError a where
  toError :: a -> Error

data Error
  = Error
      { errorType :: Text,
        errorMessage :: Text,
        stackTrace :: [Text]
      }
  deriving (Generic, ToJSON)

data TextErr = forall a. ToError a => TextErr { unTextErr :: a }

instance Response.ToText TextErr where
  toText (TextErr e) = TL.toStrict $ encodeToLazyText $ toJSON $ toError e
