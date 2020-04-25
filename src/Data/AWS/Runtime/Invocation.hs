{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.AWS.Runtime.Invocation where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Status
  = Status
      { status :: Text
      }
  deriving (Generic, ToJSON, FromJSON)

data Error
  = Error
      { errorMessage :: Text,
        errorType :: Text
      }
  deriving (Generic, ToJSON, FromJSON)

type Event = Object
