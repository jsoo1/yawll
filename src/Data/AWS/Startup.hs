{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.AWS.Startup
  ( Env (..),
    RuntimeApi(..),
    Error (..),
  )
where

import qualified Data.AWS.Error as AWS
import Data.Aeson
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import GHC.Natural (Natural)

-- | The startup environment variables parsed during bootstrap initialization.
--    Create using @startupEnv.
data Env
  = Env
      { handler :: Text,
        taskRoot :: Text,
        runtimeApi :: RuntimeApi
      } deriving (Generic, ToJSON, FromJSON)

data RuntimeApi = RuntimeApi
  { runtimeApiHost :: Text,
    runtimeApiPort :: Natural
  } deriving (Generic, ToJSON, FromJSON)

data Error
  = VarNotFound Text
  | InvalidRuntimeApi Text
  | HandlerNotFound Text
  | forall e. AWS.ToError e => FunctionInitError e

instance AWS.ToError Error where
  toError = \case
    VarNotFound var -> AWS.Error
      { AWS.errorType = "VarNotFound",
        AWS.errorMessage = var,
        AWS.stackTrace = []
      }
    HandlerNotFound handler -> AWS.Error
      { AWS.errorType = "HandlerNotFound",
        AWS.errorMessage = handler,
        AWS.stackTrace = []
      }
    FunctionInitError e -> AWS.toError e
