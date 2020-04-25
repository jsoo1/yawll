{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.AWS.Runtime.Context where

import qualified Data.CaseInsensitive as Case
import Data.Aeson
import Data.AWS.Runtime.Invocation (Event)
import qualified Data.AWS.Startup as Startup
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import Hreq.Client

data Context a
  = Context
      { contextAWSEnv :: Startup.Env,
        contextEventHeaders :: [Header],
        contextEvent :: Event,
        contextEnv :: a
      } deriving (Generic)

newtype OriginalHeaders = OriginalHeaders { unOriginalHeaders :: [Header] }

instance ToJSON OriginalHeaders where
  toJSON =
    object
    . fmap (\(k, v) -> T.decodeUtf8 (Case.original k) .= T.decodeUtf8 v)
    . unOriginalHeaders

instance ToJSON a => ToJSON (Context a) where
  toJSON ctx = object
    [ "contextAWSEnv" .= toJSON (contextAWSEnv ctx)
    , "contextEventHeaders" .= toJSON (OriginalHeaders (contextEventHeaders ctx))
    , "contextEvent" .= contextEvent ctx
    , "contextEnv" .= toJSON (contextEnv ctx)
    ]
