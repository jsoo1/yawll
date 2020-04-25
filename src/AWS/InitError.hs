{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module AWS.InitError where

import Data.AWS.Error as AWS
import qualified Data.AWS.Runtime as Runtime
import qualified Data.AWS.Startup as Startup
import Data.Text (Text)
import Hreq.Client
import Hreq.Core.Client.BaseUrl

type InitError =
  "init"
    :> "error"
    :> ReqHeaders '["Lambda-Runtime-Function-Error-Type" := Text]
    :> ReqBody JSON Error
    :> PostJson Runtime.Status

initError :: (AWS.ToError a, RunClient m) => a -> m Runtime.Status
initError e =
  hreq @InitError $ "Unhandled" :. toError e :. Empty
