{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module AWS.Invocation where

import Data.AWS.Runtime as Runtime
import qualified Data.AWS.Runtime.Response as Response
import Data.Text (Text)
import Hreq.Client
import Hreq.Core.Client.BaseUrl

type Next =
  "invocation"
    :> "next"
    :> Get
         '[ ResBody JSON Event,
            ResHeaders
              '[ "Lambda-Runtime-Aws-Request-Id" := Text,
                 "Lambda-Runtime-Trace-Id" := Text,
                 "Lambda-Runtime-Client-Context" := Text,
                 "Lambda-Runtime-Cognito-Identity" := Text,
                 "Lambda-Runtime-Deadline-Ms" := Text,
                 "Lambda-Runtime-Invoked-Function-Arn" := Text
               ]
          ]

-- | For use with @runtimeApiUrl.
next :: RunClient m => m (Hlist '[Event, [Header]])
next =
  hreq @Next Empty

type InvocationError =
  ReqHeaders '["Lambda-Runtime-Function-Error-Type" := Text]
    :> Capture Text
    :> Capture Text
    :> ReqBody PlainText Text
    :> PostJson Runtime.Status

error :: RunClient m => Text -> Response.Response -> m Runtime.Status
error reqId (Response.Response x) =
  hreq @InvocationError $
    "InvocationError" :. reqId :. "error" :. toText x :. Empty

type InvocationResponse =
  Capture Text
    :> Capture Text
    :> ReqBody PlainText Text
    :> PostJson Runtime.Status

response :: RunClient m => Text -> Response.Response -> m Runtime.Status
response reqId (Response.Response x) =
  hreq @InvocationResponse $ reqId :. "response" :. toText x :. Empty
