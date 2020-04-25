{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module AWS
  ( module AWS.InitError,
    module AWS.Invocation,
    module AWS.Lambda,
    module AWS.Startup,
    runtimePath,
    runtimeApiUrl,
    invocationUrl,
    failOnInit,
    lambdaLoop,
  )
where

import AWS.InitError
import AWS.Invocation
import AWS.Lambda
import AWS.Startup
import Data.AWS.Error
import qualified Data.AWS.Runtime as Runtime
import Data.AWS.Startup
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.UTF8 as BLU
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Hreq.Client
import System.Environment (getEnv, setEnv)
import System.Exit
import Prelude hiding (error)

-- | Base path for all aws lambda endpoints
runtimePath :: Text
runtimePath = "2018-06-01/runtime"

-- | For use with the init error and next invocation endpoints
runtimeApiUrl :: RuntimeApi -> BaseUrl
runtimeApiUrl api =
  (HttpUrl (runtimeApiHost api) runtimePath)
    { baseUrlPort = runtimeApiPort api
    }

-- | Use to create the @BaseUrl for the invocation runtime endpoints (i.e. response and error)
-- | For use with the parameterized invocation endpoints (i.e. response and error).
invocationUrl :: RuntimeApi -> BaseUrl
invocationUrl api =
  runtimeUrl {baseUrlPath = baseUrlPath runtimeUrl <> "/invocation"}
  where
    runtimeUrl = runtimeApiUrl api

-- | Fail on startup. For use before the environment has been parsed
-- | AWS requires the runtime to POST to "/init/error" if there was a
-- | startup error.
failOnInit :: ToError e => e -> IO ()
failOnInit e = do
  runtimeApi <- T.pack <$> getEnv "AWS_LAMBDA_RUNTIME_API"
  let initErrorUrl = HttpUrl runtimeApi $ runtimePath <> "/init/error"
  BLC.putStrLn $ encode $ toError e
  stat <- runHreq initErrorUrl $ initError e
  BLC.putStrLn $ encode stat
  exitWith $ ExitFailure 1

-- | Run a lambda in AWS. Includes context startup and run loop.
-- | This is due to the existential quantification of the lambda.
-- | To use this, implement a @Lambda and use @Startup.env to
-- | get the required environment variables. Non-terminating.
lambdaLoop :: Env -> Lambda IO -> IO ()
lambdaLoop contextAWSEnv@Env {..} lambdaFn@Lambda {..} =
  lambdaSetup contextAWSEnv >>= either failOnInit loop
  where
    loop contextEnv = do
      (contextEvent :. contextEventHeaders :. Empty) <-
        runHreq (runtimeApiUrl runtimeApi) next
      reqId <-
        maybe (fail "Request Id not found in next invocation") pure $
          lookup "Lambda-Runtime-Aws-Request-Id" contextEventHeaders
      traceId <-
        maybe (fail "Trace Id not found in next invocation") pure $
          lookup "Lambda-Runtime-Trace-Id" contextEventHeaders
      setEnv "_X_AMZN_TRACE_ID" $ BLU.toString traceId
      runLambda lambdaHandler Runtime.Context {..} >>= \case
        Left e -> do
          stat <- runHreq (invocationUrl runtimeApi) reportError
          loop contextEnv
          where
            errResponse = Runtime.Response $ TextErr e
            reportError = error (T.decodeUtf8 reqId) errResponse
        Right res -> do
          stat <- runHreq (invocationUrl runtimeApi) respondSuccess
          loop contextEnv
          where
            respondSuccess = response (T.decodeUtf8 reqId) res
