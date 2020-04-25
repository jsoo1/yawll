{-# LANGUAGE OverloadedStrings #-}

module AWS.Startup
  ( env,
  )
where

import Control.Error.Util (note)
import Data.AWS.Startup
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Natural (Natural)
import Hreq.Client
import System.Posix.Env (getEnv)

env :: IO (Either Error Env)
env =
  do
    h <- getEnv' handlerVar
    t <- getEnv' taskRootVar
    r <- fmap (T.splitOn ":") <$> getEnv' runtimeApiVar
    pure $ Env <$> h <*> t <*> apiUrl r
  where
    getEnv' var =
      note (VarNotFound (T.pack var)) . fmap T.pack <$> getEnv var
    apiUrl ts = do
      items <- ts
      let err = InvalidRuntimeApi $ T.intercalate ":" items
      case items of
        [host, port] -> RuntimeApi host <$> p
          where
            p :: Either Error Natural
            p = note err $ decodeStrict' $ T.encodeUtf8 port
        _ -> Left err
    handlerVar = "_HANDLER"
    taskRootVar = "LAMBDA_TASK_ROOT"
    runtimeApiVar = "AWS_LAMBDA_RUNTIME_API"
