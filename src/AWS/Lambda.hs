{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
 
module AWS.Lambda where

import qualified Data.AWS.Error as AWS
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Except (ExceptT, runExceptT)
import qualified Data.AWS.Runtime as Runtime
import qualified Data.AWS.Startup as Startup

type MonadLambda ctx e m a =
  ReaderT (Runtime.Context ctx) (ExceptT e m) a

runLambda :: ReaderT r (ExceptT e m) a -> r -> m (Either e a)
runLambda handler = runExceptT . runReaderT handler

data Lambda m
  = forall ctx startupError runtimeError.
    (AWS.ToError startupError, AWS.ToError runtimeError) =>
    Lambda
      { lambdaSetup :: Startup.Env -> m (Either startupError ctx),
        lambdaHandler :: MonadLambda ctx runtimeError m Runtime.Response
      }
