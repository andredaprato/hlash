{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Lib.App.Monad where

import Relude.Extra.Bifunctor (firstF)
import Control.Exception (try, throwIO, catch)
import Control.Monad.Except (MonadError(..))
import Servant.Server (Handler)
import Control.Monad.Except (liftEither)
import Control.Monad.Trans.Control
import Control.Monad.Base

import Lib.App.Env (Env)
import Lib.App.Error 

type AppEnv = Env App

newtype App a = App { unApp :: ReaderT AppEnv IO a
                    } deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadBaseControl IO,  MonadReader AppEnv)

instance MonadError AppError App where
    throwError :: AppError -> App a
    throwError = liftIO . throwIO . AppException
    {-# INLINE throwError #-}

    catchError :: App a -> (AppError -> App a) -> App a
    catchError action handler = App $ ReaderT $ \env -> do
        let ioAction = runApp env action
        ioAction `catch` \(AppException e) -> runApp env $ handler e
    {-# INLINE catchError #-}

{- | Helper for running route handlers in IO. Catches exception of type
'AppException' and unwraps 'AppError' from it.
Do not use this function to run the application. Use runners with logging from
"Lib.Effects.Log" module to also log the error.
-}
runAppAsIO :: AppEnv -> App a -> IO (Either AppError a)
runAppAsIO env = firstF unAppException . try . runApp env

{- | Helper for running 'App'.
Do not use this function to run the application. Use runners with logging from
"Lib.Effects.Log" module to also log the error.
-}
runApp :: AppEnv -> App a -> IO a
runApp env = usingReaderT env . unApp
 
runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env app = do
  res <- liftIO $ runAppAsIO env app
  liftEither $ first toHttpError res
