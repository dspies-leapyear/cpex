{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Impl.App
  ( App
  , runApp
  )
where

import           Control.Monad.Logger

import           CP.Class
import           Impl.GetArgsImpl
import           Impl.ReadFileImpl
import           Impl.WriteFileImpl

newtype App x = App (WriteFileT (ReadFileT (GetArgsT (LoggingT IO))) x)
  deriving (Functor, Applicative, Monad, MonadGetArgs, MonadLogger, MonadReadFile, MonadWriteFile)

runApp :: App x -> IO x
runApp (App act) =
  runStderrLoggingT $ runGetArgsT $ runReadFileT $ runWriteFileT act
