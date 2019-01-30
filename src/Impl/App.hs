{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Impl.App
  ( App
  , runApp
  )
where

import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Logger

newtype App x = App (LoggingT IO x)
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger)

runApp :: App x -> IO x
runApp (App act) = runStderrLoggingT act
