{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Impl.GetArgsImpl where

import           CP.Class

import           Control.Monad.IO.Class
import           Control.Monad.Logger           ( MonadLogger )
import qualified System.Environment            as IO
                                                ( getArgs )

newtype GetArgsT m x = GetArgsT {runGetArgsT :: m x}
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReadFile, MonadWriteFile)

instance MonadIO m => MonadGetArgs (GetArgsT m) where
  getArgs = liftIO IO.getArgs >>= \case
    [arg1, arg2] -> pure (arg1, arg2)
    _            -> error "Wrong number of arguments"
