{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module CP.Class.ReadFile where

import           Control.Monad.Reader           ( ReaderT )
import           Control.Monad.State            ( StateT )
import           Control.Monad.Trans           as Trans

import           CP.Class.IsHandle
import           Transformed

class (Monad m, IsHandle (ReadFileHandle m)) => MonadReadFile m where
  type ReadFileHandle m

  openReadHandle :: String -> m (ReadFileHandle m)
  readFileContents :: ReadFileHandle m -> m String

instance (MonadTrans t, MonadReadFile m, Monad (t m)) => MonadReadFile (Transformed t m) where
  type ReadFileHandle (Transformed t m) = ReadFileHandle m
  openReadHandle = Trans.lift . openReadHandle
  readFileContents = Trans.lift . readFileContents

deriving via Transformed (StateT s) m instance MonadReadFile m => MonadReadFile (StateT s m)
deriving via Transformed (ReaderT r) m instance MonadReadFile m => MonadReadFile (ReaderT r m)
