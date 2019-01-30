{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module CP.Class.WriteFile where

import           Control.Monad.Reader           ( ReaderT )
import           Control.Monad.State            ( StateT )
import           Control.Monad.Trans           as Trans

import           CP.Class.IsHandle
import           Transformed

class (Monad m, IsHandle (WriteFileHandle m)) => MonadWriteFile m where
  type WriteFileHandle m

  openWriteHandle :: String -> m (WriteFileHandle m)
  writeFileContents :: WriteFileHandle m -> String -> m ()

instance (MonadTrans t, MonadWriteFile m, Monad (t m)) => MonadWriteFile (Transformed t m) where
  type WriteFileHandle (Transformed t m) = WriteFileHandle m
  openWriteHandle = Trans.lift . openWriteHandle
  writeFileContents = (Trans.lift .) . writeFileContents

deriving via (Transformed (StateT s) m) instance MonadWriteFile m => MonadWriteFile (StateT s m)
deriving via (Transformed (ReaderT r) m) instance MonadWriteFile m => MonadWriteFile (ReaderT r m)
