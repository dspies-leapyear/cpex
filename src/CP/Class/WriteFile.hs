{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module CP.Class.WriteFile where

import           Control.Monad.Reader           ( ReaderT )
import           Control.Monad.State            ( StateT )
import           Control.Monad.Trans           as Trans

import           CP.Class.IsHandle

class (Monad m, IsHandle (WriteFileHandle m)) => MonadWriteFile m where
  type WriteFileHandle m

  openWriteHandle :: String -> m (WriteFileHandle m)
  default openWriteHandle
    :: (MonadTrans t, MonadWriteFile n, m ~ t n, WriteFileHandle m ~ WriteFileHandle n)
    => String -> m (WriteFileHandle m)
  openWriteHandle = Trans.lift . openWriteHandle
  writeFileContents :: WriteFileHandle m -> String -> m ()
  default writeFileContents
    :: (MonadTrans t, MonadWriteFile n, m ~ t n, WriteFileHandle m ~ WriteFileHandle n)
    => WriteFileHandle m -> String -> m ()
  writeFileContents = (Trans.lift .) . writeFileContents

instance MonadWriteFile m => MonadWriteFile (StateT s m) where
  type WriteFileHandle (StateT s m) = WriteFileHandle m
instance MonadWriteFile m => MonadWriteFile (ReaderT r m) where
  type WriteFileHandle (ReaderT r m) = WriteFileHandle m
