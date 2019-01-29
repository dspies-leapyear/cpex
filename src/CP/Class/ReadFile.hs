{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module CP.Class.ReadFile where

import           Control.Monad.Reader           ( ReaderT )
import           Control.Monad.State            ( StateT )
import           Control.Monad.Trans           as Trans

import           CP.Class.IsHandle

class (Monad m, IsHandle (ReadFileHandle m)) => MonadReadFile m where
  type ReadFileHandle m

  openReadHandle :: String -> m (ReadFileHandle m)
  default openReadHandle
    :: (MonadTrans t, MonadReadFile n, m ~ t n, ReadFileHandle m ~ ReadFileHandle n)
    => String -> m (ReadFileHandle m)
  openReadHandle = Trans.lift . openReadHandle
  readFileContents :: ReadFileHandle m -> m String
  default readFileContents
    :: (MonadTrans t, MonadReadFile n, m ~ t n, ReadFileHandle m ~ ReadFileHandle n)
    => ReadFileHandle m -> m String
  readFileContents = Trans.lift . readFileContents

instance MonadReadFile m => MonadReadFile (StateT s m) where
  type ReadFileHandle (StateT s m) = ReadFileHandle m
instance MonadReadFile m => MonadReadFile (ReaderT r m) where
  type ReadFileHandle (ReaderT r m) = ReadFileHandle m
