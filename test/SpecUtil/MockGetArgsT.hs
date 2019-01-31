{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module SpecUtil.MockGetArgsT where

import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader          as Reader

import           CP
import           CP.TH

newtype MockGetArgsT m x = MockGetArgsT (ReaderT (String, String) m x)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (String, String))

$(deriveAllT ''MockGetArgsT [''MonadGetArgs])

runMockGetArgsT :: (String, String) -> MockGetArgsT m x -> m x
runMockGetArgsT mockArgs (MockGetArgsT act) = runReaderT act mockArgs

instance Monad m => MonadGetArgs (MockGetArgsT m) where
  getArgs = Reader.ask
