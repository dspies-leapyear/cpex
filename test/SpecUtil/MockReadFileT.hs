{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SpecUtil.MockReadFileT where

import           Control.Monad.Logger           ( MonadLogger )
import           Control.Monad.Reader          as Reader
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

import           CP

data MockReadFileHandle = MockReadFileHandle
  { readPath :: Text
  , readContents :: String
  }

instance IsHandle MockReadFileHandle where
  associatedFilePath = readPath

type MockReadFiles = Map String String -- Map file name to contents

newtype MockReadFileT m x = MockReadFileT (ReaderT MockReadFiles m x)
  deriving (Functor, Applicative, Monad, MonadGetArgs, MonadIO, MonadLogger, MonadWriteFile, MonadReader MockReadFiles)

runMockReadFileT :: MockReadFiles -> MockReadFileT m x -> m x
runMockReadFileT mockFS (MockReadFileT act) = runReaderT act mockFS

instance Monad m => MonadReadFile (MockReadFileT m) where
  type ReadFileHandle (MockReadFileT m) = MockReadFileHandle

  openReadHandle path = do
    readContents <- Reader.asks (Map.! path)
    return $ MockReadFileHandle { readPath = Text.pack path, readContents }
  readFileContents = pure . readContents
