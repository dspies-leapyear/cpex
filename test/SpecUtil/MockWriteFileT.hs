{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SpecUtil.MockWriteFileT where

import           Control.Monad.IO.Class
import           Control.Monad.State           as State
import           Data.IORef
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

import           CP
import           CP.TH

data MockWriteFileHandle = MockWriteFileHandle
  { writePath :: Text
  , writeContents :: IORef String
  }

instance IsHandle MockWriteFileHandle where
  associatedFilePath = writePath

newtype MockWriteFileT m x = MockWriteFileT (StateT (Map String (IORef String)) m x)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (Map String (IORef String)))

$(deriveAllT ''MockWriteFileT [''MonadWriteFile])

runMockWriteFileT :: MonadIO m => MockWriteFileT m x -> m (x, Map String String)
runMockWriteFileT (MockWriteFileT act) = do
  (result, refMap) <- runStateT act Map.empty
  pairs            <- liftIO $ mapM readIORef refMap
  return (result, pairs)

instance MonadIO m => MonadWriteFile (MockWriteFileT m) where
  type WriteFileHandle (MockWriteFileT m) = MockWriteFileHandle

  openWriteHandle path = do
    writeContents <- State.gets (Map.lookup path) >>= \case
      Nothing -> do
        result <- liftIO $ newIORef ""
        State.modify (Map.insert path result)
        return result
      Just ref -> return ref
    return $ MockWriteFileHandle { writePath = Text.pack path, writeContents }
  writeFileContents MockWriteFileHandle { writeContents } newContents =
    liftIO $ writeIORef writeContents newContents
