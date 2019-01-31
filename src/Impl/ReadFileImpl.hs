{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Impl.ReadFileImpl where

import           Control.Monad.IO.Class
import qualified Data.Text                     as Text
import           System.IO                      ( IOMode(..)
                                                , hGetContents
                                                , openFile
                                                )

import           CP.Class
import           CP.TH
import           Impl.Handle

newtype ReadFileT m x = ReadFileT {runReadFileT :: m x}
  deriving (Functor, Applicative, Monad, MonadIO)

$(deriveAllT ''ReadFileT [''MonadReadFile])

instance MonadIO m => MonadReadFile (ReadFileT m) where
  type ReadFileHandle (ReadFileT m) = Handle

  openReadHandle path = do
    handle <- liftIO $ openFile path ReadMode
    return Handle { handle, path = Text.pack path }
  readFileContents Handle { handle } = liftIO $ hGetContents handle
