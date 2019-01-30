{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Impl.ReadFileImpl where

import           Control.Monad.IO.Class
import qualified Data.Text                     as Text
import           System.IO                      ( IOMode(..)
                                                , hGetContents
                                                , openFile
                                                )

import           CP.Class
import           Impl.App
import           Impl.Handle

instance MonadReadFile App where
  type ReadFileHandle App = Handle

  openReadHandle path = do
    handle <- liftIO $ openFile path ReadMode
    return Handle { handle, path = Text.pack path }
  readFileContents Handle { handle } = liftIO $ hGetContents handle
