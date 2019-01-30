{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Impl.WriteFileImpl where

import           Control.Monad.IO.Class
import qualified Data.Text                     as Text
import           System.IO                      ( IOMode(..)
                                                , hClose
                                                , hPutStr
                                                , openFile
                                                )

import           CP.Class
import           Impl.App
import           Impl.Handle

instance MonadWriteFile App where
  type WriteFileHandle App = Handle

  openWriteHandle path = do
    handle <- liftIO $ openFile path WriteMode
    return Handle { handle, path = Text.pack path }
  writeFileContents Handle { handle } contents = liftIO $ do
    hPutStr handle contents
    hClose handle
