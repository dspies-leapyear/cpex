{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Impl.WriteFileImpl where

import           Control.Monad.IO.Class
import           Control.Monad.Logger           ( MonadLogger )
import qualified Data.Text                     as Text
import           System.IO                      ( IOMode(..)
                                                , hClose
                                                , hPutStr
                                                , openFile
                                                )

import           CP.Class
import           Impl.Handle

newtype WriteFileT m x = WriteFileT {runWriteFileT :: m x}
  deriving (Functor, Applicative, Monad, MonadIO, MonadGetArgs, MonadLogger, MonadReadFile)

instance MonadIO m => MonadWriteFile (WriteFileT m) where
  type WriteFileHandle (WriteFileT m) = Handle

  openWriteHandle path = do
    handle <- liftIO $ openFile path WriteMode
    return Handle { handle, path = Text.pack path }
  writeFileContents Handle { handle } contents = liftIO $ do
    hPutStr handle contents
    hClose handle
