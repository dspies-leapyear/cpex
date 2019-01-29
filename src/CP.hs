{-# LANGUAGE TemplateHaskell #-}

module CP
    ( cp
    , module X
    ) where

import Control.Monad.Logger (MonadLogger, logInfo)
import qualified Data.Text as Text

import CP.Class as X

cp :: (MonadGetArgs m, MonadLogger m, MonadReadFile m, MonadWriteFile m) => m ()
cp = do
  (infileName, outfileName) <- getArgs
  infileHandle <- openReadHandle infileName
  outfileHandle <- openWriteHandle outfileName
  $(logInfo) $ Text.unwords ["Reading from", associatedFilePath infileHandle]
  contents <- readFileContents infileHandle
  $(logInfo) $ Text.unwords ["Writing to", associatedFilePath outfileHandle]
  writeFileContents outfileHandle contents
  $(logInfo) "Contents written"
