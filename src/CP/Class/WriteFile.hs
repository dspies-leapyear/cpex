{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module CP.Class.WriteFile where

import           CP.Class.IsHandle

class (Monad m, IsHandle (WriteFileHandle m)) => MonadWriteFile m where
  type WriteFileHandle m

  openWriteHandle :: String -> m (WriteFileHandle m)
  writeFileContents :: WriteFileHandle m -> String -> m ()
