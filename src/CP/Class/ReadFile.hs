{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module CP.Class.ReadFile where

import           CP.Class.IsHandle

class (Monad m, IsHandle (ReadFileHandle m)) => MonadReadFile m where
  type ReadFileHandle m

  openReadHandle :: String -> m (ReadFileHandle m)
  readFileContents :: ReadFileHandle m -> m String
