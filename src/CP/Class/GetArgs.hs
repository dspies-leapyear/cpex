{-# LANGUAGE TypeFamilies #-}

module CP.Class.GetArgs where

class Monad m => MonadGetArgs m where
  getArgs :: m (String, String)
