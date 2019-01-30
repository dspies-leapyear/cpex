{-# OPTIONS_GHC -Wno-orphans #-}

module Impl.GetArgsImpl where

import           Impl.App
import           CP.Class

import           Control.Monad.IO.Class
import qualified System.Environment            as IO
                                                ( getArgs )

instance MonadGetArgs App where
  getArgs = liftIO IO.getArgs >>= \case
    [arg1, arg2] -> pure (arg1, arg2)
    _            -> error "Wrong number of arguments"
