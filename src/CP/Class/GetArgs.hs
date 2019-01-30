{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module CP.Class.GetArgs where

import           Control.Monad.Reader           ( ReaderT )
import           Control.Monad.State            ( StateT )
import           Control.Monad.Trans           as Trans

import           Transformed

class Monad m => MonadGetArgs m where
  getArgs :: m (String, String)

instance (MonadTrans t, MonadGetArgs m, Monad (t m)) => MonadGetArgs (Transformed t m) where
  getArgs = Trans.lift getArgs

deriving via Transformed (ReaderT r) m instance MonadGetArgs m => MonadGetArgs (ReaderT r m)
deriving via Transformed (StateT s) m instance MonadGetArgs m => MonadGetArgs (StateT s m)
