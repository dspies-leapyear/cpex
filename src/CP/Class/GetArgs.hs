{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module CP.Class.GetArgs where

import           Control.Monad.Reader           ( ReaderT )
import           Control.Monad.State            ( StateT )
import           Control.Monad.Trans           as Trans

class Monad m => MonadGetArgs m where
  getArgs :: m (String, String)
  default getArgs :: (MonadGetArgs n, MonadTrans t, m ~ t n) => m (String, String)
  getArgs = Trans.lift getArgs

instance MonadGetArgs m => MonadGetArgs (ReaderT r m)
instance MonadGetArgs m => MonadGetArgs (StateT s m)
