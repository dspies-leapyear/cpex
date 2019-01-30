{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Transformed
    ( Transformed(..)
    ) where

import Control.Monad.Trans (MonadTrans(..))

newtype Transformed t (m :: * -> *) x = Transformed (t m x)
  deriving (Functor, Applicative, Monad, MonadTrans)
