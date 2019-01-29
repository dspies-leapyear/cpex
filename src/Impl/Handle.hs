module Impl.Handle
    ( Handle(..)
    ) where

import Data.Text (Text)
import qualified System.IO as IO

import CP.Class.IsHandle

data Handle = Handle
  { handle :: IO.Handle
  , path :: Text
  }

instance IsHandle Handle where
  associatedFilePath = path
