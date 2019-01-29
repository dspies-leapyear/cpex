module CP.Class.IsHandle where

import Data.Text (Text)

class IsHandle h where
  associatedFilePath :: h -> Text
