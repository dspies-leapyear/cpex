{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Logger
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Test.Hspec

import           CP
import           CP.TH                          ( deriveAll )
import           Impl.ReadFileImpl
import           SpecUtil.MockGetArgsT
import           SpecUtil.MockReadFileT
import           SpecUtil.MockWriteFileT

newtype MockServices x =
    MockServices (MockWriteFileT (MockReadFileT (MockGetArgsT (NoLoggingT IO))) x)
  deriving (Functor, Applicative, Monad, MonadIO)

$(deriveAll ''MockServices [])

main :: IO ()
main = hspec spec

data MockServicesEnv = MockServicesEnv
  { cmdLineArgs :: (String, String)
  , inputFileSystem :: Map String String
  }

defaultMockServicesEnv :: MockServicesEnv
defaultMockServicesEnv = MockServicesEnv
  { cmdLineArgs     = ("input.txt", "output.txt")
  , inputFileSystem = Map.empty
  }

runMockServices
  :: MockServicesEnv -> MockServices x -> IO (x, Map String String)
runMockServices MockServicesEnv {..} (MockServices act) =
  runNoLoggingT
    $ runMockGetArgsT cmdLineArgs
    $ runMockReadFileT inputFileSystem
    $ runMockWriteFileT act

-- Tests behave as if there are separate input and output file systems so that we can separately
-- mock either one.

spec :: Spec
spec = describe "cp" $ do
  it "should copy a file" $ do
    ((), result) <- runMockServices
      defaultMockServicesEnv
        { inputFileSystem = Map.fromList [("input.txt", "Hello world!")]
        }
      cp
    result `shouldBe` Map.fromList [("output.txt", "Hello world!")]
  it "should read from a real file" $ do
    ((), result) <-
      runMockServices defaultMockServicesEnv
          { cmdLineArgs = ("test/test_input.txt", "output.txt")
          }
        $ runReadFileT cp
    result `shouldBe` Map.fromList [("output.txt", "Hello world!\n")]
