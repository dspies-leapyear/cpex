{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Logger
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Test.Hspec

import           CP
import           Impl.ReadFileImpl
import           SpecUtil.MockGetArgsT
import           SpecUtil.MockReadFileT
import           SpecUtil.MockWriteFileT

main :: IO ()
main = hspec spec

newtype MockServices x =
    MockServices (MockWriteFileT (MockReadFileT (MockGetArgsT (NoLoggingT IO))) x)
  deriving (Functor, Applicative, Monad, MonadIO, MonadGetArgs, MonadLogger, MonadReadFile, MonadWriteFile)

runMockServices
  :: (String, String)
  -> Map String String
  -> MockServices x
  -> IO (x, Map String String)
runMockServices args infileContents (MockServices act) =
  runNoLoggingT
    $ runMockGetArgsT args
    $ runMockReadFileT infileContents
    $ runMockWriteFileT act

spec :: Spec
spec = describe "cp" $ do
  it "should copy a file" $ do
    ((), result) <- runMockServices
      ("input.txt", "output.txt")
      (Map.fromList [("input.txt", "Hello world!")])
      cp
    result `shouldBe` Map.fromList [("output.txt", "Hello world!")]
  it "should read from a real file" $ do
    ((), result) <-
      runMockServices ("test/test_input.txt", "output.txt") (error "unused")
        $ runReadFileT cp
    result `shouldBe` Map.fromList [("output.txt", "Hello world!\n")]
