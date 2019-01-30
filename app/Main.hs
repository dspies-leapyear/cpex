module Main where

import CP
import Impl.App
import Impl.GetArgsImpl ()
import Impl.ReadFileImpl ()
import Impl.WriteFileImpl ()

main :: IO ()
main = runApp cp
