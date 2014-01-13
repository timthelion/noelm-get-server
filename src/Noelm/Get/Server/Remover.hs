module Main where

import System.Environment
import System.Exit
import System.IO

import Noelm.Get.Server.Generate.Listing

main = do
  args <- getArgs
  case args of
    [name] -> do remove name
                 putStrLn "remove function called successfully"
    _      -> do hPutStrLn stderr "Bad command line arguments. Expecting a single project name."
                 exitFailure