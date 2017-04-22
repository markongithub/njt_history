module Main where
  import System.Environment as Environment
  import NJTHistory

  main = do
    args <- Environment.getArgs
    let allFiles = args
    output <- readStatusFiles allFiles
    putStrLn $ show output
