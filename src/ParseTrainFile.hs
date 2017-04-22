module Main where
  import System.Environment as Environment
  import NJTHistory

  main = do
    args <- Environment.getArgs
    let fileName = head args
    tz <- tzFileIO
    putStrLn ("Now I shall attempt to parse " ++ fileName)
    output <- readStatusFile tz fileName
    putStrLn $ show $ output
