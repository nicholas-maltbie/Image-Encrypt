-- main function 
main = do
  -- print input to user
  putStrLn "Input data to encrypt file name"
  -- get file name
  f <- getLine
  
  -- read data from file
  putStrLn "Reading data from " ++ f
  d <- readFile f
  putStrLn "Finished reading data from file"
  
  -- Get name of output file
  putStrLn "Give name of output file"
  o <- getLine
  -- Write to output file
  writeFile o d

