import Codec.Picture

-- main function 
main :: IO ()
main = do
  -- print input to user
  putStrLn "Input data to encrypt file name: "
  
  -- get file name
  f <- getLine
  -- read data from file
  putStrLn ("Reading data from " ++ f)
  d <- readFile f
  putStrLn "Finished reading data from file"
  
  -- read image to encode image into
  putStrLn "What image is being read into: "
  img <- getLine
  putStrLn "Reading in image"
  imageLoad <- readImage img
  
  -- load image and do stuff
  case imageLoad of
    Left error  -> putStrLn error
    Right (ImageRGB8 image) -> do
      px <- pixelAt 0 0 image
    
      -- get name of output file
      putStrLn "Give name of output jpg file"
      o <- getLine
      
      -- write to output file
      -- saveJpgImage 100 o (image PixelRGB8)
      putStrLn ("Saved image to " ++ o)
    Right _ -> putStrLn "Unxexpected Pixel Format"

