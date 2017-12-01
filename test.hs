import Codec.Picture
import Data.Vector
import qualified Data.Vector.Storable as V

getRed :: PixelRGB8 -> Pixel8
getRed (PixelRGB8 r g b) = r

test :: Image PixelRGB8 -> IO ()
test img = print (getRed (pixelAt img 0 0))

-- main function 
main :: IO ()
main = do
  -- print input to user
  putStrLn "Input data to encrypt file name: "
  
  -- get file name
  f <- getLine
  -- read data from file
  putStrLn ("Reading data from " Prelude.++ f)
  d <- readFile f
  putStrLn "Finished reading data from file"
  
  -- read image to encode image into
  putStrLn "What image is being read into: "
  imgf <- getLine
  putStrLn "Reading in image"
  imageLoad <- readImage imgf
  
  -- load image and do stuff
  case imageLoad of
    Left error  -> putStrLn error
    Right image -> test (convertRGB8 image)
    --Right (ImageY8      image)  -> putStrLn "Y8"
    --Right (ImageY16     image)  -> putStrLn "Y16"
    --Right (ImageYA8     image)  -> putStrLn "YA8"
    --Right (ImageYA16    image)  -> putStrLn "YA16"
    --Right (ImageCMYK8   image)  -> putStrLn "CMYK8"
    --Right (ImageYCbCr8  image)  -> putStrLn "CbCr8"
    --Right (ImageRGB8    image)  -> putStrLn "RGB8"
    --Right (ImageRGBA8   image)  -> putStrLn "RGBA8"
    --Right (ImageRGB16   image)  -> putStrLn "RGB16"
    --Right (ImageRGBA16  image)  -> putStrLn "RGBA16"
    
    --Right _ -> putStrLn "Unxexpected Pixel Format"
    
  -- get name of output file
  putStrLn "Give name of output jpg file"
  o <- getLine
  
  -- write to output file
  -- saveJpgImage 100 o (image PixelRGB8)
  putStrLn ("Saved image to " Prelude.++ o)

