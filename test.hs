import Codec.Picture
import Data.Vector
import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Storable as V

-- convert file to word list
fileToWord8 :: String -> IO [Word8]
fileToWord8 fp = do
    contents <- BS.readFile fp
    return $ BS.unpack contents

-- get width of image
getWidth  :: Image PixelRGB8 -> Int
getWidth  (Image w h _) = w

getHeight :: Image PixelRGB8 -> Int
getHeight (Image w h _) = h

-- Functions to get channel values from pixels
getRed   :: PixelRGB8 -> Pixel8
getRed   (PixelRGB8 r g b) = r

getGreen :: PixelRGB8 -> Pixel8
getGreen (PixelRGB8 r g b) = g

getBlue  :: PixelRGB8 -> Pixel8
getBlue  (PixelRGB8 r g b) = b

-- encrypt data into image
encryptBits :: Image PixelRGB8 -> [Word8] -> IO ()
encryptBits img bytes = print w
  where 
    b = 2 -- bits per channel
    l = Prelude.length bytes
    w = getWidth img
    h = getHeight img

-- sets bit 
getChanged :: Int -> Int -> Bool -> Int
getChanged num idx val 
    | val     = num .|. bit idx
    | not val = num .&. complement (bit idx)

-- set pixel bit
getChangedPixel :: Pixel8 -> Int -> Bool -> Pixel8
getChangedPixel px idx val
    | val     = px .|. bit idx
    | not val = px .&. complement (bit idx)

-- main function 
main :: IO ()
main = do
  -- print input to user
  putStrLn "Input data to encrypt file name: "
  
  -- get file name
  f <- getLine
  -- read data from file
  putStrLn ("Reading data from " Prelude.++ f)
  d <- fileToWord8 f
  putStrLn "Finished reading data from file"
  putStrLn ("Read " Prelude.++ (show (Prelude.length d)) Prelude.++ " bytes")
  
  -- read image to encode image into
  putStrLn "What image is being read into: "
  imgf <- getLine
  putStrLn "Reading in image"
  imageLoad <- readImage imgf
  
  -- load image and do stuff
  case imageLoad of
    Left error  -> putStrLn error
    Right image -> encryptBits (convertRGB8 image) d
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

