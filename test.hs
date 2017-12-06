{-# LANGUAGE ParallelListComp #-}

import Codec.Picture
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Bits
import Data.Char (chr)
import Data.Word
import qualified Codec.Picture.Types as M
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Internal as BSI (c2w, w2c)
import qualified Data.Vector.Storable as V

-- convert file to word list
fileToWord8 :: String -> IO [Word8]
fileToWord8 fp = do
    contents <- BS.readFile fp
    return $ BS.unpack contents

-- get width of image
getWidth  :: Image a -> Int
getWidth  (Image w _ _) = w

getMutWidth :: M.MutableImage m a -> Int
getMutWidth (M.MutableImage w _ _) = w

-- get height of image
getHeight :: Image a -> Int
getHeight (Image _ h _) = h

-- Functions to get channel values from pixels
getRed   :: PixelRGB8 -> Pixel8
getRed   (PixelRGB8 r g b) = r

getGreen :: PixelRGB8 -> Pixel8
getGreen (PixelRGB8 r g b) = g

getBlue  :: PixelRGB8 -> Pixel8
getBlue  (PixelRGB8 r g b) = b

-- Convert String to [Word8]
convertString :: String -> [Word8]
convertString str = map BSI.c2w (C.unpack (C.pack str))

-- convert Int to Word8
convertIntWord8     :: Int -> Word8
convertIntWord8 num = fromIntegral num

-- null character as word8
nullWord8 = convertIntWord8 0

-- Convert Int to [Bool]
convertIntBits :: Bits a => a -> Int -> [Bool]
convertIntBits x b = map (testBit x) [0..b - 1]

-- Write a set of bits at index i to an image
-- writeBitToImage :: Image PixelRGB8 -> m (M.MutableImage (PrimState m) PixelRGB8) -> Int -> Int -> Int -> Bool -> m0 ()
writeBitToImage orig img bitsPerPixel byteIdx bitIdx bitVal
  | c == 0 = M.writePixel img px py (PixelRGB8 
    (getChangedPixel (getRed (pixelAt orig px py)) d bitVal) 
    (getGreen (pixelAt orig px py)) 
    (getBlue (pixelAt orig px py)))
  | c == 1 = M.writePixel img px py (PixelRGB8 
    (getRed (pixelAt orig px py)) 
    (getChangedPixel (getGreen (pixelAt orig px py)) d bitVal)
    (getBlue (pixelAt orig px py)))
  | c == 2 = M.writePixel img px py (PixelRGB8 
    (getRed (pixelAt orig px py)) 
    (getGreen (pixelAt orig px py)) 
    (getChangedPixel (getBlue (pixelAt orig px py)) d bitVal))
  where 
    a = byteIdx * 8 + bitIdx
    px = mod a (getMutWidth img)
    py = div a (getMutWidth img)
    c = (div (mod a (bitsPerPixel * 3)) bitsPerPixel)
    d = (mod (mod a (bitsPerPixel * 3)) bitsPerPixel)

-- Write a byte at index i to an image
-- writeByteToImage :: Image PixelRGB8 -> M.MutableImage (PrimState m) PixelRGB8 -> Int -> Int -> Word8 -> [Image ()]
writeByteToImage orig img bitsPerPixel idx byte = ()
  -- [writeBitToImage orig img bitsPerPixel idx i bit | i <- [0..8-1] | bit <- (convertIntBits byte 8)]

-- hide data in image
encryptBytes :: Image PixelRGB8 -> String -> [Word8] -> Image PixelRGB8
encryptBytes img name bytes = runST $ do
  mut <- M.unsafeThawImage img
  let 
    go i
      | i >= (length message) = M.unsafeFreezeImage mut 
      | otherwise = do
          writeBitToImage img mut b i 0 (bits!!0)
          writeBitToImage img mut b i 1 (bits!!1)
          writeBitToImage img mut b i 2 (bits!!2)
          writeBitToImage img mut b i 3 (bits!!3)
          writeBitToImage img mut b i 4 (bits!!4)
          writeBitToImage img mut b i 5 (bits!!5)
          writeBitToImage img mut b i 6 (bits!!6)
          writeBitToImage img mut b i 7 (bits!!7)
          go (i + 1)
        where 
          bits = convertIntBits (message!!i) 8
  go 0
  where 
    message = (convertString name) ++ [nullWord8] ++ bytes ++ [nullWord8]
    b = 2 -- bits per channel

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
-- main :: IO ()
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
  
  -- get name of output file
  putStrLn "Give name of output jpg file"
  o <- getLine
  
  -- load image and do stuff
  case imageLoad of
    Left error  -> putStrLn error
    Right image -> do
        savePngImage 100 o (ImageRGB8 (encryptBytes (convertRGB8 image) f d))
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
    
  
  -- write to output file
  -- saveJpgImage 100 o (image PixelRGB8)
  putStrLn ("Saved image to " Prelude.++ o)

