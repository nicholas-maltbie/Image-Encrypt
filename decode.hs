{-# LANGUAGE ParallelListComp #-}

import Codec.Picture
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Bits
import Data.Char (chr)
import System.IO.Unsafe
import Data.Word
import qualified Codec.Picture.Types as M
import qualified Data.ByteString as B
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

-- Convert [Word8] to String
convertWord8List :: [Word8] -> String
convertWord8List list = map BSI.w2c list

convertInt32Word8     :: Int -> [Word8]
convertInt32Word8 num = map convertIntWord8 [(shiftR num (i * 8)) .&. 255 | i <- [0..3]]

convertWord8Int        :: [Word8] -> Int
convertWord8Int []     = 0
convertWord8Int [x]    = fromIntegral x
convertWord8Int (x:xs) = (fromIntegral x) + shiftL (convertWord8Int xs) 8

-- convert Int to Word8
convertIntWord8     :: Int -> Word8
convertIntWord8 num = fromIntegral num

-- null character as word8
nullWord8 = convertIntWord8 0

-- Convert Int to [Bool]
convertIntBits :: Bits a => a -> Int -> [Bool]
convertIntBits x b = map (testBit x) [0..b - 1]

-- Reads a bit at index i from an image
readBitFromImage :: Image PixelRGB8 -> Int -> Int -> Int -> Int -> Double -> Bool
readBitFromImage img bitsPerPixel byteIdx bitIdx offset sf
--  | px < 0 || px >= (getWidth img) || py < 0 || py >= (getHeight img) = False
  | c == 0 = readPixelBit (getRed   (pixelAt img px py)) d
  | c == 1 = readPixelBit (getGreen (pixelAt img px py)) d
  | c == 2 = readPixelBit (getBlue  (pixelAt img px py)) d
  where 
    a = floor (fromIntegral (byteIdx * 8 + bitIdx) * sf)
    p = (div a (bitsPerPixel * 3)) + offset 
    px = mod p (getWidth img)
    py = div p (getWidth img)
    c = (div (mod a (bitsPerPixel * 3)) bitsPerPixel)
    d = (mod (mod a (bitsPerPixel * 3)) bitsPerPixel)

-- get number of pixels that can store bytes
getNumPixelsForStorage :: Image PixelRGB8 -> Int
getNumPixelsForStorage img = (getWidth img) * (getHeight img) - 64 - 1

getStorableBits :: Image PixelRGB8 -> Int -> Int
getStorableBits img bitsPerPixel = ((getWidth img) * (getHeight img) - 64 - 1) * bitsPerPixel * 3

getPixelsForMessage :: Int -> Int -> Int
getPixelsForMessage len bitsPerPixel = div (len * 8) (bitsPerPixel * 3)

-- getOptium bits per pixel color channel
optiumBits :: Image PixelRGB8 -> Int -> Int
optiumBits img bytes = max (ceiling ((toRational (bytes * 8)) / (toRational ((getNumPixelsForStorage img) * 3)))) 1

maxBytes :: Image PixelRGB8 -> Int -> Int
maxBytes img b = floor ((toRational ((getWidth img) * (getHeight img))) / (toRational (8.0 / (toRational ((toRational b) * 3.0)))))

-- convert [Bool] to Word8
bitsToWord8 :: [Bool] -> Word8
bitsToWord8 = foldl (\byte bit -> byte*2 + if bit then 1 else 0) 0

-- read byte from an image
readByte :: Image PixelRGB8 -> Int -> Int -> Int -> Double -> Word8
readByte img idx b offset sf = bitsToWord8 [readBitFromImage img b idx i offset sf | i <- reverse [0..8-1]]


readByteStream :: Image PixelRGB8 -> Int -> Int -> Int -> Double -> [Word8]
readByteStream img start b offset sf
  | byte == nullWord8 = [] -- end of stream
  | otherwise         = [byte] ++ readByteStream img (start + 1) b offset sf
  where 
    byte = readByte img start b offset sf

readNBytes :: Image PixelRGB8 -> Int -> Int -> Int -> Int -> Double -> [Word8]
readNBytes img start b n offset sf
  | n <= 0     = []
  | otherwise = (readByte img start b offset sf):(readNBytes img (start + 1) b (n - 1) offset sf)

-- Decrypt bites from file
decryptBytes :: Image PixelRGB8 -> IO ()
decryptBytes img = do
  putStrLn ("Found file '" ++ filePath ++ "', " ++ (show len) ++ " total bytes")
  putStrLn ("What would you like to save the file as?")
  o <- getLine
  BS.writeFile o (BS.pack file)
  putStrLn ("Saved output to " ++ o)
  where
    len = fromIntegral (convertWord8Int (readNBytes 
      img 
      0 
      1 
      4 
      0 
      (63 / (fromIntegral (getPixelsForMessage 4 1)))
      ))
    bitsPerPixel = optiumBits img len
    b = bitsPerPixel
    name = readByteStream 
      img 
      0 
      b 
      64 
      ((fromIntegral (getStorableBits img bitsPerPixel)) / (fromIntegral (len * 8)))
    filePath = convertWord8List name
    file = readNBytes 
      img 
      ((length name) + 1) 
      b 
      (len - ((length name) + 1)) 
      64
      ((fromIntegral (getStorableBits img bitsPerPixel)) / (fromIntegral (len * 8)))


readPixelBit :: Pixel8 -> Int -> Bool
readPixelBit px idx = testBit px idx

-- main function 
-- main :: IO ()
main = do
  -- read image to encode image into
  putStrLn "What image is being decoded: "
  imgf <- getLine
  putStrLn "Reading in image"
  imageLoad <- readImage imgf
  
  --putStrLn "How many bits are stored per pixel color channel? (default 2)"
  --val <- getLine
  --let bitsPerPixel = read val :: Int
  
  -- load image and do stuff
  case imageLoad of
    Left error  -> putStrLn error
    Right image -> do
        let 
          conv = (convertRGB8 image)
        decryptBytes conv

