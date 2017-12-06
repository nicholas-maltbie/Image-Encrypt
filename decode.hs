{-# LANGUAGE ParallelListComp #-}

import Codec.Picture
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Bits
import Data.Char (chr)
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

-- convert Int to Word8
convertIntWord8     :: Int -> Word8
convertIntWord8 num = fromIntegral num

-- null character as word8
nullWord8 = convertIntWord8 0

-- Convert Int to [Bool]
convertIntBits :: Bits a => a -> Int -> [Bool]
convertIntBits x b = map (testBit x) [0..b - 1]

-- Reads a bit at index i from an image
readBitFromImage :: Image PixelRGB8 -> Int -> Int -> Int -> Bool
readBitFromImage img bitsPerPixel byteIdx bitIdx
  | c == 0 = readPixelBit (getRed (pixelAt img px py)) d
  | c == 1 = readPixelBit (getGreen (pixelAt img px py)) d
  | c == 2 = readPixelBit (getBlue (pixelAt img px py)) d
  where 
    a = byteIdx * 8 + bitIdx
    px = mod a (getWidth img)
    py = div a (getWidth img)
    c = (div (mod a (bitsPerPixel * 3)) bitsPerPixel)
    d = (mod (mod a (bitsPerPixel * 3)) bitsPerPixel)

-- convert [Bool] to Word8
bitsToWord8 :: [Bool] -> Word8
bitsToWord8 = foldl (\byte bit -> byte*2 + if bit then 1 else 0) 0

-- read byte from an image
readByte :: Image PixelRGB8 -> Int -> Int -> Word8
readByte img idx b = bitsToWord8 [readBitFromImage img b idx i | i <- reverse [0..8-1]]

readByteStream :: Image PixelRGB8 -> Int -> Int -> [Word8]
readByteStream img start b 
  | byte == nullWord8 = [] -- end of stream
  | otherwise         = [byte] ++ readByteStream img (start + 1) b
  where byte = readByte img start b

-- Decrypt bites from file
decryptBytes :: Image PixelRGB8 -> IO ()
decryptBytes img = do
  BS.writeFile filePath (BS.pack file)
  (print ("Saved output to " ++ filePath))
  where
    b = 2 --bits used per pixel
    name = readByteStream img 0 b
    filePath = convertWord8List name
    file = readByteStream img ((length name) + 1) b
  

-- hide data in image
--encryptBytes :: Image PixelRGB8 -> String -> [Word8] -> Image PixelRGB8
--encryptBytes img name bytes = runST $ do
--  mut <- M.unsafeThawImage img
--  let 
--    go i
--      | i >= (length message) = M.unsafeFreezeImage mut 
--      | otherwise = do
--          writeBitToImage img mut b i 0 (bits!!0)
--          writeBitToImage img mut b i 1 (bits!!1)
--          writeBitToImage img mut b i 2 (bits!!2)
--          writeBitToImage img mut b i 3 (bits!!3)
--          writeBitToImage img mut b i 4 (bits!!4)
--          writeBitToImage img mut b i 5 (bits!!5)
--          writeBitToImage img mut b i 6 (bits!!6)
--          writeBitToImage img mut b i 7 (bits!!7)
--          go (i + 1)
--        where 
--          bits = convertIntBits (message!!i) 8
--  go 0
--  where 
--    message = (convertString name) ++ [nullWord8] ++ bytes ++ [nullWord8]
--    b = 2 -- bits per channel

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
  
  -- load image and do stuff
  case imageLoad of
    Left error  -> putStrLn error
    Right image -> do
        decryptBytes (convertRGB8 image)

