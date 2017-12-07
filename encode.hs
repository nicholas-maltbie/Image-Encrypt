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

-- Write a set of bits at index i to an image
-- writeBitToImage :: Image PixelRGB8 -> m (M.MutableImage (PrimState m) PixelRGB8) -> Int -> Int -> Int -> Bool -> m0 ()
writeBitToImage orig img bitsPerPixel byteIdx bitIdx bitVal
  | px < 0 || px >= (getWidth orig) || py < 0 || py >= (getHeight orig) = M.writePixel img
    0 0 (PixelRGB8 
    (getRed (pixelAt orig 0 0))
    (getGreen (pixelAt orig 0 0)) 
    (getBlue (pixelAt orig 0 0)))
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
    p = (div a (bitsPerPixel * 3))
    px = mod p (getWidth orig)
    py = div p (getWidth orig)
    c = (div (mod a (bitsPerPixel * 3)) bitsPerPixel)
    d = (mod (mod a (bitsPerPixel * 3)) bitsPerPixel)

-- Write a byte at index i to an image
-- writeByteToImage :: Image PixelRGB8 -> M.MutableImage (PrimState m) PixelRGB8 -> Int -> Int -> Word8 -> [Image ()]
writeByteToImage orig img bitsPerPixel idx byte = ()
  -- [writeBitToImage orig img bitsPerPixel idx i bit | i <- [0..8-1] | bit <- (convertIntBits byte 8)]

maxBytes :: Image PixelRGB8 -> Int -> Int
maxBytes img b = floor ((toRational ((getWidth img) * (getHeight img))) / (toRational (8.0 / (toRational ((toRational b) * 3.0)))))

-- hide data in image
encryptBytes :: Image PixelRGB8 -> Int -> [Word8] -> Either String (Image PixelRGB8)
encryptBytes img bitsPerPixel message 
  | (length message) < maxBytes img b = Right (runST $ do
      mut <- M.unsafeThawImage img
      let 
        go i [] = M.freezeImage mut
        go i (x:xs) = do
              writeBitToImage img mut b i 0 (bits!!0)
              writeBitToImage img mut b i 1 (bits!!1)
              writeBitToImage img mut b i 2 (bits!!2)
              writeBitToImage img mut b i 3 (bits!!3)
              writeBitToImage img mut b i 4 (bits!!4)
              writeBitToImage img mut b i 5 (bits!!5)
              writeBitToImage img mut b i 6 (bits!!6)
              writeBitToImage img mut b i 7 (bits!!7)
              go (i + 1) xs
            where 
              bits = convertIntBits (x) 8
      go 0 message)
  | otherwise = Left "Too many bytes to encode into image"
  where 
    b = bitsPerPixel -- bits per channel

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

saveGif :: FilePath -> DynamicImage -> IO ()
saveGif f img = do
  let result = saveGifImage f img
  case result of 
    Left error -> putStrLn error
    Right good -> good

getImageFn :: String -> (FilePath -> DynamicImage -> IO ())
getImageFn output_format
  | output_format == "png"  = savePngImage
--  | output_format == "jpg"  = saveJpgImage 255
  | output_format == "bmp"  = saveBmpImage
  | output_format == "gif"  = saveGif
  | output_format == "tiff" = saveTiffImage
  | otherwise               = savePngImage

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
  
  putStrLn "How many bits are stored per pixel color channel? (default 2)"
  val <- getLine
  let bitsPerPixel = read val :: Int
  
  -- read image to encode image into
  putStrLn "What image is being read into: "
  imgf <- getLine
  putStrLn "Reading in image"
  imageLoad <- readImage imgf
  -- load image and do stuff
  case imageLoad of
    Left error  -> putStrLn error
    Right image -> do
        let 
          messgae = (convertInt32Word8 ((length f) + 1 + (length d))) ++ 
                     (convertString f) ++ [nullWord8] ++ d
          final = (encryptBytes (convertRGB8 image) bitsPerPixel messgae)
        case final of 
          Left errorStr   -> putStrLn errorStr
          Right encrypted -> do 
            putStrLn "Which format is the output file? ['png', 'bmp', 'gif', 'tiff']: "
            sel <- getLine
            let 
              image_fn = getImageFn sel
            -- get name of output file
            putStrLn "Give name of output file"
            o <- getLine
            image_fn o (ImageRGB8 encrypted)
            
            putStrLn ("Saved image to " Prelude.++ o)

