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
writeBitToImage orig img bitsPerPixel byteIdx bitIdx bitVal start sf
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
    a = floor (fromIntegral (byteIdx * 8 + bitIdx) * sf)
    p = (div a (bitsPerPixel * 3)) + start 
    px = mod p (getWidth orig)
    py = div p (getWidth orig)
    c = (div (mod a (bitsPerPixel * 3)) bitsPerPixel)
    d = (mod (mod a (bitsPerPixel * 3)) bitsPerPixel)

-- get number of pixels that can store bytes
getNumPixelsForStorage :: Image PixelRGB8 -> Int
getNumPixelsForStorage img = (getWidth img) * (getHeight img) - 64

getPixelsForMessage :: Int -> Int -> Int
getPixelsForMessage len bitsPerPixel = div (len * 8) (bitsPerPixel * 3)

-- getOptium bits per pixel color channel
optiumBits :: Image PixelRGB8 -> Int -> Int
optiumBits img bytes = max (ceiling ((toRational (bytes * 8)) / (toRational ((getNumPixelsForStorage img) * 3)))) 1

maxBytes :: Image PixelRGB8 -> Int -> Int
maxBytes img b = floor ((toRational ((getWidth img) * (getHeight img))) / (toRational (8.0 / (toRational ((toRational b) * 3.0)))))

-- hide data in image
encryptBytes :: Image PixelRGB8 -> Int -> [Word8] -> Either String (Image PixelRGB8)
encryptBytes img bitsPerPixel message 
  | bitsPerPixel <= 8 = Right (runST $ do
      mut <- M.unsafeThawImage img
      let 
        go i []     start b sf = M.freezeImage mut
        go i (x:xs) start b sf = do
              writeBitToImage img mut b i 0 (bits!!0) start sf
              writeBitToImage img mut b i 1 (bits!!1) start sf
              writeBitToImage img mut b i 2 (bits!!2) start sf
              writeBitToImage img mut b i 3 (bits!!3) start sf
              writeBitToImage img mut b i 4 (bits!!4) start sf
              writeBitToImage img mut b i 5 (bits!!5) start sf
              writeBitToImage img mut b i 6 (bits!!6) start sf
              writeBitToImage img mut b i 7 (bits!!7) start sf
              go (i + 1) xs start b sf
            where 
              bits = convertIntBits (x) 8
      go 
        0 
        (convertInt32Word8 len) 
        0 
        2 
        ((toRational (getPixelsForMessage 4 2)) / (toRational 64))
      go 
        0 
        message 
        64 
        bitsPerPixel 
        ((toRational (getPixelsForMessage len bitsPerPixel)) / (toRational (getNumPixelsForStorage img))))
  | otherwise = Left "Too many bytes to encode into image"
  where 
    len = length message

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
  
  --putStrLn "How many bits are stored per pixel color channel? (default 2)"
  --val <- getLine
  --let bitsPerPixel = read val :: Int'
  
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
          conv = (convertRGB8 image)
          len = (length f) + 1 + (length d)
          bitsPerPixel = optiumBits conv len
        print bitsPerPixel
        let
          messgae = (convertString f) ++ [nullWord8] ++ d
          final = (encryptBytes conv bitsPerPixel messgae)
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

