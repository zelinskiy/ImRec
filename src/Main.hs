module Main where

import qualified Graphics.GD as GD
import Control.Monad (mapM_,foldM,forM_, when)
import Control.Applicative ((<$>),(<*>))
import Data.Array ((!))
import Data.Array.ST (newArray, writeArray, readArray, runSTArray)
import qualified Data.Foldable as F (maximum)
import System.Environment (getArgs, getProgName)

import CV.Image

-- Library JuicyPixels:
import Codec.Picture (DynamicImage(ImageRGB8, ImageRGBA8), Image,
                      PixelRGB8(PixelRGB8), PixelRGBA8(PixelRGBA8),
                      imageWidth, imageHeight, pixelAt, generateImage,
                      readImage, pixelMap, savePngImage)
import Codec.Picture.Types (extractLumaPlane, dropTransparency)

dot :: Num a => (a, a) -> (a, a) -> a
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

mag :: Floating a => (a, a) -> a
mag a = sqrt $ dot a a

sub :: Num a => (a, a) -> (a, a) -> (a, a)
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

fromIntegralP :: (Integral a, Num b) => (a, a) -> (b, b)
fromIntegralP (x, y) = (fromIntegral x, fromIntegral y)

{-
  Create a Hough space image with y+ measuring the distance from
  the center of the input image on the range of 0 to half the hypotenuse
  and x+ measuring from [0, 2 * pi].
  The origin is in the upper left, so y is increasing down.
  The image is scaled according to thetaSize and distSize.
-}
hough :: Image PixelRGB8 -> Int -> Int -> Image PixelRGB8
hough image thetaSize distSize = hImage
  where
    width = imageWidth image
    height = imageHeight image
    wMax = width - 1
    hMax = height - 1
    xCenter = wMax `div` 2
    yCenter = hMax `div` 2

    lumaMap = extractLumaPlane image

    gradient x y =
      let orig = pixelAt lumaMap x y
          x' = pixelAt lumaMap (min (x + 1) wMax) y
          y' = pixelAt lumaMap x (min (y + 1) hMax)
      in fromIntegralP (orig - x', orig - y')

    gradMap = [((x, y), gradient x y) | x <- [0..wMax], y <- [0..hMax]]

    -- The longest distance from the center, half the hypotenuse of the image.
    distMax :: Double
    distMax = (sqrt . fromIntegral $ height ^ 2 + width ^ 2) / 2

    {-
      The accumulation bins of the polar values.
      For each value in the gradient image, if the gradient length exceeds
      some threshold, consider it evidence of a line and plot all of the
      lines that go through that point in Hough space.
    -}
    accBin = runSTArray $ do
      arr <- newArray ((0, 0), (thetaSize, distSize)) 0
      forM_ gradMap $ \((x, y), grad) -> do
        let (x', y') = fromIntegralP $ (xCenter, yCenter) `sub` (x, y)

        when (mag grad > 127) $
          forM_ [0..thetaSize] $ \theta -> do
            let theta' = (fromIntegral theta) * 360 / (fromIntegral thetaSize)
                         / 180 * pi :: Double
                dist = (cos theta' * x' + sin theta' * y')
                dist' = truncate $ dist * (fromIntegral distSize) / distMax
                idx = (theta, dist')

            when (dist' >= 0 && dist' < distSize) $ do
              old <- readArray arr idx
              writeArray arr idx $ old + 1

      return arr

    maxAcc = F.maximum accBin

    -- The image representation of the accumulation bins.
    hTransform x y =
      let l = 255 - (truncate $ (accBin ! (x, y)) / maxAcc * 255)
      in PixelRGB8 l l l

    hImage = generateImage hTransform thetaSize distSize



type RGBA = (Int,Int,Int,Int)

-- | Utility function for clamping a value between a minimum and maximum value
clamp :: (Ord a, Num a) =>  a       -- ^ Minimum
                            -> a    -- ^ Maximum
                            -> a    -- ^ Value to clamp
                            -> a
clamp minm maxm num
  | num < minm = minm
  | num > maxm = maxm
  | otherwise = num




convolute ::    GD.Image
                -> [[Float]]    -- ^ Convolution matrix
                -> Float        -- ^ Divisor
                -> Float        -- ^ Offset
                -> IO ()
convolute img matrix fdiv offset = do
    (width,height) <- GD.imageSize img
    imgCpy <- GD.copyImage img
    mapM_ (\(x,y) ->
            convoluteImage img imgCpy matrix fdiv offset x y
        ) $ (,) <$> [0..(width-1)] <*> [0..(height-1)]

convoluteImage ::    GD.Image
                  -> GD.Image
                  -> [[Float]]
                  -> Float
                  -> Float
                  -> Int
                  -> Int
                  -> IO ()
convoluteImage img imgCpy matrix fdiv offset x y = do
    (nr,ng,nb,na) <- foldM (\(or,og,ob,oa) (k,j) -> do
            let
                yy = min (max (y-(1+j)) 0) (max (y-1) 0)
                xx = min (max (x-(1+k)) 0) (max (x-1) 0)
                mVal = matrix!!j!!k
            curr <- GD.getPixel (xx,yy) imgCpy
            let (r,g,b,a) = GD.toRGBA curr
            return ( or + fromIntegral r * mVal
                    ,og + fromIntegral g * mVal
                    ,ob + fromIntegral b * mVal
                    ,fromIntegral a)
        ) (0.0,0.0,0.0,0.0) $ (,) <$> [0..(length (matrix!!0) - 1)]
                                  <*> [0..(length matrix - 1)]
    let
        new_r = clamp 0 255 . truncate $ (nr/fdiv)+offset
        new_g = clamp 0 255 . truncate $ (ng/fdiv)+offset
        new_b = clamp 0 255 . truncate $ (nb/fdiv)+offset
    GD.setPixel (x,y) (GD.rgba new_r new_g new_b (truncate na)) img

gaussianBlur :: GD.Image -> IO ()
gaussianBlur img = convolute img [
  [1.0,2.0,1.0],
  [2.0,4.0,2.0],
  [1.0,2.0,1.0]
  ] 16 0

{-
gaussianBlur5x5 :: Image -> IO ()
gaussianBlur5x5 img = convolute img [
  [1.0,4.0 ,6.0 ,4.0 ,1.0],
  [4.0,16.0,24.0,16.0,4.0],
  [6.0,24.0,36.0,24.0,6.0],
  [4.0,16.0,24.0,16.0,4.0],
  [1.0,4.0 ,6.0 ,4.0 ,1.0]
  ] 256 0
-}


pixelTransform ::    GD.Image
                    -> ( RGBA -> RGBA ) -- ^ Transform function to be performed on each pixel
                    -> IO ()
pixelTransform img fx = do
    (width,height) <- GD.imageSize img
    mapM_ (\(x,y) -> do
            curr <- GD.getPixel (x,y) img
            let
                (r,g,b,a) = GD.toRGBA curr
                (nr,ng,nb,na) = fx (r,g,b,a)
            GD.setPixel (x,y) (GD.rgba nr ng nb na) img
        ) $ (,) <$> [0..(width-1)] <*> [0..(height-1)]


grayscale :: GD.Image -> IO ()
grayscale img =
    pixelTransform img (\(r,g,b,a) -> let
            newcol = truncate $ 0.299 * fromIntegral r + 0.587 * fromIntegral g + 0.114 * fromIntegral b
        in (newcol, newcol, newcol, a))

resizeTimes :: Int -> GD.Image -> IO GD.Image
resizeTimes n img = do
  size <- GD.imageSize img
  let sizeX = (fst size) `div` n
  let sizeY = (snd size) `div` n
  GD.resizeImage sizeX sizeY img



gdIO = do
  img <- GD.loadPngFile "test.png"
  res <- resizeTimes 3 img
  grayscale res
  gaussianBlur res
  GD.savePngFile "out.png" res
  putStrLn "hello world"




houghIO :: FilePath -> FilePath -> Int -> Int -> IO ()
houghIO path outpath thetaSize distSize = do
  image <- readImage path
  case image of
    Left err                  -> putStrLn err
    Right (ImageRGB8 image')  -> doImage image'
    Right (ImageRGBA8 image') -> doImage $ pixelMap dropTransparency image'
    _                         -> putStrLn $ "Expecting RGB8 or RGBA8 image"
  where
    doImage image = do
      let houghImage = hough image thetaSize distSize
      savePngImage outpath $ ImageRGB8 houghImage

main :: IO ()
main = do
  let path = "test.png"
  let outpath = "out.png"
  houghIO path outpath 800 400
