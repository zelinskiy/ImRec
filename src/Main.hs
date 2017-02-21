{-# LANGUAGE FlexibleContexts, BangPatterns #-}

module Main where

import System.Environment
import Codec.Picture
import Data.List
import Control.Monad.ST
import Codec.Picture.Types
import Control.Monad
import Data.Array.ST
import Data.Array
import qualified Data.Foldable as F (maximumBy)
import Data.Maybe
import System.Random (randomRIO)
import Control.Parallel.Strategies

import Bresenham
import Utils


{-
TODO:
Paralelize via repa
Many circles on single MutableImage
Circle detection
-}





tests = do
  --[ntests, width, height, minrad, maxrad] <- (read <$>) <$> getArgs
  --[ntests] <- (read <$>) <$> getArgs

  let ntests = 100

  let width = 1000
      height = 1000
      maxrad = 300
      minrad = 10

  let gen a b n = sequence $ replicate n $ randomRIO (a,b::Int)
  cases <- zipWith3 (\a b c -> ((a,b),c))
          <$> gen (maxrad + 1)   (width - maxrad - 1) ntests
          <*> gen (maxrad + 1)   (height - maxrad - 1) ntests
          <*> gen minrad maxrad ntests

  let bres = uncurry $ bresenham width height (255 :: Pixel8) (0:: Pixel8)
  let check = findCenter . fromJust . bres

  let x = map check cases `using` parList rdeepseq


  print $ x

mkRandomImage = do
  x <- randomRIO (10,999)
  y <- randomRIO (10,999)
  r <- randomRIO (20,999)
  print $ (x,y,r)
  if checkBounds 999 999 r (x,y)
  then saveCircleIO "lol2.png" (x,y) r
  else mkRandomImage



main1 = saveCircleIO "lol2.png" (500,500) 150

findCircleIO imgname = do
  img' <- readPng imgname
  let img = case img' of
        Right (ImageY8 i) -> i
        Left err -> error $ "ALARM "++err
        Right (ImageRGB16 i) -> error $ "Incorrect format RGB16"
        Right (ImageRGBA8 i) -> error $ "Incorrect format RGB8"
        Right (ImageRGBA16 i) -> error $ "Incorrect format RGBA16"
        Right (ImageRGBF i) -> error $ "Incorrect format RGBF"

        Right (ImageYCbCr8 i) -> error $ "Incorrect format Jpeg"
        Right (ImageRGB8 i) -> error $ "Incorrect format RGB8"
        Right (ImageY16 i) -> error $ "Incorrect format Y16"
        _ -> error $ "Incorrect format"
  print $ findCenter img

main = do
  img' <- readPng "lol2.png"
  let img = case img' of
        Right (ImageY8 i) -> i
        Left err -> error $ "ALARM "++err
        Right (ImageRGB16 i) -> error $ "Incorrect format RGB16"
        Right (ImageRGBA8 i) -> error $ "Incorrect format RGB8"
        Right (ImageRGBA16 i) -> error $ "Incorrect format RGBA16"
        Right (ImageRGBF i) -> error $ "Incorrect format RGBF"

        Right (ImageYCbCr8 i) -> error $ "Incorrect format Jpeg"
        Right (ImageRGB8 i) -> error $ "Incorrect format RGB8"
        Right (ImageY16 i) -> error $ "Incorrect format Y16"
        _ -> error $ "Incorrect format"
  --print $ hough img 360 50
  --tests

  putStrLn "done"

--------------------------------------------------------------------------------


findCenter :: Image Pixel8 -> ((Int,Int), Int)
findCenter img = (center, radius)
  where predicate = (==) 0
        left =  findLeftCorner img predicate
        right =  findRightCorner img predicate
        ((x1,y1),(x2,y2)) = runEval $ evalTuple2 rpar rpar (left,right)
        (dx,dy) = (x2 - x1, y2 - y1)
        center = (x1 + dx `div` 2, y1 + dy `div` 2)
        radius = dy `div` 2

findLeftCorner  :: (Pixel px) => Image px -> (px -> Bool) -> (Int,Int)
findLeftCorner pic predicate = step (0,0)
  where mx = (imageWidth pic) - 1
        my = (imageHeight pic) - 1
        step acc@(x,y) =
          if predicate (pixelAt pic x y) then acc
          else if acc == (mx,my) then (-1,-1)
          else if mx == x then step (0, y+1)
          else step (x+1, y)

findRightCorner  :: (Pixel px) => Image px -> (px -> Bool) -> (Int,Int)
findRightCorner pic predicate = step (mx,my)
  where mx = (imageWidth pic) - 1
        my = (imageHeight pic) - 1
        step acc@(x,y)  | predicate (pixelAt pic x y) = acc
                        | acc == (0,0)                = (-1,-1)
                        | x == 0                      = step (mx, y-1)
                        | otherwise                   = step (x-1, y)


--------------------------------------------------------------------------------


hough :: Image PixelRGB8 -> Int -> Int -> ((Int,Int),Int)
hough image thetaSize distSize = maxAcc
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
      arr <- newArray ((0, 0), (thetaSize, distSize)) 0 ::ST s (STArray s (Int,Int) Int)
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

    maxAcc = F.maximumBy (\((i1,j1),m1) ((i2,j2),m2) -> compare m1 m2) $ assocs accBin



mag :: Floating a => (a, a) -> a
mag a = sqrt $ dot a a
  where dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

sub :: Num a => (a, a) -> (a, a) -> (a, a)
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

fromIntegralP :: (Integral a, Num b) => (a, a) -> (b, b)
fromIntegralP (x, y) = (fromIntegral x, fromIntegral y)


--------------------------------------------------------------------------------


toPolar :: (Fractional a, Floating a, RealFrac a)
        => a -> a -> a -> a -> (Int,Int)
toPolar x y r t = (truncate a,truncate b)
  where a = x - r * cos(t * pi / 180)
        b = y - r * sin(t * pi / 180)

findCircle1 :: Array (Int, Int) Int -> Int -> Int -> ((Int,Int,Int), Int)
findCircle1 img size rad = maximumBy (\(_, m1) (_, m2) -> compare m1 m2)
  $ assocs $ runSTArray $ do
    acc <- newArray ((0,0,0), (size,size,rad)) 0
    forM_ [(x,y) | x <- [0..size-1], y <- [0..size-1]] $ \(x,y) -> do
      let mag = img ! (x,y)
      when (mag == 255) $ do
        forM_ [(r,t) | r <- [0..rad-1], t <- [0..360]] $ \(r,t) -> do
          let (a,b) = toPolar (toEnum x) (toEnum y) (toEnum r) (toEnum t)
          let m = img ! (a,b)
          when (m == 255) $ do
            old <- readArray acc (a,b,r)
            writeArray acc (a,b,r) $ old + 1
    return acc
  where




--------------------------------------------------------------------------------


saveCircleIO :: String -> (Int,Int) -> Int -> IO ()
saveCircleIO filename cent rad = do
  let back = 255::Pixel8
      fill = 0 :: Pixel8
      width = 1000
      height = 1000
  putStrLn $ "making " ++ filename
  let res = ImageY8 <$> bresenham width height back fill cent rad
  case res of
    Just r -> savePngImage filename r
    Nothing -> putStrLn "out of bounds"
  putStrLn "done"
