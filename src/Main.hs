module Main where

import Graphics.GD
import Control.Monad (mapM_,foldM)
import Control.Applicative ((<$>),(<*>))


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




convolute ::    Image
                -> [[Float]]    -- ^ Convolution matrix
                -> Float        -- ^ Divisor
                -> Float        -- ^ Offset
                -> IO ()
convolute img matrix fdiv offset = do
    (width,height) <- imageSize img
    imgCpy <- copyImage img
    mapM_ (\(x,y) ->
            convoluteImage img imgCpy matrix fdiv offset x y
        ) $ (,) <$> [0..(width-1)] <*> [0..(height-1)]

convoluteImage ::    Image
                  -> Image
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
            curr <- getPixel (xx,yy) imgCpy
            let (r,g,b,a) = toRGBA curr
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
    setPixel (x,y) (rgba new_r new_g new_b (truncate na)) img

gaussianBlur :: Image -> IO ()
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

resizeTimes :: Int -> Image -> IO Image
resizeTimes n img = do
  size <- imageSize img
  let sizeX = (fst size) `div` n
  let sizeY = (snd size) `div` n
  resizeImage sizeX sizeY img


main :: IO ()
main = do
  img <- loadPngFile "test.png"
  res <- resizeTimes 3 img
  gaussianBlur res
  savePngFile "out.png" res
  putStrLn "hello world"
