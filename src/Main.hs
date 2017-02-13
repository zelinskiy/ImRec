module Main where

import qualified Graphics.GD as GD
import Control.Monad (mapM_,foldM)
import Control.Applicative ((<$>),(<*>))

import HoughRosetta



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

edgeConvolution8 :: GD.Image -> IO ()
edgeConvolution8 img = convolute img [
  [-1.0,-1.0,-1.0],
  [-1.0, 8.0,-1.0],
  [-1.0,-1.0,-1.0]
  ] 1 0

edgeConvolution4 :: GD.Image -> IO ()
edgeConvolution4 img = convolute img [
  [0.0,1.0,0.0],
  [1.0,-4.0,1.0],
  [0.0,1.0,0.0]
  ] 1 0

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
  img <- GD.loadPngFile "test2.png"
  res <- resizeTimes 2 img
  --grayscale res
  edgeConvolution4 res
  GD.savePngFile "out.png" res
  putStrLn "hello world"

main :: IO ()
main = do
  --gdIO
  houghIO2 "test0.png" "out.png" 200 200
