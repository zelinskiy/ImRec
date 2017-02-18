{-# LANGUAGE FlexibleContexts, BangPatterns #-}

module Main where

import Data.Array.Repa
import System.Environment
import Data.Array.Repa.Repr.ForeignPtr
import Data.Word
import Codec.Picture.Repa as PR
import Codec.Picture
import Data.List
import Control.Monad.ST
import Codec.Picture.Types
import Data.STRef
import Control.Monad

{-
TODO:
Many circles on single MutableImage
Circle detection
-}


main = do
  let back = 255::Pixel8
  let fill = 0 :: Pixel8
  let width = 1000
  let height = 1000
  let cent = (250,250)
  let rad = 200

  let initImg = generateImage (\x y -> back) width height
  let res = ImageY8 <$> bresenham initImg fill cent rad
  case res of
    Just r -> savePngImage "lol2.png" r
    Nothing -> putStrLn "out of bounds"
  putStrLn "1"


checkBounds mW mH rad (x0,y0)
  | (x0 - rad, y0 - rad) < (0,0)    = False
  | (x0 + rad, y0 - rad) > (mW,mH)  = False
  | otherwise                       = True


bresenham :: (Pixel px)
          => Image px
          -> px
          -> (Int,Int)
          -> Int
          -> Maybe (Image px)
bresenham mpic lineColor (x0,y0) radius =
  if not $ checkBounds (imageWidth mpic) (imageHeight mpic) radius (x0,y0)
  then Nothing
  else Just $ runST $ do
    pic   <- thawImage mpic
    f     <- newSTRef $ 1 - radius
    ddF_x <- newSTRef 1
    ddF_y <- newSTRef $ -2 * radius
    x     <- newSTRef 0
    y     <- newSTRef radius

    writePixel pic x0           (y0 + radius) lineColor
    writePixel pic x0           (y0 - radius) lineColor
    writePixel pic (x0 + radius) y0           lineColor
    writePixel pic (x0 - radius) y0           lineColor

    bresStep pic f ddF_x ddF_y x y x0 y0 radius lineColor

bresStep pic _f _ddF_x _ddF_y _x _y x0 y0 radius lineColor = do
  f     <- readSTRef _f
  ddF_x <- readSTRef _ddF_x
  ddF_y <- readSTRef _ddF_y
  x     <- readSTRef _x
  y     <- readSTRef _y

  if x > y then freezeImage pic
  else do
    when (f >= 0) (
      writeSTRef _y     (y - 1) >>
      writeSTRef _ddF_y (ddF_y + 2) >>
      writeSTRef _f     (f + ddF_y + 2) )

    writeSTRef _x     (x+1)
    writeSTRef _ddF_x (ddF_x + 2)
    f2 <- readSTRef _f
    writeSTRef _f (f2 + ddF_x + 2)

    x2 <- readSTRef _x
    y2 <- readSTRef _y

    writePixel pic (x0 + x2) (y0 + y2) lineColor
    writePixel pic (x0 - x2) (y0 + y2) lineColor
    writePixel pic (x0 + x2) (y0 - y2) lineColor
    writePixel pic (x0 - x2) (y0 - y2) lineColor

    writePixel pic (x0 + y2) (y0 + x2) lineColor
    writePixel pic (x0 - y2) (y0 + x2) lineColor
    writePixel pic (x0 + y2) (y0 - x2) lineColor
    writePixel pic (x0 - y2) (y0 - x2) lineColor

    bresStep pic _f _ddF_x _ddF_y _x _y x0 y0 radius lineColor
