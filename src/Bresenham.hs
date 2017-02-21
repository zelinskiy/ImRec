module Bresenham (
      bresenham
    , checkBounds
) where

import Codec.Picture
import Control.Monad.ST
import Codec.Picture.Types
import Data.STRef
import Control.Monad


--rewrite this to pure logic
checkBounds mW mH rad (x0,y0)
  | (x0 - rad, y0 - rad) < (0,0)    = False
  | (x0 + rad, y0 - rad) > (mW,mH)  = False
  | otherwise                       = True

bresenham :: (Pixel px) -- ^ Pixel type , e.g. Pixel8
          => Int        -- ^ mwidth
          -> Int        -- ^ mheight
          -> px         -- ^ background pixel
          -> px         -- ^ Line pixel
          -> (Int,Int)  -- ^ Center
          -> Int        -- ^ Radius
          -> Maybe (Image px)
bresenham mwidth mheight bgPixel lineColor (x0,y0) radius =
  if not $ checkBounds mwidth mheight radius (x0,y0)
  then Nothing
  else Just $ runST $ do
    pic   <- createMutableImage mwidth mheight bgPixel
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
