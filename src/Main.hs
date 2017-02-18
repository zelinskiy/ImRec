{-# LANGUAGE FlexibleContexts, BangPatterns #-}

module Main where

<<<<<<< HEAD
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

{-
main1 :: IO ()
main1 = do
    let n = 90.0
    let f1 = "lol.png"
    let f2 = "lol2.png"

    inp' <- PR.readImageRGBA f1
    case inp' of
      Left err -> putStrLn err
      Right img -> do
        rotated <- (computeP $ rotate n (imgData img) :: IO (Array F DIM3 Word8))
        savePngImage f2 (imgToImage rotated)
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

    bres pic f ddF_x ddF_y x y x0 y0 radius lineColor

bres pic _f _ddF_x _ddF_y _x _y x0 y0 radius lineColor = do
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

    bres pic _f _ddF_x _ddF_y _x _y x0 y0 radius lineColor







originalFnc :: Int -> Int -> Word8
originalFnc x y = if (x,y) `elem` ps then 255 else 0
  where ps = generateCirclePoints (600,600) 100

type Point = (Int, Int)

-- Takes the center of the circle and radius, and returns the circle points
generateCirclePoints :: Point -> Int -> [Point]
generateCirclePoints (x0, y0) radius
  -- Four initial points, plus the generated points
  = (x0, y0 + radius) : (x0, y0 - radius) : (x0 + radius, y0) : (x0 - radius, y0) : points
    where
      -- Creates the (x, y) octet offsets, then maps them to absolute points in all octets.
      points = concatMap generatePoints $ unfoldr step initialValues
      generatePoints (x, y)
        = [(xop x0 x', yop y0 y') | (x', y') <- [(x, y), (y, x)], xop <- [(+), (-)], yop <- [(+), (-)]]

      -- The initial values for the loop
      initialValues = (1 - radius, 1, (-2) * radius, 0, radius)

      -- One step of the loop. The loop itself stops at Nothing.
      step (f, ddf_x, ddf_y, x, y) | x >= y = Nothing
                                   | otherwise = Just ((x', y'), (f', ddf_x', ddf_y', x', y'))
                                     where
                                       (f', ddf_y', y') | f >= 0 = (f + ddf_y' + ddf_x', ddf_y + 2, y - 1)
                                                        | otherwise = (f + ddf_x, ddf_y, y)
                                       ddf_x' = ddf_x + 2
                                       x' = x + 1


-- <<rotate
rotate :: Double -> Array D DIM3 Word8 -> Array D DIM3 Word8
rotate deg g = fromFunction (Z :. y :. x :. k) f      -- <1>
    where
        sh@(Z :. y :. x :. k)   = extent g

        !theta = pi/180 * deg                         -- <2>

        !st = sin theta                               -- <3>
        !ct = cos theta

        !cy = fromIntegral y / 2 :: Double            -- <4>
        !cx = fromIntegral x / 2 :: Double

        f (Z :. i :. j :. k)                          -- <5>
          | inShape sh old = g ! old                  -- <6>
          | otherwise      = 0                        -- <7>
          where
            fi = fromIntegral i - cy                  -- <8>
            fj = fromIntegral j - cx

            i' = round (st * fj + ct * fi + cy)       -- <9>
            j' = round (ct * fj - st * fi + cx)

            old = Z :. i' :. j' :. k                  -- <10>
=======
import HoughRosetta
import Experimental




main :: IO ()
main = do
  --gdIO
  houghIO2 "test0.png" "out2.png" 200 200
>>>>>>> 74904096b444e1157979c9aba3227391ff13090b
