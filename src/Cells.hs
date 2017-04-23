module Cells(findCellsIO) where

import Codec.Picture
import Codec.Picture.Types
import Control.Monad.ST
import Control.Monad.Primitive
import Data.List


findCellsIO filename = do
  img' <- readBitmap filename
  let img = case img' of
        Right (ImageRGB8 i) -> i
        _ -> error "ERROR"
  let ps = helper img
  putStrLn $ "Found " ++ show (length ps) ++ " cells"

helper img = runST $ do
    i <- thawImage img
    let a = imageWidth img - 2
    let b = imageHeight img - 2
    return =<< helperLoop 1 (a,b) (0,0) [] i

-- a b
-- c d

helperLoop:: (PrimMonad m) =>
            Int
         -> (Int, Int)
         -> (Int, Int)
         -> [(Int,Int)]
         -> MutableImage (PrimState m) PixelRGB8
         -> m [(Int,Int)]
helperLoop min_x max_cs@(max_x, max_y) cs@(x,y) acc img = do

  a <- readPixel img x        y
  b <- readPixel img (x + 1)  y
  c <- readPixel img x        (y + 1)
  d <- readPixel img (x + 1)  (y + 1)
  let black = (==) (PixelRGB8 0 0 0)
  let hit = all black [a,b,c] && (not . black) d

  let acc' = if hit then cs:acc else acc;

  if cs == max_cs then return acc'
  else if x == max_x
    then helperLoop min_x max_cs (min_x, y + 1) acc' img
    else helperLoop min_x max_cs (x+1, y) acc' img
