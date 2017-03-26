module Circle where

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

import Bresenham(bresenham, checkBounds)

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

mkRandomCircle :: IO ()
mkRandomCircle = do
  x <- randomRIO (10,999)
  y <- randomRIO (10,999)
  r <- randomRIO (20,999)
  print $ (x,y,r)
  if checkBounds 999 999 r (x,y)
  then saveCircleIO "lol2.png" (x,y) r
  else mkRandomCircle

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

--------------------------------------------------------------------------------

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
