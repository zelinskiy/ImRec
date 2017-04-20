{-# LANGUAGE RankNTypes, KindSignatures #-}
module Lines (findLinesIO) where


import Codec.Picture
import Codec.Picture.Types
import Control.Monad.ST
import Control.Monad.Primitive
import Data.List

data Direction = Horiz | Vert deriving (Eq, Show)
data LPType = Begin | End deriving (Eq, Show)
data LinePoint = LinePoint LPType Direction (Int, Int) deriving (Eq)
data Line = Line LinePoint LinePoint deriving (Eq)


instance Show LinePoint where
  show (LinePoint Begin Horiz cs) = " ⊢ " ++ show cs
  show (LinePoint End   Horiz cs) = " ⊣ " ++ show cs
  show (LinePoint Begin Vert  cs) = " ⊤ " ++ show cs
  show (LinePoint End   Vert  cs) = " ⊥ " ++ show cs

instance Show Line where
  show (Line b e) = "{ " ++ show b ++ " # " ++ show e ++ " }"

instance Ord Line where
  compare (Line b1 e1) (Line b2 e2) = compare (magnitude b1 e1) (magnitude b2 e2)

magnitude p1 p2 = sqrt $ fromIntegral $ dx * dx + dy * dy
  where dx = xCoord p1 - xCoord p2
        dy = yCoord p1 - yCoord p2

xCoord (LinePoint _ _ (x,y)) = x
yCoord (LinePoint _ _ (x,y)) = y
pType (LinePoint t _ _) = t
pDir (LinePoint _ d _) = d

flipType Begin = End
flipType End = Begin

black = (==) (PixelRGB8 0 0 0)
nblack = not . black
(<#>) (f, g) (x, y) = (f x, g y)

findLinesIO filename = do
  img' <- readBitmap filename
  let img = case img' of
        Right (ImageRGB8 i) -> i
        _ -> error "ERROR"
  let ps = helper img
  let ls = matchSides [] ps
  putStrLn $ "Found " ++ show (length ls) ++ " lines"
  putStrLn $ "Longest: " ++ show (maximum ls)
  putStrLn $ "Shortest: " ++ show (minimum ls)

matchSides :: [Line] -> [LinePoint] -> [Line]
matchSides acc (p:ps) =
  let check (LinePoint t Horiz (_,y)) (LinePoint t' Horiz (_,y')) =
          y == y' && flipType t == t'
      check (LinePoint t Vert (x,_)) (LinePoint t' Vert (x',_)) =
          x == x' && flipType t == t'
      check _ _ = False
  in case find (check p) ps of
  Just p' -> matchSides ((Line p p'):acc) (delete p' ps)
  Nothing -> error "CANNOT MATCH SIDES"

matchSides acc [] = map correct acc
  where correct (Line a@(LinePoint End _ _) b@(LinePoint Begin _ _)) = Line b a
        correct l = l



helper img = runST $ do
    i <- thawImage img
    let a = imageWidth img - 2
    let b = imageHeight img - 2
    inner <- helperLoop 1 (a,b) (1,1) [] i
    outer <- helperLoopBorders (0,0) (a + 1 ,b + 1) (0,0) [] ((+1), id) i
    return $ outer ++ inner



helperLoopBorders :: (PrimMonad m) =>
                      (Int, Int)
                   -> (Int, Int)
                   -> (Int, Int)
                   -> [LinePoint]
                   -> ((Int -> Int), (Int -> Int))
                   -> MutableImage (PrimState m) PixelRGB8
                   -> m [LinePoint]
helperLoopBorders min_cs@(min_x, min_y) max_cs@(max_x, max_y) cs@(x,y) acc inc img = do
  p <- readPixel img x y
  let b = black p

  let acc'  | b && x == min_x = (LinePoint Begin Horiz cs):acc
            | b && x == max_x = (LinePoint End   Horiz cs):acc
            | b && y == min_y = (LinePoint Begin Vert  cs):acc
            | b && y == max_y = (LinePoint End   Vert  cs):acc
            | otherwise = acc

  let inc'  | y == min_y && x == max_x  = (id, (+1))
            | y == max_y && x == max_x = (subtract 1, id)
            | y == max_y && x == min_x = (id, subtract 1)
            | otherwise = inc

  let cs' = inc' <#> cs

  if cs' == min_cs
  then return acc'
  else helperLoopBorders min_cs max_cs cs' acc' inc' img


-- TL TT TR
-- LL CC RR
-- BL BB BR

helperLoop:: (PrimMonad m) =>
            Int
         -> (Int, Int)
         -> (Int, Int)
         -> [LinePoint]
         -> MutableImage (PrimState m) PixelRGB8
         -> m [LinePoint]
helperLoop min_x max_cs@(max_x, max_y) cs@(x,y) acc img = do

  tl <- readPixel img (x - 1) (y - 1)
  tt <- readPixel img x       (y - 1)
  tr <- readPixel img (x + 1) (y - 1)

  ll <- readPixel img (x - 1) y
  cc <- readPixel img x       y
  rr <- readPixel img (x + 1) y

  bl <- readPixel img (x - 1) (y + 1)
  bb <- readPixel img x       (y + 1)
  br <- readPixel img (x + 1) (y + 1)

  let isHitBH = all black [cc, rr] && all nblack [tl, tt, tr, ll, bl, bb, br]
  let isHitBV = all black [cc, bb] && all nblack [tl, tt, tr, ll, rr, bl, br]

  let isHitEH = all black [cc, ll] && all nblack [tl, tt, tr, rr, bl, bb, br]
  let isHitEV = all black [cc, tt] && all nblack [tl, tr, ll, rr, bl, bb, br]

  let acc'  | isHitBH = (LinePoint Begin  Horiz cs):acc
            | isHitBV = (LinePoint Begin  Vert  cs):acc
            | isHitEH = (LinePoint End    Horiz cs):acc
            | isHitEV = (LinePoint End    Vert  cs):acc
            | otherwise = acc

  if cs == max_cs then return acc'
  else if x == max_x
    then helperLoop min_x max_cs (min_x, y + 1) acc' img
    else helperLoop min_x max_cs (x+1, y) acc' img
