module Main where

import Circle (saveCircleIO, findCircleIO)
import Square (saveSquareIO, findSquareIO, findPointsByDiagonal)
import Dots   (saveDotsIO  , findDotsIO,  findDotsBmpIO)

import Bresenham (bresenhamLine, bresenhamSquare, bresenhamDots)
import Codec.Picture

import qualified Data.Vector.Storable as V
import GHC.Word(Word8)

main1 = saveCircleIO "lol2.png" (700,300) 150

square1 = ((200, 600), (600, 600), (600,200), (200,200))

romb1 = ((200, 400), (400, 200), (200,0),  (0,200))
romb2 = ((100, 100), (575, 175), (500,650),(25,575))
romb3 = ((100, 100), (325, 25),  (400,250),(175,325))

main = do
  let ps = [(x,y) | x <- [100, 120 .. 900], y <- [100, 200 .. 900]]
  putStrLn $ "Made " ++ (show $ length ps) ++ " dots"
  saveDotsIO "dots.png" ps

main2 = do
  findDotsIO "dots.png"
  putStrLn "done"
