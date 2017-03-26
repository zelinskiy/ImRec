module Main where

import Circle (saveCircleIO, findCircleIO)
import Square (saveSquareIO, findSquareIO, findPointsByDiagonal)

import Bresenham (bresenhamLine, bresenhamSquare)
import Codec.Picture

main1 = saveCircleIO "lol2.png" (700,300) 150

square1 = ((200, 600), (600, 600), (600,200), (200,200))

romb1 = ((200, 400), (400, 200), (200,0), (0,200))
romb2 = ((100,100),(575,175),(500,650),(25,575))
romb3 = ((100,100),(325,25),(400,250),(175,325))

main = do
  --print $ findPointsByDiagonal ((100,100), (400,250))
  saveSquareIO "romb1.png" romb1
  saveSquareIO "romb2.png" romb2
  saveSquareIO "romb3.png" romb3
  --findSquareIO "sqare1.png"
  putStrLn "done"
