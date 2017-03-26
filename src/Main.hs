module Main where

import Circle (saveCircleIO, findCircleIO)
import Square (findPointsByDiagonal)

import Bresenham (bresenhamLine, bresenhamSquare)
import Codec.Picture

main1 = saveCircleIO "lol2.png" (700,300) 150

main = do
  let back = 255::Pixel8
      fill = 0 :: Pixel8
      width = 1000
      height = 1000
      pa = (200,400)
      pb = (400,200)
      pc = (200,0)
      pd = (0,200)
      filename = "sq.png"
  putStrLn $ "making " ++ filename
  let res = ImageY8 <$> bresenhamSquare back fill width height pa pb pc pd
  case res of
    Just r -> savePngImage filename r
    Nothing -> putStrLn "<out of bounds>"
  putStrLn "done"
