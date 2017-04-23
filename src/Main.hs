module Main where

--import Circle (saveCircleIO, findCircleIO)
--import Square (saveSquareIO, findSquareIO, findPointsByDiagonal)
--import Dots   (saveDotsIO  , findDotsIO,  findDotsBmpIO)
import Lines (findLinesIO)
import Cells (findCellsIO)


main = do
  --findLinesIO "lines/2_000_o.bmp"
  findCellsIO "cells/3_030_o.bmp"
  putStrLn "DONE"
