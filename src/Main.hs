module Main where

--import Circle (saveCircleIO, findCircleIO)
--import Square (saveSquareIO, findSquareIO, findPointsByDiagonal)
--import Dots   (saveDotsIO  , findDotsIO,  findDotsBmpIO)
import Lines (findLinesIO)

main = findLinesIO "lines/2_015_o.bmp"
