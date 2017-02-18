module Utils where


import Data.List
import Data.Array

mkPseudoImage size =
  let ps = generateCirclePoints (size `quot` 2, size `quot`2) (size `quot` 4)
  in  listArray
    ((0,0),(size-1,size-1))
    [if (x,y) `elem` ps then 255 else 0 | x <- [0..size-1], y <- [0..size-1]]


type Point = (Int, Int)

-- Takes the center of the circle and radius, and returns the circle points
generateCirclePoints :: Point -> Int -> [Point]
generateCirclePoints (x0, y0) radius
  = (x0, y0 + radius) : (x0, y0 - radius) : (x0 + radius, y0) : (x0 - radius, y0) : points
    where
      points = concatMap generatePoints $ unfoldr step initialValues
      generatePoints (x, y)
        = [(xop x0 x', yop y0 y') | (x', y') <- [(x, y), (y, x)], xop <- [(+), (-)], yop <- [(+), (-)]]
      initialValues = (1 - radius, 1, (-2) * radius, 0, radius)
      step (f, ddf_x, ddf_y, x, y) | x >= y = Nothing
                                   | otherwise = Just ((x', y'), (f', ddf_x', ddf_y', x', y'))
                                     where
                                       (f', ddf_y', y') | f >= 0 = (f + ddf_y' + ddf_x', ddf_y + 2, y - 1)
                                                        | otherwise = (f + ddf_x, ddf_y, y)
                                       ddf_x' = ddf_x + 2
                                       x' = x + 1
