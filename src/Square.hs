module Square where

type Point2 = (Int, Int)

findPointsByDiagonal  :: Point2
                      -> Point2
                      -> (Point2, Point2, Point2, Point2)
findPointsByDiagonal pa@(x1, y1) pc@(x2, y2) = (pa, pb, pc, pd)
  where xc = quot (x1 + x2) 2
        yc = quot (y1 + y2) 2
        xd = quot (x1 - x2) 2
        yd = quot (y1 - y2) 2
        pb = (xc - yd, yc + xd)
        pd = (xc + yd, yc - xd)
