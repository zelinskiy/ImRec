module Square where

import Bresenham(bresenhamSquare)
import Codec.Picture
import Circle(findDiagonal)

type Point2 = (Int, Int)
type Square2 = (Point2, Point2, Point2, Point2)

findPointsByDiagonal  :: (Point2, Point2) -> Square2
findPointsByDiagonal (pa@(x1, y1), pc@(x2, y2)) = (pa, pb, pc, pd)
  where xc = quot (x1 + x2) 2
        yc = quot (y1 + y2) 2
        xd = quot (x1 - x2) 2
        yd = quot (y1 - y2) 2
        pb = (xc - yd, yc + xd)
        pd = (xc + yd, yc - xd)


findSquare :: Image Pixel8 -> Square2
findSquare = findPointsByDiagonal . findDiagonal

saveSquareIO :: Square2 -> IO ()
saveSquareIO (pa, pb, pc, pd) = do
  let back = 255::Pixel8
      fill = 0 :: Pixel8
      width = 1000
      height = 1000
      filename = "sq.png"
  putStrLn $ "making " ++ filename
  let res = ImageY8 <$> bresenhamSquare back fill width height pa pb pc pd
  case res of
    Just r -> savePngImage filename r
    Nothing -> putStrLn "FAIL"


findSquareIO imgname = do
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
  print $ findSquare img
