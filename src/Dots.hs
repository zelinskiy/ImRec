module Dots where

import Codec.Picture
import qualified Data.Vector.Storable as V
import Bresenham (bresenhamDots)

saveDotsIO :: FilePath -> [(Int, Int)] -> IO ()
saveDotsIO filepath ps=  do
  let back = 255::Pixel8
      fill = 0 :: Pixel8
      width = 1000
      height = 1000
  let res = ImageY8 <$> bresenhamDots back fill width height ps
  case res of
    Just r -> savePngImage filepath r
    Nothing -> putStrLn "FAIL"

findDotsIO :: FilePath -> IO ()
findDotsIO filepath = do
  img' <- readPng filepath
  let img = case img' of
        Right (ImageY8 i) -> imageData i
        Right (ImageRGB8 i) -> error "ERROR ImageRGB8"
        _ -> error "ERROR"
  let folder a p = if p >= (0::Pixel8) then a+1 else a
  let res = V.foldl folder 0 img
  putStrLn $ "Found " ++ show res ++" dots."

findDotsBmpIO :: FilePath -> IO ()
findDotsBmpIO filepath = do
  putStrLn $ "testing " ++ filepath
  img' <- readBitmap filepath
  let img = case img' of
        Right (ImageY8 i) -> error "ERROR 1"
        Right (ImageRGB8 i) -> imageData i
        Right (ImageRGBA8 i) -> error "ERROR 3"
        Left e -> error e
        _ -> error "ERROR 0"
  let folder a p = if p > 0 then a else a+1
  let res = V.foldl folder 0 img
  putStrLn $ "Found " ++ show res ++" dots."

testDots1 = sequence $ map findDotsBmpIO ["dots/1_0" ++ show i ++"_o.bmp" | i <- [10..30]]
