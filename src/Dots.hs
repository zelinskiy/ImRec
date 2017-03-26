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
        _ -> error "ERROR"
  let folder a p = if p == (0::Pixel8) then a+1 else a
  let res = V.foldl folder 0 img
  putStrLn $ "Found " ++ show res ++" dots."
