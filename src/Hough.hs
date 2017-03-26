module Hough where

  hough :: Image PixelRGB8 -> Int -> Int -> ((Int,Int),Int)
  hough image thetaSize distSize = maxAcc
    where
      width = imageWidth image
      height = imageHeight image
      wMax = width - 1
      hMax = height - 1
      xCenter = wMax `div` 2
      yCenter = hMax `div` 2

      lumaMap = extractLumaPlane image

      gradient x y =
        let orig = pixelAt lumaMap x y
            x' = pixelAt lumaMap (min (x + 1) wMax) y
            y' = pixelAt lumaMap x (min (y + 1) hMax)
        in fromIntegralP (orig - x', orig - y')

      gradMap = [((x, y), gradient x y) | x <- [0..wMax], y <- [0..hMax]]

      -- The longest distance from the center, half the hypotenuse of the image.
      distMax :: Double
      distMax = (sqrt . fromIntegral $ height ^ 2 + width ^ 2) / 2

      {-
        The accumulation bins of the polar values.
        For each value in the gradient image, if the gradient length exceeds
        some threshold, consider it evidence of a line and plot all of the
        lines that go through that point in Hough space.
      -}
      accBin = runSTArray $ do
        arr <- newArray ((0, 0), (thetaSize, distSize)) 0 ::ST s (STArray s (Int,Int) Int)
        forM_ gradMap $ \((x, y), grad) -> do
          let (x', y') = fromIntegralP $ (xCenter, yCenter) `sub` (x, y)

          when (mag grad > 127) $
            forM_ [0..thetaSize] $ \theta -> do
              let theta' = (fromIntegral theta) * 360 / (fromIntegral thetaSize)
                           / 180 * pi :: Double
                  dist = (cos theta' * x' + sin theta' * y')
                  dist' = truncate $ dist * (fromIntegral distSize) / distMax
                  idx = (theta, dist')

              when (dist' >= 0 && dist' < distSize) $ do
                old <- readArray arr idx
                writeArray arr idx $ old + 1

        return arr

      maxAcc = F.maximumBy (\((i1,j1),m1) ((i2,j2),m2) -> compare m1 m2) $ assocs accBin



  mag :: Floating a => (a, a) -> a
  mag a = sqrt $ dot a a
    where dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

  sub :: Num a => (a, a) -> (a, a) -> (a, a)
  sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

  fromIntegralP :: (Integral a, Num b) => (a, a) -> (b, b)
  fromIntegralP (x, y) = (fromIntegral x, fromIntegral y)


  --------------------------------------------------------------------------------


  toPolar :: (Fractional a, Floating a, RealFrac a)
          => a -> a -> a -> a -> (Int,Int)
  toPolar x y r t = (truncate a,truncate b)
    where a = x - r * cos(t * pi / 180)
          b = y - r * sin(t * pi / 180)

  findCircle1 :: Array (Int, Int) Int -> Int -> Int -> ((Int,Int,Int), Int)
  findCircle1 img size rad = maximumBy (\(_, m1) (_, m2) -> compare m1 m2)
    $ assocs $ runSTArray $ do
      acc <- newArray ((0,0,0), (size,size,rad)) 0
      forM_ [(x,y) | x <- [0..size-1], y <- [0..size-1]] $ \(x,y) -> do
        let mag = img ! (x,y)
        when (mag == 255) $ do
          forM_ [(r,t) | r <- [0..rad-1], t <- [0..360]] $ \(r,t) -> do
            let (a,b) = toPolar (toEnum x) (toEnum y) (toEnum r) (toEnum t)
            let m = img ! (a,b)
            when (m == 255) $ do
              old <- readArray acc (a,b,r)
              writeArray acc (a,b,r) $ old + 1
      return acc
    where
