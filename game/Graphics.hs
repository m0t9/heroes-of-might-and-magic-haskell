module Graphics where
hexToCoords:: (Double, Double) -> (Int, Int) -> Double -> (Double, Double)
hexToCoords (xOffset, yOffset) (xHex, yHex) side =
  (xOffset + xShift, yOffset + yShift)
  where
    x = fromIntegral xHex
    y = fromIntegral yHex
    yShift = side * (1 + y)
    xShift
      | even yHex = (sqrt 3) * side * (1 + x) 
      | otherwise = (sqrt 3) * side * (1/2 + x)

offset = (1.6, 2.5)
hexSide = 2.4
currentConversion :: (Int, Int) -> (Double, Double)
currentConversion hexCoords = hexToCoords offset hexCoords hexSide  

makeHexagonDotSet :: (Int, Int) -> Double -> [(Double, Double)]
makeHexagonDotSet hexCoords side =
   [up, upRight, downRight, down, downLeft, upLeft]
     where
      (x, y) = currentConversion hexCoords
      halfSide = side/2
      sqrtSide = side * (sqrt 3) /2
      up = (x, y+side)
      upRight = (x + sqrtSide, y + halfSide)
      downRight = (x + sqrtSide, y - halfSide)
      down = (x, y-side)
      downLeft = (x - sqrtSide, y - halfSide)
      upLeft = (x - sqrtSide, y + halfSide)