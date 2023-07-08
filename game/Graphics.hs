module Graphics where
import Graphics.Gloss
hexToCoords:: Offset -> CellCoords -> Double -> DoubleCoords
hexToCoords (xOffset, yOffset) (xHex, yHex) side =
  (xOffset + xShift, yOffset + yShift)
  where
    x = fromIntegral xHex
    y = fromIntegral yHex
    yShift = side * (1+ 1.5*y)--(1 + (2*y))
    xShift
      | even yHex = (sqrt 3) * side * (1 + x) 
      | otherwise = (sqrt 3) * side * (1/2 + x)

offset = (1.6, 2.5)
hexSide = 2.4
currentConversion :: CellCoords -> DoubleCoords
currentConversion hexCoords = hexToCoords offset hexCoords hexSide  

makeHexagonDotSet :: CellCoords -> Double -> [(Float, Float)]
makeHexagonDotSet hexCoords side =
   [up, upRight, downRight, down, downLeft, upLeft]
     where
      (x, y) = currentConversion hexCoords
      halfSide = side/2
      sqrtSide = side * (sqrt 3) /2
      up = (realToFrac x, realToFrac (y+side))
      upRight = (realToFrac (x + sqrtSide), realToFrac(y + halfSide))
      downRight = (realToFrac (x + sqrtSide), realToFrac(y - halfSide))
      down = (realToFrac x, realToFrac(y-side))
      downLeft = (realToFrac (x - sqrtSide), realToFrac(y - halfSide))
      upLeft = (realToFrac(x - sqrtSide), realToFrac(y + halfSide))

type FieldSize = (Int, Int)
type Offset = (Double, Double)
type CellCoords = (Int, Int)
type Field = Picture
type DoubleCoords = (Double, Double)

drawField :: CellCoords -> FieldSize -> Picture
drawField (x, y) (xF, yF) 
  | (x == xF && y == yF) = drawCell (x, y)
  | (x == xF) = drawField (0, y+1) (xF, yF) <> drawCell (x, y)
  | otherwise = drawField (x+1, y) (xF, yF) <> drawCell (x, y)

drawCell :: CellCoords -> Field
drawCell cellCoords = lineLoop (makeHexagonDotSet cellCoords hexSide)