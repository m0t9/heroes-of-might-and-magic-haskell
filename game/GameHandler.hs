module GameHandler where
import Graphics
isInHex :: (Double, Double) -> (Double, Double) -> Double -> Bool
isInHex (x, y) (xHexCenter, yHexCenter) side
  | (((x-xHexCenter)*(x-xHexCenter)) + ((y-yHexCenter) * (y-yHexCenter))) > (side*side) = False
  | otherwise                                                                           = True


coordsToHexWithDomain 
  :: (Double, Double) 
  -> (Double, Double) 
  -> Double 
  -> (Int, Int) 
  -> Maybe (Int, Int)
coordsToHexWithDomain offsetSize coords side (xMaxHex, yMaxHex) 
  = case coordsToHex offsetSize coords side of
      Nothing -> Nothing
      Just (x, y) -> result
        where
          result
            | (x > xMaxHex) || (y > yMaxHex) = Nothing
            | otherwise                      = Just (x, y)  


coordsToHex 
  :: (Double, Double) 
  -> (Double, Double) 
  -> Double 
  -> Maybe (Int, Int)
coordsToHex (xOffset, yOffset) (x, y) side
  | (xCoord < xOffset) || (yCoord < yOffset) = Nothing
  | otherwise                  = result
  where
    sqrtSide = side * (sqrt 3) /2
    xCoord = x-xOffset
    yCoord = y-yOffset
    x1 = floor (xCoord / (sqrtSide*2))
    x2 = floor ((xCoord+sqrtSide) / (sqrtSide*2))
    y1 = floor (yCoord / side)
    y2 = floor ((yCoord + (side / 2)) / side)
    isDotInHexagon :: (Int, Int) -> Bool
    isDotInHexagon (xHex, yHex) = isInHex (x, y) centerCoords side
      where
        centerCoords = hexToCoords offset (xHex, yHex) side
    result 
      | isDotInHexagon (x1, y1) = Just (x1, y1)
      | isDotInHexagon (x2, y1) = Just (x2, y1)
      | isDotInHexagon (x1, y2) = Just (x1, y2)
      | isDotInHexagon (x2, y2) = Just (x2, y2)
      | otherwise               = Nothing