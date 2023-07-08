module GameHandler where
import Graphics
isInHex :: (Double, Double) -> (Double, Double) -> Double -> Bool
isInHex (x, y) (xHexCenter, yHexCenter) side
  | (((x-xHexCenter)*(x-xHexCenter)) + ((y-yHexCenter) * (y-yHexCenter))) > (side*side) = False
  | otherwise                                                                           = True


coordsToHex :: (Double, Double) -> (Double, Double) -> Double -> Maybe (Int, Int)
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
    getCenter :: (Int, Int) -> (Double, Double)
    getCenter (xCrd, yCrd) = hexToCoords offset (xCrd, yCrd) side
    center1 = getCenter (x1, y1)
    center2 = getCenter (x2, y1)
    center3 = getCenter (x1, y2)
    center4 = getCenter (x2, y2)
    checkAttachment :: (Double, Double) -> Bool
    checkAttachment centerCoords = isInHex (x, y) centerCoords side 
    result 
      | checkAttachment center1 = Just (x1, y1)
      | checkAttachment center2 = Just (x2, y1)
      | checkAttachment center3 = Just (x1, y2)
      | checkAttachment center4 = Just (x2, y2)
      | otherwise               = Nothing