module GameHandler where
import Graphics
import Graphics.Gloss
import GameInternal

isInHex :: DoubleCoords -> DoubleCoords -> Double -> Bool
isInHex (x, y) (xHexCenter, yHexCenter) side
  | (((x-xHexCenter)*(x-xHexCenter)) + ((y-yHexCenter) * (y-yHexCenter))) > (side*side) = False
  | otherwise                                                                           = True


coordsToHexWithDomain
  :: Offset
  -> DoubleCoords
  -> Double
  -> FieldSize
  -> Maybe CellCoords
coordsToHexWithDomain offsetSize coords side (xMaxHex, yMaxHex)
  = case coordsToHex offsetSize coords side of
      Nothing -> Nothing
      Just (x, y) -> result
        where
          result
            | (x > xMaxHex) || (y > yMaxHex) = Nothing
            | otherwise                      = Just (x, y)


coordsToHex
  :: Offset
  -> DoubleCoords
  -> Double
  -> Maybe CellCoords
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
    isDotInHexagon :: CellCoords -> Bool
    isDotInHexagon (xHex, yHex) = isInHex (x, y) centerCoords side
      where
        centerCoords = hexToCoords offset (xHex, yHex) side
    result
      | isDotInHexagon (x1, y1) = Just (x1, y1)
      | isDotInHexagon (x2, y1) = Just (x2, y1)
      | isDotInHexagon (x1, y2) = Just (x1, y2)
      | isDotInHexagon (x2, y2) = Just (x2, y2)
      | otherwise               = Nothing


isAngleMoreThen30InHex :: DoubleCoords -> DoubleCoords -> Bool
isAngleMoreThen30InHex (xCenter, yCenter) (x, y)
  | cot > sqrt 3 = False
  | otherwise    = True
  where
    cot = leg1 / leg2
    leg1 = abs (x - xCenter)
    leg2 = abs (y - yCenter)


determineCellPart :: DoubleCoords -> DoubleCoords -> CellPart
determineCellPart (x, y) (xCenter, yCenter)
  | (x > xCenter) && (y > yCenter) && isInVerticalPart            = UR
  | (x < xCenter) && (y > yCenter) && isInVerticalPart            = UL
  | (x > xCenter) && (y < yCenter) && isInVerticalPart            = DR
  | (x < xCenter) && (y < yCenter) && isInVerticalPart            = DL
  | ((x > xCenter) && (y > yCenter) && (not isInVerticalPart))
    || ((x > xCenter) && (y < yCenter) && (not isInVerticalPart)) = R
  | otherwise                                                     = L
    where
        isInVerticalPart = isAngleMoreThen30InHex (x, y) (xCenter, yCenter)

getCellsToMove :: Unit -> GameState -> [CellCoords]
getCellsToMove ::
data State = NoSelected GameState | Selected GameState

gameHandler :: Event -> State -> State 
gameHandler _event (NoSelected gameState)    = NoSelectedHandler _event ()
gameHandler _event (Selected gameState unit) = selectedHandler