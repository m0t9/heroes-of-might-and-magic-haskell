module GameHandler where
import GameInternal

import Graphics.Gloss.Interface.IO.Game
-- import Data.Ord (Down)
import GHC.Float (float2Double)
import Utils

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

isInHex :: DoubleCoords -> DoubleCoords -> Double -> Bool
isInHex (x, y) (xHexCenter, yHexCenter) side
  | (((x-xHexCenter)*(x-xHexCenter)) + ((y-yHexCenter) * (y-yHexCenter))) > (side*side) = False
  | otherwise                                                                           = True

coordsToHexHMM3 :: DoubleCoords -> Maybe CellCoords
coordsToHexHMM3 coords 
  = coordsToHexWithDomain offset coords hexSide (15, 11)  
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
getCellsToMove _u _gs = getAllCellsHH3
getAllCellsHH3 :: [CellCoords]
getAllCellsHH3 = getAllCells (0, 0) (15, 11)    
getAllCells :: CellCoords -> FieldSize -> [CellCoords]
getAllCells (x, y) (xF, yF)
  | (x == xF && y == yF) = [(x, y)]
  | (x == xF) = (x, y) : (getAllCells (0, y+1) (xF, yF))
  | otherwise = (x, y) : (getAllCells (x+1, y) (xF, yF)) 


data State = NoSelected GameState | Selected GameState Unit


gameHandler :: Event -> State -> State 
gameHandler _event (NoSelected gameState)    = noSelectedStateHandler _event (NoSelected gameState)
gameHandler _event (Selected gameState unit) = selectedStateHandler _event (Selected gameState unit)


getUnitCoords::Unit -> CellCoords
getUnitCoords (Unit _ (UnitState _ _ coords _ _ _)) = coords 


getCoordsOfUnits:: [Unit] -> [CellCoords]
getCoordsOfUnits = map getUnitCoords 


getFriendlyCoords :: Player -> [Unit] -> [CellCoords]
getFriendlyCoords player units = getCoordsOfUnits (filterFriendly player units)


getNeighbourCell :: CellCoords -> CellPart -> CellCoords
getNeighbourCell (x, y) R = (x+1, y)
getNeighbourCell (x, y) L = (x-1, y)
getNeighbourCell (x, y) dir
  | even y    = case dir of
    UR -> (x+1, y+1)
    UL -> (x, y+1)
    DR -> (x+1, y-1)
    DL -> (x, y-1)
  | otherwise = case dir of 
    UR -> (x, y+1)
    UL -> (x-1, y+1)
    DR -> (x, y-1)
    DL -> (x-1, y-1)

findUnit:: CellCoords -> [Unit] -> Maybe Unit
findUnit _ [] = Nothing
findUnit coords (unit:units)
  | coords == (getUnitCoords unit) = Just unit 
  | otherwise                      = findUnit coords units


selectFriendlyUnit :: GameState -> CellCoords -> Maybe Unit
selectFriendlyUnit gameState coords = findUnit coords friendlyUnits
  where 
    friendlyUnits = filterFriendly (turn gameState) (getUnits gameState) 


selectUnit :: State -> DoubleCoords -> State
selectUnit (NoSelected gameState) coords
  = case (coordsToHexHMM3 coords) of
    Nothing -> NoSelected gameState
    Just (x, y) -> case selectFriendlyUnit gameState (x, y) of
      Nothing -> NoSelected gameState
      Just unit -> Selected gameState unit
selectUnit _state _ = _state      


noSelectedStateHandler :: Event -> State -> State
noSelectedStateHandler (EventKey (MouseButton LeftButton) Down _ (x, y)) state = selectUnit state (float2Double x, float2Double y)
noSelectedStateHandler _e _st = _st


determineAction :: State -> DoubleCoords -> State
determineAction (Selected gameState unit) coords =
  case (coordsToHexHMM3 coords) of
    Nothing -> NoSelected gameState
    Just (x, y) -> NoSelected gameState 
determineAction _state _ = _state  

selectedStateHandler :: Event -> State -> State
selectedStateHandler (EventKey (SpecialKey KeyEsc) Down _ _) (Selected gameState _) = NoSelected gameState 
selectedStateHandler (EventKey (MouseButton LeftButton) Down _ (x, y)) state = determineAction state (float2Double x, float2Double y)
selectedStateHandler _e _st = _st
