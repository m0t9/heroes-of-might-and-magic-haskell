module GameHandler where
import Game

import Graphics.Gloss.Interface.IO.Game
-- import Data.Ord (Down)
import GHC.Float (float2Double)
import Utils
import Graph


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
            | (x > xMaxHex) || (y > yMaxHex) 
             || (x < 0) || (y < 0) = Nothing
            | otherwise            = Just (x, y)


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
    x2 = floor ((xCoord-(sqrtSide)) / (sqrtSide*2))
    y1 = (floor (yCoord / (3 * side))) * 2
    y2 = (floor ((yCoord - (3 * side / 2)) / (3* side))) *2 + 1
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

type Animation = Maybe [Coords]

data State = Selected GameState Unit | Moving GameState Unit Coords Animation


gameHandler :: Event -> State -> State 
--gameHandler _event (NoSelected gameState)    = noSelectedStateHandler _event (NoSelected gameState)
gameHandler _event (Selected gameState unit) = selectedStateHandler _event (Selected gameState unit)
gameHandler _event (Moving gameState unit crds animation) = (Moving gameState unit crds animation)


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

doesExistInList :: CellCoords -> [CellCoords] -> Bool
doesExistInList _ [] = False
doesExistInList req (crd:crds)
  | req == crd = True
  | otherwise = doesExistInList req crds

isMovable :: State -> CellCoords -> Bool
isMovable (Selected gameState unit) coords = doesExistInList coords (getCellsToMove unit gameState)

determineAction :: State -> DoubleCoords -> State
determineAction (Selected gameState unit) coords =
  case (coordsToHexHMM3 coords) of
    Nothing -> Selected gameState unit
    Just (x, y) -> if isMovable (Selected gameState unit) (x, y)
      then moveCharacter (Selected gameState unit) (x, y)
      else Selected gameState unit 
determineAction state _ = state

getFirstUnit :: [Unit] -> Unit
getFirstUnit (y:ys) = y

determineTheFirst :: [Unit] -> Player
determineTheFirst [] = Player RightPlayer
determineTheFirst (unit:units) = getPlayer
  where
    (Unit _unitType (UnitState _ getPlayer _ _ _ _ )) = unit

graph :: Graph
graph = generateGraph 15 11

moveCharacter :: State -> CellCoords -> State
moveCharacter (Selected gameState unit) crds = Moving gameState unit crds newAnimation
  where
    (GameState units (Player turn) queue) = gameState
    (Unit unitType unitProps) = unit
    newAnimation = getAnimationPath gameState unit crds
moveCharacter (Moving (GameState units (Player turn) queue) unit _crdsState animation) crds
  | (unitCoords == crds) = Selected (GameState units updatedPlayer queue) updatedSelected
  | otherwise = Moving (GameState updatedUnits (Player turn) updatedQueue) updatedUnit crds updatedAnimation
  where
    unitCoords = getUnitCoords unit
    updatedUnit = Unit unitType newPosition
    updatedPlayer = determineTheFirst updatedQueue
    newQueue = map (changeUnitProps unit newPosition) queue
    updatedQueue = moveUnitToQueueEnd updatedUnit newQueue
    updatedSelected = getFirstUnit updatedQueue
    (frame, updatedAnimation) = getFrame animation
    (Unit unitType unitProps) = unit
    newPosition = changeStateCoords unitProps frame
    updatedUnits = map (changeUnitProps unit newPosition) units

getAnimationPath :: GameState -> Unit -> Coords -> Maybe [Coords]
getAnimationPath _state (Unit Harpy props) coords = findPath graph (getUnitCoords (Unit Harpy props)) coords (const False)
getAnimationPath state unit coords = findPath graph (getUnitCoords unit) coords (isUnitObstacle state)

getFrame :: Animation -> (Coords, Animation)
getFrame frames = case frames of
  Nothing -> ((0, 0), Nothing)
  Just (fr:frms) -> (fr, Just frms)
  Just [] -> ((0, 0), Nothing)

--getTest :: [Int] -> (Int, [Int])
--getTest (x:xs) = (x, xs)

invertPlayer :: Player -> Player
invertPlayer (Player LeftPlayer) = Player RightPlayer
invertPlayer (Player RightPlayer) = Player LeftPlayer

moveUnitToQueueStart :: Unit -> [Unit] -> [Unit]
moveUnitToQueueStart _ [] = []
moveUnitToQueueStart unit units = unit : delete unit units

moveUnitToQueueEnd :: Unit -> [Unit] -> [Unit]
moveUnitToQueueEnd unit [] = [unit]
moveUnitToQueueEnd unit (ut:uts)
  | unit == ut = uts ++ [unit]
  | otherwise = ut : moveUnitToQueueEnd unit uts

changeUnitProps :: Unit -> UnitState -> Unit -> Unit
changeUnitProps (Unit unitType unitState) u1prps uCur
  | (Unit unitType unitState) == uCur = Unit unitType u1prps
  | otherwise = uCur


selectedStateHandler :: Event -> State -> State
selectedStateHandler (EventKey (SpecialKey KeyEsc) Down _ _) (Selected gameState unit) = Selected gameState unit
selectedStateHandler (EventKey (MouseButton LeftButton) Down _ (x, y)) state = determineAction state (float2Double x, float2Double y)
selectedStateHandler _e _st = _st

timeHandler :: Float -> State -> State
timeHandler _dt (Moving gameState unit coords animation) = moveCharacter (Moving gameState unit coords animation) coords
timeHandler _dt state = state