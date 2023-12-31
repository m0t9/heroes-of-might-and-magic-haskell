module GameHandler where
import Game
import Random
import Graphics.Gloss.Interface.IO.Game
-- import Data.Ord (Down)
import GHC.Float (float2Double)
import Utils
import Graph
import Data.List (delete)
import Data.Maybe (isNothing, isJust, catMaybes, fromJust)

type Animation = Maybe [Coords]

data State
  = Selected GameState Unit (Maybe Unit)
  | Moving GameState Unit Coords Animation
  | AttackMoving GameState Unit Coords Unit CellPart Animation PostAttackParameter
  | Attacking GameState Unit Coords Unit CellPart PostAttackParameter
  | CounterAttacking GameState Unit (Maybe Unit) CellPart PostAttackParameter
  | PostAttacking GameState (Maybe Unit) PostAttackParameter
  | GameOver GameState Player

data PostAttackParameter
  = HarpyReturnPoint Coords
  | NoParams

data Action = Attack AttackType | Move CellCoords | Skip | NoAction
data HexClick = Hex CellCoords CellPart
data AttackType = RangeAttack Unit CellCoords | MeleeAttack Unit CellCoords CellPart

-- | Convert hexagonal to double coordinates using global constants
hexToCoords 
  :: Offset -- | Offset of the field
  -> CellCoords -- | Hexagonal coordinates to convert
  -> Double -- | Size of the hexagon side
  -> DoubleCoords -- | Double coordinates of hexagon
hexToCoords (xOffset, yOffset) (xHex, yHex) side =
  (xOffset + xShift, yOffset + yShift)
  where
    x = fromIntegral xHex
    y = fromIntegral yHex
    yShift = side * (1+ 1.5*y)--(1 + (2*y))
    xShift
      | even yHex = (sqrt 3) * side * (1 + x)
      | otherwise = (sqrt 3) * side * (1/2 + x)

-- | Convert hexagonal coordinates to double coordinates
hexToCoordsHMM3 
  :: CellCoords -- | Hexagonal coordinates to convert
  -> DoubleCoords -- | Double coordinates of hexagon
hexToCoordsHMM3 coords = hexToCoords offset coords hexSide

-- | Check if the target double coordinates are in some hexagon
isInHex 
  :: DoubleCoords -- | Target double coordinates to check
  -> DoubleCoords -- | Center of the target hexagon
  -> Double -- | Size of the hexagon side
  -> Bool -- | True or False
isInHex (x, y) (xHexCenter, yHexCenter) side
  | (((x-xHexCenter)*(x-xHexCenter)) + ((y-yHexCenter) * (y-yHexCenter))) > (side*side) = False
  | otherwise                                                                           = True

-- | Convert double to the hexagonal coordinates as in HMM3
coordsToHexHMM3 
  :: DoubleCoords -- | Double coordinates to convert
  -> Maybe CellCoords -- | Corresponding hexagonal coordinates in HMM3
coordsToHexHMM3 coords
  = coordsToHexWithDomain offset coords hexSide (15, 11)

-- | Convert double to the hexagonal coordinates with the domain
coordsToHexWithDomain
  :: Offset -- | Offset of the field
  -> DoubleCoords -- | Double coordinates to convert
  -> Double -- | Size of the hexagon side
  -> FieldSize -- | Size of the field
  -> Maybe CellCoords -- | Corresponding hexagonal coordinates
coordsToHexWithDomain offsetSize coords side (xMaxHex, yMaxHex)
  = case coordsToHex offsetSize coords side of
      Nothing -> Nothing
      Just (x, y) -> result
        where
          result
            | (x > xMaxHex) || (y > yMaxHex)
             || (x < 0) || (y < 0) = Nothing
            | otherwise            = Just (x, y)

-- | Convert double coordinates to the hexagonal
coordsToHex
  :: Offset -- | Offset of the field
  -> DoubleCoords -- | Double coordinates to convert
  -> Double -- | Size of the hexagon side
  -> Maybe CellCoords -- | Corresponding hexagonal coordinates
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

-- | Determine if the double coordinates are vertical part
isVerticalPart 
  :: DoubleCoords -- | Double coordinates of the hexagon center
  -> DoubleCoords -- | Target double coordinates
  -> Bool -- | True or False
isVerticalPart (xCenter, yCenter) (x, y)
  | cot > sqrt 3 = False
  | otherwise    = True
  where
    cot = leg1 / leg2
    leg1 = abs (x - xCenter)
    leg2 = abs (y - yCenter)

-- | Determine which part of hexagon the double coordinates belong to
determineCellPartHex 
  :: DoubleCoords -- | Target double coordinates
  -> CellCoords -- | Target hexagon coordinates
  -> CellPart -- | The corresponding part of hexagon
determineCellPartHex coords hexCoords = determineCellPart coords (hexToCoordsHMM3 hexCoords)
-- | Determine the part of the cell
determineCellPart 
  :: DoubleCoords -- | Target double coordinates
  -> DoubleCoords -- | Double coordinates of the hexagon center
  -> CellPart -- | The corresponding part of hexagon
determineCellPart (x, y) (xCenter, yCenter)
  | (x > xCenter) && (y > yCenter) && verticalPart            = UR
  | (x < xCenter) && (y > yCenter) && verticalPart            = UL
  | (x > xCenter) && (y < yCenter) && verticalPart            = DR
  | (x < xCenter) && (y < yCenter) && verticalPart            = DL
  | ((x > xCenter) && (y > yCenter) && (not verticalPart))
    || ((x > xCenter) && (y < yCenter) && (not verticalPart)) = R
  | otherwise                                                     = L
    where
        verticalPart = isVerticalPart (x, y) (xCenter, yCenter)

-- | Event handler of the world
gameHandler 
  :: Event -- | Incoming event
  -> State -- | Current state of the world
  -> State -- | Updated state of the world due to the event
--gameHandler _event (NoSelected gameState)    = noSelectedStateHandler _event (NoSelected gameState)
gameHandler (EventKey (Char 'r') Down _ _ ) _ = initialWorld
gameHandler (EventKey (Char 'R') Down _ _ ) _ = initialWorld
gameHandler _event (Selected gameState unit pkm) = selectedStateHandler _event (Selected gameState unit pkm)
gameHandler _event (Moving gameState unit crds animation) = (Moving gameState unit crds animation)
gameHandler _event (GameOver _gameState _player) = gameOverHandler _event (GameOver _gameState _player)
gameHandler _event _s = _s

-- | Get the hexagonal coordinates of the unit
getUnitCoords
  :: Unit -- | Target unit
  -> CellCoords -- | Corresponding hexagonal coordinates
getUnitCoords (Unit _ (UnitState _ _ coords _ _ _)) = coords

-- | Get the hexagonal coordinates of all units in the list
getCoordsOfUnits
  :: [Unit] -- | Target list of the units
  -> [CellCoords] -- | List of the corresponding hexagonal coordinates
getCoordsOfUnits = map getUnitCoords

-- | Get the hexagonal coordinates of the friendly units
getFriendlyCoords 
  :: Player -- | Target player
  -> [Unit] -- | List of all units
  -> [CellCoords] -- | List of the corresponding hexagonal coordinates 
getFriendlyCoords player units = getCoordsOfUnits (filterFriendly player units)

-- | Find the unit by the hexagonal coordinates
findUnit
  :: CellCoords -- | Target hexagonal coordinates
  -> [Unit] -- | List of all units
  -> Maybe Unit -- | Found unit or Nothing
findUnit _ [] = Nothing
findUnit coords (unit:units)
  | coords == (getUnitCoords unit) = Just unit
  | otherwise                      = findUnit coords units

-- | Find the friendly unit at specified coordinates
selectFriendlyUnit 
  :: GameState -- | Current game state
  -> CellCoords -- | Target hexagonal coordinates
  -> Maybe Unit -- | Found friendly unit or Nothing
selectFriendlyUnit gameState coords = findUnit coords friendlyUnits
  where
    friendlyUnits = filterFriendly (turn gameState) (getUnits gameState)

-- | Check if the coordinates exist in the list of hexagonal coordinates
doesExistInList 
  :: CellCoords -- | Target hexagonal coordinates
  -> [CellCoords] -- | List of the hexagonal coordinates
  -> Bool -- | True or False
doesExistInList _ [] = False
doesExistInList req (crd:crds)
  | req == crd = True
  | otherwise = doesExistInList req crds

-- | Check if the movement to the specified hexagonal coordinates is possible
isMovable 
  :: State -- | Current world state
  -> CellCoords -- | Target hexagonal coordinates
  -> Bool -- | True or False
isMovable (Selected gameState unit _pkm) coords = doesExistInList coords (getCellsToMove unit gameState)
isMovable _s _c = False

-- | Convert the click to the corresponding action
clickToAction 
  :: State -- | Current world state
  -> DoubleCoords -- | Target double coordinates
  -> Action -- | Corresponding action
clickToAction state@(Selected gameState unit _pkm) coords = case coordsToHexHMM3 coords of
  Nothing -> NoAction
  Just hexCoords -> hexToAction state (Hex hexCoords (determineCellPartHex coords hexCoords))

clickToAction _s _ = NoAction

-- | Convert click to the hexagon to the corresponding action
hexToAction 
  :: State -- | Current world state
  -> HexClick -- | Target hexagonal coordinates
  -> Action -- | Corresponding action
hexToAction state@(Selected gameState unit _pkm) (Hex coords part)
  | isMovable state coords = Move coords
  | isJust victim = isAttackable state justVictim part
  | otherwise = NoAction
  where
    victim = isInteractable gameState unit coords
    Just justVictim = victim
hexToAction _ _ = NoAction

-- | Check if the cell part with the unit may be attacked
isAttackable 
  :: State -- | Current world state
  -> Unit -- | Victim unit
  -> CellPart -- | Target cell part to check
  -> Action -- | Corresponding action
isAttackable state@(Selected gameState damager _pkm) victim part
  | (getCurrentAmmo (getUnitState damager) > 0) && not (isEnemyNear gameState damager) = Attack (RangeAttack victim damagerCell)
  | isMovable state attackCell || isDamager = Attack (MeleeAttack victim attackCell attackDirection)
  | otherwise = NoAction
  where
    attackCell = getNeighbourCell victimCell part
    victimCell = getUnitCoords victim
    damagerCell = getUnitCoords damager
    isDamager = attackCell == damagerCell
    attackDirection = invertDirection part
isAttackable _ _ _ = NoAction

-- | Get the opposite direction
invertDirection 
  :: CellPart -- | Target direction
  -> CellPart -- | The opposite direction to the target one
invertDirection UR =  DL
invertDirection UL =  DR
invertDirection DR =  UL
invertDirection DL =  UR
invertDirection R =  L
invertDirection L =  R

-- | Determine and perform the action according to the click
determineAction' 
  :: State -- | Current world state
  -> DoubleCoords -- | Target double coordinates
  -> State -- | Updated world state according to the click
determineAction' state@(Selected gameState unit _pkm) coords = case clickToAction state coords of
  NoAction
   -> state
  Skip
   -> skipTurn state
  Move moveCoords
   -> moveCharacter state moveCoords
  Attack (RangeAttack victim rangeAttackCell)
   -> attackPhase (Attacking gameState unit rangeAttackCell victim R param)
    where
      param = determinePostAttackParameter unit
  Attack (MeleeAttack victim meleeAttackCell attackDirection)
   -> moveBeforeAttack (AttackMoving gameState unit meleeAttackCell victim attackDirection animation param)
    where
      animation = getAnimationPath gameState unit meleeAttackCell
      param = determinePostAttackParameter unit
determineAction' _s _ = _s

-- | Determine the parameter that will be used during Post-Attack Stage
determinePostAttackParameter 
  :: Unit -- | Target unit
  -> PostAttackParameter -- | Corresponding Post-Attack Stage
determinePostAttackParameter (Unit Harpy state) = HarpyReturnPoint (getCoords state)
determinePostAttackParameter _ = NoParams

-- | Replace the seed in the game state
replaceSeed 
  :: GameState -- | Current game state
  -> Int -- | New seed
  -> GameState -- | Updated game state with the new seed
replaceSeed (GameState _u _p _q _s) = GameState _u _p _q

-- | Replace the unit if replacement is alive
replaceIfAlive 
  :: [Unit] -- | Current list of units
  -> (Unit, Maybe Unit) -- | Tuple of old unit and replacement unit
  -> [Unit] -- | Updated list of units
replaceIfAlive units (unit, postUnit) = case postUnit of
  Nothing -> delete unit units
  Just newUnit -> replace unit newUnit units

-- | Check if the specified cell is interactable
isInteractable 
  :: GameState -- | Current game state
  -> Unit -- | Target unit
  -> CellCoords -- | Target hexagonal coordinates
  -> Maybe Unit -- | The unit to interact with
isInteractable state unit coords = findUnit coords (getInteractableEntities state unit)

-- | Check if specified units are enemies to each other
isEnemy 
  :: Unit -- | First target unit
  -> Unit -- | Second target unit
  -> Bool -- | True or False
isEnemy (Unit _ state1) (Unit _ state2) = getPlayer state1 == getPlayer state2

-- | Check if the hexagon is attackable from the initial coordinates
isAttackableFrom 
  :: DoubleCoords -- | Target double coordinates
  -> CellCoords -- | Initial hexagon coordinates
  -> State -- | Current world state
  -> Maybe (CellCoords, Unit) -- | The tuple of coordinates and unit or Nothing
isAttackableFrom coords cellCoords state@(Selected gameState damager _pkm) = case isInteractable gameState damager cellCoords of
  Nothing -> Nothing
  Just victim -> if isMovable'
    then Just (neighbourCell, victim)
    else Nothing
  where
    isMovable' = (isMovable state neighbourCell) || (neighbourCell == (getUnitCoords damager))
    neighbourCell = getNeighbourCell cellCoords part
    part = determineCellPart coords hexCenter
    hexCenter = hexToCoordsHMM3 cellCoords
isAttackableFrom _ _ _ = Nothing

-- | Get the first unit in the list of units
getFirstUnit 
  :: [Unit] -- | List of the units
  -> Unit -- | First unit in the list
getFirstUnit (y:ys) = y

-- | Update the first unit in the list by specified function
updateFirstUnit 
  :: [Unit] -- | List of units
  -> (UnitState -> field -> UnitState) -- | Function of change
  -> field -- | The field for the function of change
  -> [Unit] -- | Updated list of units
updateFirstUnit (unit:units) changeFunction field = changeUnitState unit changeFunction field : units
updateFirstUnit [] _ _ = []

-- | Update the last unit in the list by specified function
updateLastUnit 
  :: [Unit] -- | List of units
  -> (UnitState -> field -> UnitState) -- | Function of change
  -> field -- | The field for the function of change
  -> [Unit] -- | Updated list of units
updateLastUnit [] _ _ = []
updateLastUnit [unit] changeFunction field = [changeUnitState unit changeFunction field]
updateLastUnit (unit:units) changeFunction field = unit : updateLastUnit units changeFunction field

-- | Determine the first player in the units queue
determineTheFirst 
  :: [Unit] -- | Queue of the units
  -> Player -- | The first player in the queue 
determineTheFirst [] = Player RightPlayer
determineTheFirst (unit:units) = getPlayer
  where
    (Unit _unitType (UnitState _ getPlayer _ _ _ _ )) = unit

-- | Generate the graph to navigate in the field
graph 
  :: Graph -- | Generated graph
graph = generateGraph 15 11

-- | Skip the current turn
skipTurn 
  :: State -- | Current world state
  -> State -- | Updated world state with the skipped turn
skipTurn (Selected gameState unit pkm) = Selected updatedGameState updatedUnit pkm
  where
    (GameState _units _ queue r_) = gameState
    updatedQueue = moveUnitToQueueEnd unit queue
    updatedUnit = getFirstUnit updatedQueue
    (Unit _ state) = updatedUnit
    updatedPlayer = getPlayer state
    updatedGameState = GameState _units updatedPlayer updatedQueue r_
skipTurn _s = _s

-- | Move the unit from the world state to the specified coordinates
moveCharacter 
  :: State -- | Current world state with the unit
  -> CellCoords -- | Target coordinates
  -> State -- | Updated world state with the moved unit
moveCharacter (Selected gameState unit _pkm) crds = Moving gameState unit crds newAnimation
  where
    newAnimation = getAnimationPath gameState unit crds

moveCharacter (Moving (GameState units (Player turn) queue _r) unit _crdsState animation) crds
  | (unitCoords == crds) = Selected (GameState units finalPlayer finalQueue _r) finalUnit Nothing
  | otherwise = Moving (GameState updatedUnits (Player turn) updatedQueue _r) updatedUnit crds updatedAnimation
  where
    (frame, updatedAnimation) = getFrame animation

    finalQueue = moveUnitToQueueEnd unit queue
    finalUnit = getFirstUnit finalQueue
    finalPlayer = determineTheFirst finalQueue

    unitCoords = getUnitCoords unit
    updatedUnit = changeUnitState unit changeStateCoords frame
    updatedQueue = replace unit updatedUnit queue
    updatedUnits = replace unit updatedUnit units
moveCharacter _s _ = _s

-- | Perform the move before attack
moveBeforeAttack 
  :: State -- | Current world state
  -> State -- | Updated world state
moveBeforeAttack (AttackMoving gameState unit coords _v _d animation _param)
  | getUnitCoords unit == coords = attackPhase (Attacking gameState unit coords _v _d _param)
  | otherwise                    = AttackMoving newGameState updatedUnit coords _v _d updatedAnimation _param
    where
      (frame, updatedAnimation) = getFrame animation
      GameState units _p queue _r = gameState
      updatedUnit = changeUnitState unit changeStateCoords frame
      updatedQueue = updateFirstUnit queue changeStateCoords frame
      updatedUnits = replace unit updatedUnit units
      newGameState = GameState updatedUnits _p updatedQueue _r

moveBeforeAttack s_ = s_

-- | Perform the Attack Phase
attackPhase 
  :: State -- | Current world state
  -> State -- | Updated world state
attackPhase (Attacking gameState damager coords victim _d param) = case (postVictim, param) of
 -- Nothing -> PostAttacking newGameState (Just damager) _param
  (Nothing, HarpyReturnPoint returnCoords) -> moveCharacter movingBack returnCoords
  (Nothing, _) -> selected
  (Just _, _) -> counterAttackPhase (CounterAttacking newGameState damager postDamager _d param)
  where
    movingBack = Selected newGameState newUnit Nothing
    GameState units _p queue seed = gameState
    (newSeed, attackResult) = runJavaRandom (attack gameState damager victim coords) seed
    newGameState = GameState newUnits newPlayer newQueue newSeed
    postDamager = getDamager attackResult
    postVictim = getVictim attackResult
    newUnit = getFirstUnit newQueue
    newPlayer = getPlayer (getUnitState newUnit)
    newUnits = replaceIfAlive units (victim, postVictim)
    newQueue = replaceIfAlive queue (victim, postVictim)
    selected = Selected selectedGameState selectedUnit Nothing
    selectedQueue = replaceIfAlive (moveUnitToQueueEnd damager queue) (victim, postVictim)
    selectedUnit = getFirstUnit selectedQueue
    selectedPlayer = getPlayer (getUnitState selectedUnit)
    selectedGameState = GameState newUnits selectedPlayer selectedQueue newSeed

attackPhase _s = _s

-- | Perform the Counter-Attack Phase
counterAttackPhase 
  :: State -- | Current world state
  -> State -- | Updated world state
-- counterAttackPhase (CounterAttacking gameState damager postDamager _d _param) = postAttackPhase (PostAttacking newGameState postDamager _param)
counterAttackPhase (CounterAttacking gameState damager postDamager _d param) = case (postDamager, param) of
  (Nothing, _) -> selected
  -- Just _ -> PostAttacking newGameState postDamager _param
  (Just u, HarpyReturnPoint coords) -> moveCharacter (Selected newGameState u Nothing) coords
  (Just _, _) -> selected
  where

     GameState units _p queue _s = gameState
     newGameState = GameState newUnits newPlayer newQueue _s
     newUnits = replaceIfAlive units (damager, postDamager)
     newQueue = replaceIfAlive queue (damager, postDamager)
     newUnit = getFirstUnit newQueue
     newPlayer = getPlayer (getUnitState newUnit)
     selected = Selected selectedGameState selectedUnit Nothing
     selectedQueue = replaceIfAlive (moveUnitToQueueEnd damager queue) (damager, postDamager)
     selectedUnit = getFirstUnit selectedQueue
     selectedPlayer = getPlayer (getUnitState selectedUnit)
     selectedGameState = GameState newUnits selectedPlayer selectedQueue _s

counterAttackPhase _s = _s
{-
postAttackPhase :: State -> State
postAttackPhase (PostAttacking gameState postUnit param) = case param of
  NoParams -> selected
    where
      (GameState units _p queue _r) = gameState
      newGameState = GameState units newPlayer newQueue _r
      unit = getFirstUnit queue
      newQueue = moveUnitToQueueEnd unit queue
      newUnit = getFirstUnit newQueue
      newPlayer = getPlayer (getUnitState newUnit)
      selected = Selected newGameState newUnit Nothing
  HarpyReturnPoint coords -> selected
    where
      (GameState units _p queue _r) = gameState
      newGameState = GameState units newPlayer newQueue _r
      unit = getFirstUnit queue
      newQueue = moveUnitToQueueEnd unit queue
      newUnit = getFirstUnit newQueue
      newPlayer = getPlayer (getUnitState newUnit)
      selected = Selected newGameState newUnit Nothing
      movingBack = Selected gameState unit Nothing
      harpyReturn = case postUnit of
        Just _ -> moveCharacter movingBack coords
        Nothing -> selected

postAttackPhase _s = _s
-}

-- | Get the unit animation path for the target coordinates
getAnimationPath 
  :: GameState -- | Current game state
  -> Unit -- | Target unit
  -> Coords -- | Target coordinates
  -> Maybe [Coords] -- | The animation path or Nothing
getAnimationPath _state (Unit Harpy props) coords = findPath graph (getUnitCoords (Unit Harpy props)) coords (const False)
getAnimationPath state unit coords = findPath graph (getUnitCoords unit) coords (isUnitObstacle state)

-- Extract the next frame of the animation
getFrame 
  :: Animation -- | Target animation
  -> (Coords, Animation) -- | The tuple of frame and updated animation
getFrame frames = case frames of
  Nothing -> ((0, 0), Nothing)
  Just (fr:frms) -> (fr, Just frms)
  Just [] -> ((0, 0), Nothing)

-- | Get the inverted player
invertPlayer 
  :: Player -- | Target player
  -> Player -- | The inverted player
invertPlayer (Player LeftPlayer) = Player RightPlayer
invertPlayer (Player RightPlayer) = Player LeftPlayer

-- | Move the unit to the queue start
moveUnitToQueueStart 
  :: Unit -- | Target unit
  -> [Unit] -- | The current queue of units
  -> [Unit] -- | The updated queue of units
moveUnitToQueueStart _ [] = []
moveUnitToQueueStart unit units = unit : delete unit units

-- | Move the unit to the queue end
moveUnitToQueueEnd 
  :: Unit -- | Target unit
  -> [Unit] -- | The current queue of units
  -> [Unit] -- | The updated queue of units
moveUnitToQueueEnd unit [] = [unit]
moveUnitToQueueEnd unit (ut:uts)
  | unit == ut = uts ++ [unit]
  | otherwise = ut : moveUnitToQueueEnd unit uts

-- | The event handle for the Selected world state
selectedStateHandler 
  :: Event -- | Incoming event 
  -> State -- | The current world state
  -> State -- | The updated world state
selectedStateHandler (EventKey (SpecialKey KeyEsc) Down _ _) (Selected gameState unit pkm) = Selected gameState unit pkm
selectedStateHandler (EventKey (SpecialKey KeySpace) Down _ _) (Selected gameState unit pkm) = skipTurn (Selected gameState unit pkm)
selectedStateHandler (EventKey (MouseButton LeftButton) Down _ (x, y)) state = determineAction' state (float2Double x, float2Double y)
selectedStateHandler (EventKey (MouseButton RightButton) Down _ (x, y)) (Selected gameState unit _pkm) = Selected gameState unit determinedPKM
  where
    determinedPKM = case cellCoords of 
      (-1, -1) -> Nothing
      crds -> findUnit cellCoords (getUnits gameState)
    cellCoords = case coordsToHex offset (float2Double x, float2Double y) hexSide of
      Just crds -> crds
      Nothing -> (-1, -1)
selectedStateHandler (EventKey (MouseButton RightButton) Up _ _crds) (Selected gameState unit _pkm) = Selected gameState unit Nothing
selectedStateHandler _e (Selected gameState unit pkm) = case filterEnemy player units of
  [] -> GameOver gameState player
  _ -> Selected gameState unit pkm
  where
    (GameState units player _queue _r) = gameState
selectedStateHandler _e st = st

-- | Handle the events in the Game Over screen
gameOverHandler 
  :: Event -- | Incoming event
  -> State -- | Current world state
  -> State -- | Updated world state
gameOverHandler (EventKey (SpecialKey KeyEnter) Down _ _ ) (GameOver _gameState _player) = initialWorld
  
gameOverHandler _event state = state

-- | Initial state of the world
initialWorld 
  :: State -- | Initial state of the world
initialWorld = Selected (GameState unitsStart firstPlayer sortedUnits 0) firstUnit Nothing
  where
    firstUnit = getFirstUnit sortedUnits
    firstPlayer = determineTheFirst sortedUnits
    sortedUnits = sortUnits unitsStart

-- | Time handler for the world
timeHandler 
  :: Float -- | New time
  -> State -- | Current world state
  -> State -- | Updated world state due to the time
timeHandler _dt (Moving gameState unit coords animation) = moveCharacter (Moving gameState unit coords animation) coords
timeHandler _dt (AttackMoving _gs _at coords _v _d _an _param) = moveBeforeAttack (AttackMoving _gs _at coords _v _d _an _param)
timeHandler _dt state = state


-- | Definition of the left player
playerLeft :: Player
playerLeft = Player LeftPlayer
-- | Definition of the right player
playerRight :: Player
playerRight = Player RightPlayer

-- | Initial list of the units
unitsStart 
  :: [Unit] -- | List of the units on their initial positions
unitsStart = [
    createUnit Pikeman playerLeft (0, 0) 100,
    createUnit Archer playerLeft (0, 1) 100,
    createUnit Swordsman playerLeft (0, 2) 100,
    createUnit Monk playerLeft (0, 3) 100,
    createUnit Dwarf playerLeft (0, 4) 100,
    createUnit WoodElf playerLeft (0, 5) 100,
    createUnit DenroidGuard playerLeft (0, 6) 100,
    createUnit Troglodyte playerLeft (0, 7) 100,
    createUnit Harpy playerLeft (0, 8) 100,
    createUnit Beholder playerLeft (0, 9) 100,
    createUnit Minotaur playerLeft (0, 10) 100,

    createUnit Pikeman playerRight (14, 0) 10,
    createUnit Archer playerRight (14, 1) 10,
    createUnit Swordsman playerRight (14, 2) 10,
    createUnit Monk playerRight (14, 3) 10,
    createUnit Dwarf playerRight (14, 4) 10,
    createUnit WoodElf playerRight (14, 5) 10,
    createUnit DenroidGuard playerRight (14, 6) 10,
    createUnit Troglodyte playerRight (14, 7) 10,
    createUnit Harpy playerRight (14, 8) 10,
    createUnit Beholder playerRight (14, 9) 10,
    createUnit Minotaur playerRight (14, 10) 10
  ]