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

hexToCoordsHMM3 :: CellCoords -> DoubleCoords
hexToCoordsHMM3 coords = hexToCoords offset coords hexSide

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


isVerticalPart :: DoubleCoords -> DoubleCoords -> Bool
isVerticalPart (xCenter, yCenter) (x, y)
  | cot > sqrt 3 = False
  | otherwise    = True
  where
    cot = leg1 / leg2
    leg1 = abs (x - xCenter)
    leg2 = abs (y - yCenter)

determineCellPartHex :: DoubleCoords -> CellCoords -> CellPart
determineCellPartHex coords hexCoords = determineCellPart coords (hexToCoordsHMM3 hexCoords)
determineCellPart :: DoubleCoords -> DoubleCoords -> CellPart
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

type Animation = Maybe [Coords]

data State
  = Selected GameState Unit
  | Moving GameState Unit Coords Animation
  | AttackMoving GameState Unit Coords Unit CellPart Animation PostAttackParameter
  | Attacking GameState Unit Coords Unit CellPart PostAttackParameter
  | CounterAttacking GameState Unit (Maybe Unit) CellPart PostAttackParameter
  | PostAttacking GameState (Maybe Unit) PostAttackParameter
  | GameOver GameState Player

data PostAttackParameter
  = HarpyReturnPoint Coords
  | NoParams  


gameHandler :: Event -> State -> State
--gameHandler _event (NoSelected gameState)    = noSelectedStateHandler _event (NoSelected gameState)
gameHandler _event (Selected gameState unit) = selectedStateHandler _event (Selected gameState unit)
gameHandler _event (Moving gameState unit crds animation) = (Moving gameState unit crds animation)
gameHandler _event (GameOver _gameState _player) = gameOverHandler _event (GameOver _gameState _player)
gameHandler _event _s = _s


getUnitCoords::Unit -> CellCoords
getUnitCoords (Unit _ (UnitState _ _ coords _ _ _)) = coords


getCoordsOfUnits:: [Unit] -> [CellCoords]
getCoordsOfUnits = map getUnitCoords


getFriendlyCoords :: Player -> [Unit] -> [CellCoords]
getFriendlyCoords player units = getCoordsOfUnits (filterFriendly player units)




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
isMovable _s _c = False

data Action = Attack AttackType | Move CellCoords | Skip | NoAction
data HexClick = Hex CellCoords CellPart
data AttackType = RangeAttack Unit CellCoords | MeleeAttack Unit CellCoords CellPart

clickToAction :: State -> DoubleCoords -> Action
clickToAction state@(Selected gameState unit) coords = case coordsToHexHMM3 coords of
  Nothing -> NoAction
  Just hexCoords -> hexToAction state (Hex hexCoords (determineCellPartHex coords hexCoords))

clickToAction _s _ = NoAction

hexToAction :: State -> HexClick -> Action
hexToAction state@(Selected gameState unit) (Hex coords part)
  | isMovable state coords = Move coords
  | isJust victim = isAttackable state justVictim part
  | otherwise = NoAction
  where
    victim = isInteractable gameState unit coords
    Just justVictim = victim
hexToAction _ _ = NoAction

isAttackable :: State -> Unit -> CellPart -> Action
isAttackable state@(Selected gameState damager) victim part
  | getCurrentAmmo (getUnitState damager) > 0 = Attack (RangeAttack victim damagerCell)
  | isMovable state attackCell || isDamager = Attack (MeleeAttack victim attackCell attackDirection)
  | otherwise = NoAction
  where
    attackCell = getNeighbourCell victimCell part
    victimCell = getUnitCoords victim
    damagerCell = getUnitCoords damager
    isDamager = attackCell == damagerCell
    attackDirection = invertDirection part
isAttackable _ _ _ = NoAction

invertDirection :: CellPart -> CellPart
invertDirection UR =  DL
invertDirection UL =  DR
invertDirection DR =  UL
invertDirection DL =  UR
invertDirection R =  L
invertDirection L =  R

determineAction' :: State -> DoubleCoords -> State
determineAction' state@(Selected gameState unit) coords = case clickToAction state coords of
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

determinePostAttackParameter :: Unit -> PostAttackParameter
determinePostAttackParameter (Unit Harpy state) = HarpyReturnPoint (getCoords state)
determinePostAttackParameter _ = NoParams 
replaceSeed :: GameState -> Int -> GameState
replaceSeed (GameState _u _p _q _s) = GameState _u _p _q

determineAction :: State -> DoubleCoords -> State
determineAction (Selected gameState unit) coords =
  case (coordsToHexHMM3 coords) of
    Nothing -> Selected gameState unit
    Just (x, y) -> if isMovable (Selected gameState unit) (x, y)
      then moveCharacter (Selected gameState unit) (x, y)
      else case isAttackableFrom coords (x, y)  (Selected gameState unit) of
        Nothing -> Selected gameState unit
        Just (place, victim) -> performAttack (Selected gameState unit) unit victim place
determineAction state _ = state


performAttack
  :: State
  -> Unit
  -> Unit
  -> CellCoords
  -> State
performAttack (Selected gameState unit) damager victim place = newState
  where
    newState = Selected newGameState newUnit
    result = attack damager victim place
    (newSeed, AttackResult postDamager postVictim) = runJavaRandom result seed
    GameState units _player queue seed = gameState
    newQueue = replaceAttackUnits (moveUnitToQueueEnd unit queue)
    newUnits = replaceAttackUnits units
    replaceAttackUnits unitList = replaceIfAlive (replaceIfAlive unitList (victim, postVictim)) (damager, postDamager)
    newUnit = getFirstUnit newQueue
    (Unit _unitType unitState) = newUnit
    newPlayer = getPlayer unitState
    newGameState = GameState newUnits newPlayer newQueue newSeed


performAttack s_ _ _ _ = s_


replaceIfAlive :: [Unit] -> (Unit, Maybe Unit) -> [Unit]
replaceIfAlive units (unit, postUnit) = case postUnit of
  Nothing -> delete unit units
  Just newUnit -> replace unit newUnit units

isInteractable :: GameState -> Unit -> CellCoords -> Maybe Unit
isInteractable state unit coords = findUnit coords (getInteractableEntities state unit)

isEnemy :: Unit -> Unit -> Bool
isEnemy (Unit _ state1) (Unit _ state2) = getPlayer state1 == getPlayer state2


isAttackableFrom :: DoubleCoords -> CellCoords -> State -> Maybe (CellCoords, Unit)
isAttackableFrom coords cellCoords state@(Selected gameState damager) = case isInteractable gameState damager cellCoords of
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

getFirstUnit :: [Unit] -> Unit
getFirstUnit (y:ys) = y

updateFirstUnit :: [Unit] -> (UnitState -> field -> UnitState) -> field -> [Unit]
updateFirstUnit (unit:units) changeFunction field = changeUnitState unit changeFunction field : units
updateFirstUnit [] _ _ = []

updateLastUnit :: [Unit] -> (UnitState -> field -> UnitState) -> field -> [Unit]
updateLastUnit [] _ _ = []
updateLastUnit [unit] changeFunction field = [changeUnitState unit changeFunction field]
updateLastUnit (unit:units) changeFunction field = unit : updateLastUnit units changeFunction field

determineTheFirst :: [Unit] -> Player
determineTheFirst [] = Player RightPlayer
determineTheFirst (unit:units) = getPlayer
  where
    (Unit _unitType (UnitState _ getPlayer _ _ _ _ )) = unit

graph :: Graph
graph = generateGraph 15 11

skipTurn :: State -> State
skipTurn (Selected gameState unit) = Selected updatedGameState updatedUnit
  where
    (GameState _units _ queue r_) = gameState
    updatedQueue = moveUnitToQueueEnd unit queue
    updatedUnit = getFirstUnit updatedQueue
    (Unit _ state) = updatedUnit
    updatedPlayer = getPlayer state
    updatedGameState = GameState _units updatedPlayer updatedQueue r_
skipTurn _s = _s

moveCharacter :: State -> CellCoords -> State
moveCharacter (Selected gameState unit) crds = Moving gameState unit crds newAnimation
  where
    newAnimation = getAnimationPath gameState unit crds


moveCharacter (Moving (GameState units (Player turn) queue _r) unit _crdsState animation) crds
  | (unitCoords == crds) = Selected (GameState units finalPlayer finalQueue _r) finalUnit
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

moveBeforeAttack :: State -> State
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

attackPhase :: State -> State
attackPhase (Attacking gameState damager coords victim _d param) = case (postVictim, param) of
 -- Nothing -> PostAttacking newGameState (Just damager) _param
  (Nothing, HarpyReturnPoint returnCoords) -> moveCharacter movingBack returnCoords
  (Nothing, _) -> selected
  (Just _, _) -> counterAttackPhase (CounterAttacking newGameState damager postDamager _d param) 
  where
    movingBack = Selected newGameState newUnit
    GameState units _p queue seed = gameState
    (newSeed, attackResult) = runJavaRandom (attack damager victim coords) seed
    newGameState = GameState newUnits newPlayer newQueue newSeed
    postDamager = getDamager attackResult
    postVictim = getVictim attackResult
    newUnit = getFirstUnit newQueue
    newPlayer = getPlayer (getUnitState newUnit)
    newUnits = replaceIfAlive units (victim, postVictim)
    newQueue = replaceIfAlive queue (victim, postVictim)
    selected = Selected selectedGameState selectedUnit
    selectedQueue = replaceIfAlive (moveUnitToQueueEnd damager queue) (victim, postVictim)
    selectedUnit = getFirstUnit selectedQueue
    selectedPlayer = getPlayer (getUnitState selectedUnit)   
    selectedGameState = GameState newUnits selectedPlayer selectedQueue newSeed

attackPhase _s = _s

counterAttackPhase :: State -> State
-- counterAttackPhase (CounterAttacking gameState damager postDamager _d _param) = postAttackPhase (PostAttacking newGameState postDamager _param)
counterAttackPhase (CounterAttacking gameState damager postDamager _d param) = case (postDamager, param) of 
  (Nothing, _) -> selected
  -- Just _ -> PostAttacking newGameState postDamager _param
  (Just u, HarpyReturnPoint coords) -> moveCharacter (Selected newGameState u) coords
  (Just _, _) -> selected
  where
    
     GameState units _p queue _s = gameState
     newGameState = GameState newUnits newPlayer newQueue _s
     newUnits = replaceIfAlive units (damager, postDamager)
     newQueue = replaceIfAlive queue (damager, postDamager)
     newUnit = getFirstUnit newQueue
     newPlayer = getPlayer (getUnitState newUnit)
     selected = Selected selectedGameState selectedUnit
     selectedQueue = replaceIfAlive (moveUnitToQueueEnd damager queue) (damager, postDamager)
     selectedUnit = getFirstUnit selectedQueue
     selectedPlayer = getPlayer (getUnitState selectedUnit)   
     selectedGameState = GameState newUnits selectedPlayer selectedQueue _s
     
counterAttackPhase _s = _s

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
      selected = Selected newGameState newUnit
  HarpyReturnPoint coords -> selected
    where 
      (GameState units _p queue _r) = gameState
      newGameState = GameState units newPlayer newQueue _r
      unit = getFirstUnit queue
      newQueue = moveUnitToQueueEnd unit queue
      newUnit = getFirstUnit newQueue
      newPlayer = getPlayer (getUnitState newUnit)
      selected = Selected newGameState newUnit
      movingBack = Selected gameState unit
      harpyReturn = case postUnit of
        Just _ -> moveCharacter movingBack coords
        Nothing -> selected
     
postAttackPhase _s = _s  

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
selectedStateHandler (EventKey (SpecialKey KeySpace) Down _ _) (Selected gameState unit) = skipTurn (Selected gameState unit)
selectedStateHandler (EventKey (MouseButton LeftButton) Down _ (x, y)) state = determineAction' state (float2Double x, float2Double y)
selectedStateHandler _e (Selected gameState unit) = case filterEnemy player units of
  [] -> GameOver gameState player
  _ -> Selected gameState unit
  where
    (GameState units player _queue _r) = gameState
selectedStateHandler _e st = st
gameOverHandler :: Event -> State -> State
gameOverHandler (EventKey (SpecialKey KeyEnter) Down _ _ ) (GameOver _gameState _player) = Selected (GameState unitsStart firstPlayer sortedUnits 0) firstUnit
  where
    firstUnit = getFirstUnit sortedUnits
    firstPlayer = determineTheFirst sortedUnits
    sortedUnits = sortUnits unitsStart
gameOverHandler _event state = state

timeHandler :: Float -> State -> State
timeHandler _dt (Moving gameState unit coords animation) = moveCharacter (Moving gameState unit coords animation) coords
timeHandler _dt (AttackMoving _gs _at coords _v _d _an _param) = moveBeforeAttack (AttackMoving _gs _at coords _v _d _an _param) 
timeHandler _dt state = state


playerLeft :: Player
playerLeft = Player LeftPlayer
playerRight :: Player
playerRight = Player RightPlayer
unitsStart :: [Unit]
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