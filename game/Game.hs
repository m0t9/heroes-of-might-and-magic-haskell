{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use <$>" #-}
module Game where
import Utils (getPath, sign, CellCoords, FieldSize)
import Random (JavaRandom, randomInt)
import Data.Maybe (fromMaybe, isJust)
import Control.Monad (replicateM)
import GHC.Float (int2Double)
import Graph (Graph, generateGraph, findDistances)
import Data.Foldable (find)
import Data.List (sortBy)

-- | Game related data types
data GameState = GameState {getUnits :: [Unit], turn :: Player, queue :: [Unit], seed :: Int}
data PlayerType = LeftPlayer | RightPlayer deriving (Eq)
data Player = Player {getType :: PlayerType } deriving (Eq)

-- | Unit related data types
data UnitType =
  Pikeman | Archer | Swordsman | Monk        -- | Castle fraction
  | Dwarf | WoodElf | DenroidGuard           -- | Rampart fraction
  | Troglodyte | Harpy | Beholder | Minotaur -- | Dungeon fraction
  deriving (Eq)
data UnitProps = UnitProps {getAttackPoints :: Int, getDefensePoints :: Int, getDamage :: [Int], getHealth :: Int, getSpeed :: Int, getAmmo :: Int, isFlying :: Bool} deriving (Eq)

data UnitState = UnitState {getProps :: UnitProps, getPlayer :: Player, getCoords :: CellCoords, getHealthOfLast :: Int, getCurrentAmmo :: Int, getStackSize :: Int} deriving (Eq)

data Unit = Unit UnitType UnitState

instance (Eq Unit) where
  (Unit a b) == (Unit c d) = (a == c) && b == d
  

createUnit :: UnitType -> Player -> CellCoords -> Int -> Unit
createUnit unitType player coords stackSize = Unit unitType (UnitState props player coords (getHealth props) (getAmmo props) stackSize)
  where
    props = getInitialProps unitType

-- | Constants
fieldSize :: FieldSize
fieldSize = (15, 11)

fieldGraph :: Graph
fieldGraph = generateGraph (fst fieldSize) (snd fieldSize)

-- | State mutations
changeUnitState :: Unit -> (UnitState -> param -> UnitState) -> param -> Unit
changeUnitState (Unit unitType unitState) changeFunction stateFields = Unit unitType (changeFunction unitState stateFields)

changeStateCoords :: UnitState -> CellCoords -> UnitState
changeStateCoords state coords = UnitState (getProps state) (getPlayer state) (coords) (getHealthOfLast state) (getCurrentAmmo state) (getStackSize state)

changeStateHealthOfLast :: UnitState -> Int -> UnitState
changeStateHealthOfLast state health = UnitState (getProps state) (getPlayer state) (getCoords state) (health) (getCurrentAmmo state) (getStackSize state)

changeStateStackSize :: UnitState -> Int -> UnitState
changeStateStackSize state stackSize = UnitState (getProps state) (getPlayer state) (getCoords state) (getHealthOfLast state) (getCurrentAmmo state) (stackSize)

changeStateCurrentAmmo :: UnitState -> Int -> UnitState
changeStateCurrentAmmo state ammo = UnitState (getProps state) (getPlayer state) (getCoords state) (getHealthOfLast state) (ammo) (getStackSize state)


-- | Initial properties
getInitialProps :: UnitType -> UnitProps
-- || Castle fraction
getInitialProps Pikeman   = UnitProps 4 5 [1..3] 10 4 0 False      -- Tier 1
getInitialProps Archer    = UnitProps 6 3 [2..3] 10 4 12 False     -- Tier 2
getInitialProps Swordsman = UnitProps 6 3 [2..3] 10 4 12 False     -- Tier 4
getInitialProps Monk      = UnitProps 12 7 [10..12] 30 4 12 False  -- Tier 5
-- || Rampart fraction
getInitialProps Dwarf        = UnitProps 6 3 [2..3] 10 4 12 False    -- Tier 2
getInitialProps WoodElf      = UnitProps 9 5 [3..5] 15 6 24 False    -- Tier 3
getInitialProps DenroidGuard = UnitProps 9 12 [10..14] 55 3 0 False  -- Tier 5
-- || Dungeon fraction
getInitialProps Troglodyte = UnitProps 4 3 [1..3] 5 4 0 False      -- Tier 2
getInitialProps Harpy      = UnitProps 9 5 [3..5] 15 6 24 True     -- Tier 3
getInitialProps Beholder   = UnitProps 9 12 [10..14] 55 3 0 False  -- Tier 5
getInitialProps Minotaur   = UnitProps 9 12 [10..14] 55 3 0 False  -- Tier 5

-- | Attack strategies

-- || Default strategies
getMeleeAttackableEntities :: GameState -> Unit -> [Unit]
getMeleeAttackableEntities gameState (Unit _ state) = filter filterFunc (getUnits gameState)
  where
    coords = getCoords state
    speed' = getSpeed (getProps state)

    filterFunc :: Unit -> Bool
    filterFunc (Unit _ state') = length path - 1 <= speed'
      where
        path = getPath coords (getCoords state')


getRangedAttackableEntities :: GameState -> Unit -> [Unit]
getRangedAttackableEntities gameState unit@(Unit _ state)
  | getCurrentAmmo state > 0 = getUnits gameState
  | otherwise                = getMeleeAttackableEntities gameState unit


-- | Filters
filterFriendly :: Player -> [Unit] -> [Unit]
filterFriendly player = filter (\(Unit _ state) -> getPlayer state == player)

filterEnemy :: Player -> [Unit] -> [Unit]
filterEnemy player = filter (\(Unit _ state) -> getPlayer state /= player)


filterEnemyWrapper :: (GameState -> Unit -> [Unit]) -> GameState -> Unit -> [Unit]
filterEnemyWrapper func gameState unit@(Unit _ state) = filterEnemy (getPlayer state) (func gameState unit)

getInteractableEntitiesFunc :: UnitType -> (GameState -> Unit -> [Unit])
-- ||| Castle fraction
getInteractableEntitiesFunc Pikeman      = filterEnemyWrapper getMeleeAttackableEntities
getInteractableEntitiesFunc Swordsman    = filterEnemyWrapper getMeleeAttackableEntities
getInteractableEntitiesFunc Archer       = filterEnemyWrapper getRangedAttackableEntities
getInteractableEntitiesFunc Monk         = filterEnemyWrapper getRangedAttackableEntities
-- ||| Rampart fraction
getInteractableEntitiesFunc Dwarf        = filterEnemyWrapper getMeleeAttackableEntities
getInteractableEntitiesFunc WoodElf      = filterEnemyWrapper getRangedAttackableEntities
getInteractableEntitiesFunc DenroidGuard = filterEnemyWrapper getMeleeAttackableEntities
-- ||| Dungeon fraction
getInteractableEntitiesFunc Troglodyte   = filterEnemyWrapper getMeleeAttackableEntities
getInteractableEntitiesFunc Harpy        = filterEnemyWrapper getRangedAttackableEntities
getInteractableEntitiesFunc Beholder     = filterEnemyWrapper getMeleeAttackableEntities
getInteractableEntitiesFunc Minotaur     = filterEnemyWrapper getMeleeAttackableEntities

-- ||| Strategies for each unit
getInteractableEntities :: GameState -> Unit -> [Unit]
getInteractableEntities gameState unit@(Unit unitType _) = (getInteractableEntitiesFunc unitType) gameState unit


data AttackResult = AttackResult {getDamager :: Maybe Unit, getVictim :: Maybe Unit}

-- | Attack functions
meleeAttackWithMultiplier
  :: Double -- | Damager multiplier
  -> Unit   -- | Damager
  -> Unit   -- | Victim
  -> CellCoords -- | Coords from which unit will attack
  -> JavaRandom AttackResult
meleeAttackWithMultiplier coefficient (Unit unitType1 state1) (Unit unitType2 state2) attackCoords = do
  let stackSize = getStackSize state1
  let damage = getDamage (getProps state1)

  damages <- replicateM stackSize (do
    r <- randomInt
    let index = r `mod` (length damage)
    return (damage !! index)
    )

  let totalDamage = int2Double (sum damages) * coefficient
  let i = getAttackPoints (getProps state1) - getDefensePoints (getProps state2)
  let damageCoefficient = (1.0 + 0.1 * (int2Double (sign i))) ^ abs i

  let finalDamage = floor (totalDamage * int2Double stackSize * damageCoefficient)

  let health2 = (getHealth (getProps state2))
  let totalHealth = (getHealthOfLast state2) + (getStackSize state2 - 1) * health2
  let finalHealth = totalHealth - finalDamage 

  let movedUnit1 = Unit unitType1 (changeStateCoords state1 attackCoords)

  if finalHealth <= 0
    then return (AttackResult (Just movedUnit1) (Nothing))
    else do
      let temp = finalHealth `mod` health2

      let newHealthOfLast = if temp == 0 then health2 else temp
      let newStackSize = (finalHealth `div` health2) + (sign temp)

      let newState2 = changeStateStackSize (changeStateHealthOfLast state2 newHealthOfLast) newStackSize

      return (AttackResult (Just movedUnit1) (Just (Unit unitType2 newState2)))

meleeAttack
  :: Unit   -- | Damager
  -> Unit   -- | Victim
  -> CellCoords -- | Coords from which unit will attack
  -> JavaRandom AttackResult
meleeAttack = meleeAttackWithMultiplier 1.0 

rangeAttack
  :: Unit   -- | Damager
  -> Unit   -- | Victim
  -> CellCoords -- | Coords from which unit will attack
  -> JavaRandom AttackResult
rangeAttack unit1@(Unit _ state1) unit2@(Unit _ state2) coords = do
  let isClose = length (getPath (getCoords state1) (getCoords state2)) - 1 <= 1
  if isClose || getCurrentAmmo state2 <= 0
    then (counterAttackAttackWrapper (meleeAttackWithMultiplier 0.5)) unit1 unit2 coords
    else do
      attackResult@(AttackResult damager victim) <- meleeAttack unit1 unit2 (getCoords state1)

      case damager of
        Just (Unit damagerType damagerState) -> do
          let newDamageState = changeStateCurrentAmmo damagerState (getCurrentAmmo damagerState - 1)
          return (AttackResult (Just (Unit damagerType newDamageState)) victim)
        Nothing -> return attackResult

counterAttackAttackWrapper :: (Unit -> Unit -> CellCoords -> JavaRandom AttackResult) -> Unit -> Unit -> CellCoords -> JavaRandom AttackResult
counterAttackAttackWrapper func unit1 unit2 attackCoords = do
    firstAttack <- func unit1 unit2 attackCoords
    counterAttackIfPossibleFunc firstAttack
  where
    counterAttackIfPossibleFunc :: AttackResult -> JavaRandom AttackResult
    counterAttackIfPossibleFunc attackResult@(AttackResult damager victim) = fromMaybe defaultValue maybeValue
      where
        defaultValue = return attackResult
        maybeValue = do
          victimValue@(Unit _ victimState) <- victim
          damagerValue <- damager
          return (attack victimValue damagerValue (getCoords victimState))


getAttackFunc :: UnitType -> (Unit -> Unit -> CellCoords -> JavaRandom AttackResult)
-- ||| Castle fraction
getAttackFunc Pikeman      = counterAttackAttackWrapper meleeAttack
getAttackFunc Swordsman    = counterAttackAttackWrapper meleeAttack
getAttackFunc Archer       = rangeAttack
getAttackFunc Monk         = rangeAttack
-- ||| Rampart fraction
getAttackFunc Dwarf        = counterAttackAttackWrapper meleeAttack
getAttackFunc WoodElf      = rangeAttack
getAttackFunc DenroidGuard = counterAttackAttackWrapper meleeAttack
-- ||| Dungeon fraction
getAttackFunc Troglodyte   = counterAttackAttackWrapper meleeAttack
getAttackFunc Harpy        = rangeAttack
getAttackFunc Beholder     = rangeAttack
getAttackFunc Minotaur     = counterAttackAttackWrapper meleeAttack

attack
  :: Unit   -- | Damager
  -> Unit   -- | Victim
  -> CellCoords -- | Coords from which unit will attack
  -> JavaRandom AttackResult
attack unit1@(Unit unitType _) unit2 coords = (getAttackFunc unitType) unit1 unit2 coords

postAttack
  :: Unit       -- | Unit before attack
  -> Maybe Unit -- | Unit after attack
  -> Maybe Unit -- | Final unit
postAttack _ Nothing = Nothing
postAttack before@(Unit Harpy _) (Just (Unit Harpy _)) = Just before
postAttack _ after = after

postDefense
  :: Unit       -- | Unit before attack
  -> Maybe Unit -- | Unit after attack
  -> Maybe Unit -- | Final unit
postDefense _ after = after

postAttackHandler
  :: Unit                    -- | Before unit
  -> Unit                    -- | After unit
  -> JavaRandom AttackResult -- | States right after performed attack
  -> JavaRandom AttackResult -- | Final states after attack
postAttackHandler unit1 unit2 attackResult' = do
  (AttackResult damager victim) <- attackResult'
  return (AttackResult (postAttack unit1 damager) (postDefense unit2 victim))

-- | Cells to move
graphCellsToMove :: (GameState -> CellCoords -> Bool) -> Unit -> GameState -> [CellCoords]
graphCellsToMove isObstacleFunc (Unit unitType state) gameState = map fst filteredBySpeed
  where
    startCoords = (getCoords state)
    distances = findDistances fieldGraph startCoords (isObstacleFunc gameState)
    speed = getSpeed (getInitialProps unitType)

    filteredBySpeed = filter (\(coords, distance) -> distance < speed && coords /= startCoords) distances

defaultCellsToMove :: Unit -> GameState -> [CellCoords]
defaultCellsToMove = graphCellsToMove isUnitObstacle

flyingCellsToMove :: Unit -> GameState -> [CellCoords]
flyingCellsToMove unit gameState@(GameState units _ _ _) = filter (not . unitWithCoordsExists) cells
  where
    cells = graphCellsToMove (\_ _ -> False) unit gameState

    unitWithCoordsExists :: CellCoords -> Bool
    unitWithCoordsExists coords = isJust (find (\(Unit _ state) -> getCoords state == coords) units)
    

getCellsToMoveFunc :: UnitType -> Unit -> GameState -> [CellCoords]
-- ||| Castle fraction
getCellsToMoveFunc Pikeman      = defaultCellsToMove
getCellsToMoveFunc Swordsman    = defaultCellsToMove
getCellsToMoveFunc Archer       = defaultCellsToMove
getCellsToMoveFunc Monk         = defaultCellsToMove
-- ||| Rampart fraction
getCellsToMoveFunc Dwarf        = defaultCellsToMove
getCellsToMoveFunc WoodElf      = defaultCellsToMove
getCellsToMoveFunc DenroidGuard = defaultCellsToMove
-- ||| Dungeon fraction
getCellsToMoveFunc Troglodyte   = defaultCellsToMove
getCellsToMoveFunc Harpy        = flyingCellsToMove
getCellsToMoveFunc Beholder     = defaultCellsToMove
getCellsToMoveFunc Minotaur     = defaultCellsToMove

getCellsToMove :: Unit -> GameState -> [CellCoords]
getCellsToMove unit@(Unit unitType _) = getCellsToMoveFunc unitType unit

-- | Utilities function
isUnitObstacle :: GameState -> CellCoords -> Bool
isUnitObstacle (GameState units _ _ _) coords = not (null unitsWithSameCoords)
  where
    unitsWithSameCoords = filter (\(Unit _ state) -> (getCoords state == coords)) units 

-- | Sort given units in order to find player for next turn
sortUnits 
  :: [Unit]   -- Units of both players
  -> [Unit]
sortUnits units = sortBy cmp units
  where 
    cmp (Unit _ s1) (Unit _ s2)
      | sp1 > sp2 = LT
      | sp2 > sp1 = GT
      | y1 < y2 = LT
      | y2 < y1 = GT
      | otherwise
      = case tp1 of
        LeftPlayer -> LT
        RightPlayer -> GT
        where 
          sp1 = getSpeed $ getProps s1
          sp2 = getSpeed $ getProps s2
          y1 = snd (getCoords s1)
          y2 = snd (getCoords s2)
          tp1 = getType $ getPlayer s1
