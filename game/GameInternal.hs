{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module GameInternal where
import Utils (getPath, sign)
import Random (JavaRandom, randomInt)
import Data.Maybe (fromMaybe)
import Control.Monad (replicateM)
import GHC.Float (int2Double)

-- | Game related data types
data GameState = GameState {getUnits :: [Unit]}
data Player = Player Bool deriving (Eq)

-- | Unit related data types
data UnitType =
  Pikeman | Archer | Swordsman | Monk  -- | Castle faction
  | Dwarf | WoodElf | DenroidGuard     -- | Rampart faction
data UnitProps = UnitProps {getAttackPoints :: Int, getDefensePoints :: Int, getDamage :: [Int], getHealth :: Int, getSpeed :: Int, getAmmo :: Int}

data UnitState = UnitState {getProps :: UnitProps, getPlayer :: Player, getCoords :: (Int, Int), getHealthOfLast :: Int, getCurrentAmmo :: Int, getStackSize :: Int}

data Unit = Unit UnitType UnitState


-- | Initial properties
getInitialProps :: UnitType -> UnitProps
-- || Castle faction
getInitialProps Pikeman   = UnitProps 4 5 [1..3] 10 4 0      -- Tier 1
getInitialProps Archer    = UnitProps 6 3 [2..3] 10 4 12     -- Tier 2
getInitialProps Swordsman = UnitProps 6 3 [2..3] 10 4 12     -- Tier 4
getInitialProps Monk      = UnitProps 12 7 [10..12] 30 4 12  -- Tier 5
-- || Rampart faction
getInitialProps Dwarf             = UnitProps 6 3 [2..3] 10 4 12     -- Tier 2
getInitialProps WoodElf           = UnitProps 9 5 [3..5] 15 6 24     -- Tier 3
getInitialProps DenroidGuard      = UnitProps 9 12 [10..14] 55 3 0   -- Tier 5

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

getAttackableEntitiesFunc :: UnitType -> (GameState -> Unit -> [Unit])
-- ||| Castle faction
getAttackableEntitiesFunc Pikeman      = filterEnemyWrapper getMeleeAttackableEntities
getAttackableEntitiesFunc Swordsman    = filterEnemyWrapper getMeleeAttackableEntities
getAttackableEntitiesFunc Archer       = filterEnemyWrapper getRangedAttackableEntities
getAttackableEntitiesFunc Monk         = filterEnemyWrapper getRangedAttackableEntities
-- ||| Rampart faction
getAttackableEntitiesFunc Dwarf        = filterEnemyWrapper getMeleeAttackableEntities
getAttackableEntitiesFunc WoodElf      = filterEnemyWrapper getRangedAttackableEntities
getAttackableEntitiesFunc DenroidGuard = filterEnemyWrapper getMeleeAttackableEntities

-- ||| Strategies for each unit
getAttackableEntities :: GameState -> Unit -> [Unit]
getAttackableEntities gameState unit@(Unit unitType _) = (getAttackableEntitiesFunc unitType) gameState unit


data AttackResult = AttackResult {getDamager :: Maybe Unit, getVictim :: Maybe Unit}


-- | Attack functions
meleeAttack :: Unit -> Unit -> JavaRandom AttackResult
meleeAttack unit1@(Unit _ state1) unit2@(Unit unitType2 state2) = do

  let stackSize = getStackSize state1
  let damage = getDamage (getProps state1)

  damages <- replicateM stackSize (do
    r <- randomInt
    let index = r `mod` (length damage)
    return (damage !! index)
    )

  let totalDamage = sum damages
  let i = getAttackPoints (getProps state1) - getDefensePoints (getProps state2)
  let damageCoefficient = (1.0 + 0.1 * (int2Double (sign i))) ^ abs i

  let finalDamage = floor (int2Double totalDamage * int2Double stackSize * damageCoefficient)

  let health2 = (getHealth (getProps state2))
  let totalHealth = (getHealthOfLast state2) + (getStackSize state2 - 1) * health2

  if totalHealth == 0
    then return (AttackResult (Just unit1) (Nothing))
    else do
      let finalHealth = finalDamage - totalHealth
      let temp = finalHealth `mod` health2

      let newHealthOfLast = if temp == 0 then health2 else temp
      let newStackSize = (finalHealth `div` health2) + (sign temp)

      let newState2 = UnitState (getProps state2) (getPlayer state2) (getCoords state2) (newHealthOfLast) (getCurrentAmmo state2) (newStackSize)

      return (AttackResult (Just unit1) (Just (Unit unitType2 newState2)))


rangeAttack :: Unit -> Unit -> JavaRandom AttackResult
rangeAttack unit1@(Unit _ state1) unit2@(Unit _ state2) = return (AttackResult (Just unit1) (Just unit2))

parryAttackWrapper :: (Unit -> Unit -> JavaRandom AttackResult) -> Unit -> Unit -> JavaRandom AttackResult
parryAttackWrapper func unit1 unit2 = do
    firstAttack <- func unit1 unit2
    parryIfPossibleFunc firstAttack
  where
    parryIfPossibleFunc :: AttackResult -> JavaRandom AttackResult
    parryIfPossibleFunc attackResult@(AttackResult damager victim) = fromMaybe (return attackResult) a
      where
        a = do
          victimUnit <- victim
          damagerUnit <- damager
          return (meleeAttack damagerUnit victimUnit)


getAttackFunc :: UnitType -> (Unit -> Unit -> JavaRandom AttackResult)
-- ||| Castle faction
getAttackFunc Pikeman      = parryAttackWrapper meleeAttack
getAttackFunc Swordsman    = parryAttackWrapper meleeAttack
getAttackFunc Archer       = rangeAttack
getAttackFunc Monk         = rangeAttack
-- ||| Rampart faction
getAttackFunc Dwarf        = parryAttackWrapper meleeAttack
getAttackFunc WoodElf      = rangeAttack
getAttackFunc DenroidGuard = parryAttackWrapper meleeAttack

attack :: Unit -> Unit -> JavaRandom AttackResult
attack unit1@(Unit unitType _) unit2 = (getAttackFunc unitType) unit1 unit2
