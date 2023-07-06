module Game where
import Utils (getPath)

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

