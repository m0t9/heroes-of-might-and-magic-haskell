module Game where
import Utils (getPath)

data GameState = GameState {getUnits :: [Unit]}
data Player = Player Bool deriving (Eq)

data UnitType = Pikeman | Archer
data UnitProps = UnitProps {getAttackPoints :: Int, getDefensePoints :: Int, getDamage :: [Int], getHealth :: Int, getSpeed :: Int, getAmmo :: Int}

data UnitState = UnitState {getProps :: UnitProps, getPlayer :: Player, getCoords :: (Int, Int), getHealthOfLast :: Int, getCurrentAmmo :: Int, getStackSize :: Int}

data Unit = Unit UnitType UnitState


getMeleeAttackableEntities :: GameState -> Unit -> [Unit]
getMeleeAttackableEntities gameState (Unit _ state) = filter filterFunc (getUnits gameState)
  where
    coords = getCoords state
    speed' = getSpeed (getProps state)

    filterFunc :: Unit -> Bool
    filterFunc (Unit _ state') = length path - 1 <= speed'
      where
        path = getPath coords (getCoords state')


filterFriendly :: Player -> [Unit] -> [Unit]
filterFriendly player = filter (\(Unit _ state) -> getPlayer state == player)

filterEnemy :: Player -> [Unit] -> [Unit]
filterEnemy player = filter (\(Unit _ state) -> getPlayer state /= player)

getAttackableEntities :: GameState -> Unit -> [Unit]
getAttackableEntities gameState unit@(Unit Pikeman state) = filterEnemy (getPlayer state) (getMeleeAttackableEntities gameState unit) 
getAttackableEntities gameState (Unit Archer state)       = filterEnemy (getPlayer state) (getUnits gameState)


getInitialProps :: UnitType -> UnitProps
getInitialProps Pikeman = UnitProps 4 5 [1..3] 10 4 0
