{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics where
import Graphics.Gloss
import GameHandler
import Game
import Utils
import Codec.Picture ( convertRGBA8, readImage, DynamicImage )
import Graphics.Gloss.Juicy
import Data.Maybe
import GHC.Float

-- | Colors
leftPlayerFieldColor :: Color
leftPlayerFieldColor = makeColor 0.529 1 0.616 0.6
rightPlayerFieldColor :: Color
rightPlayerFieldColor = makeColor 1 0.529 0.616 0.6
selectedCellColor :: Color
selectedCellColor = makeColor 255 255 255 0.15
selectedCellColorDefault :: Color
selectedCellColorDefault = makeColor 255 255 255 0.1
chosenUnitCellColor :: Color
chosenUnitCellColor = makeColor 1 0.5 1 1
borderColor :: Color
borderColor = makeColor 0.953 0.855 0.871 1
stackColor :: Color
stackColor = makeColor 0.38 0.129 0.631 1
hpColor :: Color
hpColor = makeColor 0.282 0.188 0.102 1
statsColor :: Color
statsColor = makeColor 0.45 0.255 0.102 1

-- | Convert hex coords to usual
currentConversion 
  :: CellCoords 
  -> DoubleCoords
currentConversion hexCoords = hexToCoords offset hexCoords hexSide

-- | Get coordinates to draw hexagon
makeHexagonDotSet 
  :: CellCoords           -- | Coordinates of cell
  -> Double               -- | Size of hexagon side
  -> [(Float, Float)]
makeHexagonDotSet hexCoords side =
   [up, upRight, downRight, down, downLeft, upLeft]
     where
      (x, y) = currentConversion hexCoords
      halfSide = side/2
      sqrtSide = side * (sqrt 3) /2
      up = (realToFrac x, realToFrac (y+side))
      upRight = (realToFrac (x + sqrtSide), realToFrac (y + halfSide))
      downRight = (realToFrac (x + sqrtSide), realToFrac (y - halfSide))
      down = (realToFrac x, realToFrac (y-side))
      downLeft = (realToFrac (x - sqrtSide), realToFrac (y - halfSide))
      upLeft = (realToFrac (x - sqrtSide), realToFrac (y + halfSide))

-- | Types for render
type Offset = (Double, Double)
type Field = Picture
data CellPart = UR | UL | L | DL | DR | R

renderFieldWithState 
  :: [Unit]                     -- | List of units to draw
  -> [(UnitType, Picture)]      -- | Units' sprites
  -> [(String, Picture)]        -- | Game screens
  -> Unit                       -- | Selected unit
  -> Picture
renderFieldWithState units assets screenAssets unit = renderField units assets <> 
  translate (realToFrac (x+offSetX)) (realToFrac (y-25)) (displayStats (fromMaybe blank (lookup "stats" screenAssets)) unit assets)
  where
    (x, y) = currentConversion (getUnitCoords unit)

    offSetX = case cellPlayerDirection of
      1 -> 20
      -1 -> -240
      _ -> 0
    --(xSt, ySt) = (xConv + xOff, yConv + yOff)
    cellPlayerDirection = case getType (getPlayer (getUnitState unit)) of
      LeftPlayer -> 1
      RightPlayer -> -1
-- RENDERERS
-- We are rendering the whole situation right here.
renderState :: Picture -> [(UnitType, Picture)] -> [(String, Picture)] -> State -> Picture
--renderState (NoSelected (GameState units _turn _queue)) = renderField units
renderState background assets screenAssets (Selected (GameState units turn _queue _r) unit pkm) = case pkm of
  Nothing -> background <> (renderSelection (GameState units turn _queue _r) unit <> 
    selectedCellUnit unit assets <> renderField units assets)
  Just unitPKM -> background <> (renderSelection (GameState units turn _queue _r) unit <>
    selectedCellUnit unit assets <> renderFieldWithState units assets screenAssets unitPKM)
renderState background assets _gmvr (Moving state _unit _coords _animation) = 
  background <> renderField units assets
  where
    (GameState units _turn _queue _r) = state
renderState background assets _gmvr 
  (AttackMoving state _damager _coords _victim _d _animation _param) = 
  background <> renderField units assets
  where
    (GameState units _turn _queue _r) = state    
renderState background assets _gmvr (Attacking state _damager _coords _victim _d _param) = 
  background <> renderField units assets
  where
    (GameState units _turn _queue _r) = state      
renderState background assets _gmvr (CounterAttacking state _damager _postDamager _d _param) = 
  background <> renderField units assets
  where
    (GameState units _turn _queue _r) = state    
renderState background assets _gmvr (PostAttacking state _post _param) = 
  background <> renderField units assets
  where
    (GameState units _turn _queue _r) = state       
renderState background assets gameOverAssets (GameOver state winner) =
  background <> renderField units assets <> winnerTable winner gameOverAssets
  where
    (GameState units _turn _queue _r) = state

-- | Render end game screen
winnerTable 
  :: Player                 -- | Winner
  -> [(String, Picture)]    -- | Sprites
  -> Picture
winnerTable player gameOverAssets = translate 0 0 (fromMaybe blank (lookup "gameover" gameOverAssets))
  <> translate (-50) 25 case player of
      Player LeftPlayer -> fromMaybe blank (lookup "leftplayer" gameOverAssets)
      Player RightPlayer -> fromMaybe blank (lookup "rightplayer" gameOverAssets)

-- | Render all selected (available to walk) cells
renderSelection 
  :: GameState  -- | Current game state
  -> Unit       -- | Unit to draw cells for
  -> Picture
renderSelection gameState unit = pictures (map selectedCell (getCellsToMove unit gameState))

-- | Get picture of selected cell (available for move)
selectedCell 
  :: CellCoords       -- | Coords of selected cell
  -> Picture
selectedCell coords = color selectedCellColor (drawCell coords polygon)

-- | Draw unit at selected cell
selectedCellUnit 
  :: Unit                   -- | Unit to draw
  -> [(UnitType, Picture)]  -- | Sprites for units
  -> Picture
selectedCellUnit unit assets = color chosenUnitCellColor (drawCell coords polygon) <> 
  renderUnit coords unit getSelectedUnitPicture assets <> 
  renderUnit coords unit getUnitPicture assets
  where
    coords = getUnitCoords unit

-- | Draw field, units on it, highlighted cells
renderField 
  :: [Unit]                 -- | List of units
  -> [(UnitType, Picture)]  -- | List of sprites for units
  -> Picture
renderField units assets = drawField (0, 0) <> drawUnitCells units <> drawUnits units assets

-- | Recursively draw entire game field
drawField 
  :: CellCoords   -- | Current coordinates
  -> Picture
drawField (x, y)
  | (x == xF - 1 && y == yF - 1) = 
      color selectedCellColorDefault (drawCell (x, y) polygon) 
      <> color black (drawCell (x, y) lineLoop)
  | (x == xF - 1) = drawField (0, y+1) <> 
    color selectedCellColorDefault (drawCell (x, y) polygon) <> 
    color black (drawCell (x, y) lineLoop)
  | otherwise = drawField (x+1, y) <> 
    color selectedCellColorDefault (drawCell (x, y) polygon) <>
      color black (drawCell (x, y) lineLoop)
  where
    (xF, yF) = fieldSize

-- | Draw cell on field
drawCell 
  :: CellCoords         -- | Coords to draw cell at
  -> (Path -> Picture)  -- | Drawer
  -> Field
drawCell cellCoords drawFunc = drawFunc (makeHexagonDotSet cellCoords hexSide)

-- | Draw multiple units
drawUnits 
  :: [Unit]                     -- | Units to draw  
  -> [(UnitType, Picture)]      -- | Sprites for units
  -> Picture
drawUnits units assets = pictures (map (drawUnit assets) units)

-- | Convenient function to draw one unit
drawUnit 
  :: [(UnitType, Picture)]      -- | List of sprites for units
  -> Unit                       -- | Unit to draw
  -> Picture
drawUnit assets unit = renderUnit (getUnitCoords unit) unit getUnitPicture assets

-- | Higlight cells where units stay
drawUnitCells 
  :: [Unit]    -- | List of units to draw cells for
  -> Picture
drawUnitCells units = pictures (map unitCell units)
  where
    unitCell unit = color cellPlayerColor (drawCell (x, y) polygon)
      where
        cellPlayerColor = case getType (getPlayer (getUnitState unit)) of
          LeftPlayer -> leftPlayerFieldColor
          RightPlayer -> rightPlayerFieldColor
        (x, y) = getCoords $ getUnitState unit

-- | Render unit on field
renderUnit 
  :: CellCoords                                       -- | Coords to render unit at
  -> Unit                                             -- | Unit to render
  -> (Unit -> [(UnitType, Picture)] -> Picture)       -- | Function to find unit picture
  -> [(UnitType, Picture)]                            -- | List of sprites for units
  -> Picture                                
renderUnit (x, y) unit renderer assets = 
  translate (realToFrac realX) (realToFrac realY + 10) (scale cellPlayerDirection 1 
    (renderer unit assets))
      <> translate (realToFrac realXSt) (realToFrac realYSt) (displayStackSize unit)
  where
    (realX, realY) = currentConversion (x, y)

    (realXSt, realYSt) = (xSt - (hexSide/2) * float2Double cellPlayerDirection, ySt)
    (realXHP, realYHP) = (realXSt + 24, realYSt)

    (xSt, ySt) = currentConversion(x + float2Int cellPlayerDirection, y)

    cellPlayerDirection = case getType (getPlayer (getUnitState unit)) of
        LeftPlayer -> 1
        RightPlayer -> -1

-- | Display amount of units on cell
displayStackSize 
  :: Unit       -- | Unit to draw its amount
  -> Picture
displayStackSize unit = color borderColor (rectangleSolid 27 12) 
  <> color stackColor (rectangleSolid 25 10)
  <> translate (-2.5 * int2Float (length stackToShow)) 
    (-3.75) (color white (scale 0.07 0.07 (text stackToShow)))
  where
    stackToShow = show (getStackSize (getUnitState unit))

-- | Get picture for given unit
getUnitPicture 
  :: Unit                     -- Unit to find picture for             
  -> [(UnitType, Picture)]    -- List of sprites for units
  -> Picture
getUnitPicture (Unit unitType _unitState) assets =
  translate (0) (double2Float hexSide/2) (fromMaybe blank (lookup unitType assets))

-- | Get picture for unit's selected state
getSelectedUnitPicture 
  :: Unit                       -- | Unit to find sprite for
  -> [(UnitType, Picture)]      -- | List of sprites for units
  -> Picture                  
getSelectedUnitPicture (Unit unitType _unitState) assets = 
  translate (0) (double2Float hexSide/2) (fromMaybe blank (lookup unitType assets))

-- | Get path to sprite with given name
getImagePath 
  :: String         -- | Name of sprite
  -> IO [Char]      -- | Path to sprite
getImagePath str = do
  let me = "sprites/" ++ str ++ ".png"
  return me

-- | Extract sprite for given sprite name
loadImage
  :: String                     -- | Sprite name
  -> IO (Maybe DynamicImage)
loadImage name = do
  path <- getImagePath name
  img <- readImage path
  return $ case img of
    Left _ -> Nothing
    Right img -> Just img

-- | Convert dynamic image to drawable one
convertImage 
  :: DynamicImage 
  -> Picture
convertImage = fromImageRGBA8 . convertRGBA8

-- | Get sprite that corresponds to given name
getImage 
  :: String       -- | Sprite name
  -> IO Picture
getImage name = do
    processImage <$> loadImage name
  where
    processImage = maybe (color yellow (circleSolid 10)) convertImage

-- | Draw unit stats
displayStats
  :: Picture    -- | Stats backgrou
  -> Unit       -- | Unit to draw his stats
  -> [(UnitType, Picture)]
  -> Picture
displayStats initBg unit assets = bg <> stats <> renderSprite
  where
    state = getUnitState unit
    props = getProps state

    atk = "Attack " ++ show (getAttackPoints props)
    ammo = "Ammo " ++ show (getCurrentAmmo state)
    def = "Defense " ++ show (getDefensePoints props)
    curHp = "Current health " ++ show (getHealthOfLast state)
    hp = "Health " ++ show (getHealth props)
    damage = "Damage " ++ show (head (getDamage props)) ++ 
      "-" ++ show (last (getDamage props))
    speed = "Speed " ++ show (getSpeed props)

    statsList = [atk, def, ammo, damage, hp, curHp, speed]

    getTextPic txt = color white (scale 0.1 0.1 (text txt))

    renderListText [] _ = blank
    renderListText (p : ps) ind = 
      translate 25 ((-13) * fromIntegral ind) (getTextPic p) <>
        renderListText ps (ind + 1)
    
    renderSprite = translate 185 (-36) (scale 0.75 0.75 
      (fromMaybe blank ((lookup (getUnitType unit) assets))))
    
    bg = translate (115) ((-36)) (color borderColor 
      (rectangleSolid 232.5 108.1) <> (scale 0.65 0.4 initBg))
    stats = renderListText statsList 0
    