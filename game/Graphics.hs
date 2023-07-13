{-# LANGUAGE BlockArguments #-}
module Graphics where
import Graphics.Gloss
import GameHandler
import Game
import Utils
import Codec.Picture ( convertRGBA8, readImage, DynamicImage )
import Graphics.Gloss.Juicy
import Data.Maybe
import GHC.Float (double2Float)

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

currentConversion :: CellCoords -> DoubleCoords
currentConversion hexCoords = hexToCoords offset hexCoords hexSide

makeHexagonDotSet :: CellCoords -> Double -> [(Float, Float)]
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

type Offset = (Double, Double)
type Field = Picture
data CellPart = UR | UL | L | DL | DR | R

-- RENDERERS
-- We are rendering the whole situation right here.
renderState :: Picture -> [(UnitType, Picture)] -> State -> Picture
--renderState (NoSelected (GameState units _turn _queue)) = renderField units
renderState background assets (Selected (GameState units turn _queue _r) unit) = 
  background <> (renderSelection (GameState units turn _queue _r) unit <> 
  selectedCellUnit unit assets <> renderField units assets)
renderState background assets (Moving state _unit _coords _animation) = 
  background <> renderField units assets
  where
    (GameState units _turn _queue _r) = state
renderState background assets (AttackMoving state _damager _coords _victim _d _animation _param) = 
  background <> renderField units assets
  where
    (GameState units _turn _queue _r) = state    
renderState background assets (Attacking state _damager _coords _victim _d _param) = 
  background <> renderField units assets
  where
    (GameState units _turn _queue _r) = state      
renderState background assets (CounterAttacking state _damager _postDamager _d _param) = 
  background <> renderField units assets
  where
    (GameState units _turn _queue _r) = state    
renderState background assets (PostAttacking state _post _param) = 
  background <> renderField units assets
  where
    (GameState units _turn _queue _r) = state       

renderSelection :: GameState -> Unit -> Picture
renderSelection gameState unit = pictures (map selectedCell (getCellsToMove unit gameState))

selectedCell :: CellCoords -> Picture
selectedCell coords = color selectedCellColor (drawCell coords polygon)

selectedCellUnit :: Unit -> [(UnitType, Picture)] -> Picture
selectedCellUnit unit assets = color chosenUnitCellColor (drawCell coords polygon) <> 
  renderUnit coords unit getSelectedUnitPicture assets <> 
  renderUnit coords unit getUnitPicture assets
  where
    coords = getUnitCoords unit

renderField :: [Unit] -> [(UnitType, Picture)] -> Picture
renderField units assets = drawField (0, 0) <> drawUnitCells units <> drawUnits units assets

drawField :: CellCoords -> Picture
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

drawCell :: CellCoords -> (Path -> Picture) -> Field
drawCell cellCoords drawFunc = drawFunc (makeHexagonDotSet cellCoords hexSide)

drawUnits :: [Unit] -> [(UnitType, Picture)] -> Picture
drawUnits units assets = pictures (map (drawUnit assets) units)

drawUnit :: [(UnitType, Picture)] -> Unit -> Picture
drawUnit assets unit = renderUnit (getUnitCoords unit) unit getUnitPicture assets

drawUnitCells :: [Unit] -> Picture
drawUnitCells units = pictures (map unitCell units)
  where
    unitCell unit = color cellPlayerColor (drawCell (x, y) polygon)
      where
        cellPlayerColor = case getType (getPlayer (getUnitState unit)) of
          LeftPlayer -> leftPlayerFieldColor
          RightPlayer -> rightPlayerFieldColor
        (x, y) = getCoords $ getUnitState unit

renderUnit :: CellCoords -> Unit -> (Unit -> [(UnitType, Picture)] -> Picture) -> [(UnitType, Picture)] -> Picture
renderUnit (x, y) unit renderer assets = 
  translate (realToFrac realX) (realToFrac realY) (scale cellPlayerDirection 1 (renderer unit assets))
  where
    (realX, realY) = currentConversion (x, y)
    cellPlayerDirection = case getType (getPlayer (getUnitState unit)) of
        LeftPlayer -> 1
        RightPlayer -> -1

getUnitPicture :: Unit -> [(UnitType, Picture)] -> Picture
getUnitPicture (Unit unitType _unitState) assets = 
  translate (0) (double2Float hexSide/2) (fromMaybe blank (lookup unitType assets))

getSelectedUnitPicture :: Unit -> [(UnitType, Picture)] -> Picture
getSelectedUnitPicture (Unit unitType _unitState) assets = 
  translate (0) (double2Float hexSide/2) (fromMaybe blank (lookup unitType assets))

getImagePath :: String -> IO [Char]
getImagePath str = do
  let me = "sprites/" ++ str ++ ".png"
  return me

loadImage :: String -> IO (Maybe DynamicImage)
loadImage name = do
  path <- getImagePath name
  img <- readImage path
  return $ case img of
    Left _ -> Nothing
    Right img -> Just img

convertImage :: DynamicImage -> Picture
convertImage = fromImageRGBA8 . convertRGBA8

getImage :: String -> IO Picture
getImage name = do
    processImage <$> loadImage name
  where
    processImage = maybe (color yellow (circleSolid 10)) convertImage