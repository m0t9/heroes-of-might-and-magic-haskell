{-# LANGUAGE BlockArguments #-}
module Graphics where
import Graphics.Gloss
import GameHandler
import Game
import Utils
import Codec.Picture ( convertRGBA8, readImage, DynamicImage )
import Graphics.Gloss.Juicy

-- | Colors
leftPlayerFieldColor :: Color
leftPlayerFieldColor = makeColor 0.529 1 0.616 0.6
rightPlayerFieldColor :: Color
rightPlayerFieldColor = makeColor 1 0.529 0.616 0.6
selectedCellColor :: Color
selectedCellColor = makeColor 255 255 255 0.15
selectedCellColorDefault :: Color
selectedCellColorDefault = makeColor 255 255 255 0.1

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
renderState :: Picture -> State -> Picture
--renderState (NoSelected (GameState units _turn _queue)) = renderField units
renderState background (Selected (GameState units turn _queue _r) unit) = background <> (renderSelection (GameState units turn _queue _r) unit <> renderField units <> selectedCellUnit unit)
renderState background (Moving state _unit _coords _animation) = background <> (renderField units)
  where
    (GameState units _turn _queue _r) = state

renderSelection :: GameState -> Unit -> Picture
renderSelection gameState unit = pictures (map selectedCell (getCellsToMove unit gameState))

selectedCell :: CellCoords -> Picture
selectedCell coords = color selectedCellColor (drawCell coords polygon)

selectedCellUnit :: Unit -> Picture
selectedCellUnit unit = color (greyN 0.3) (drawCell coords polygon) <> renderUnit coords unit getSelectedUnitPicture <> renderUnit coords unit getUnitPicture
  where
    coords = getUnitCoords unit

renderField :: [Unit] -> Picture
renderField units = drawUnits units <> drawField (0, 0)

drawField :: CellCoords -> Picture
drawField (x, y)
  | (x == xF - 1 && y == yF - 1) = color selectedCellColorDefault (drawCell (x, y) polygon) <> color black (drawCell (x, y) lineLoop)
  | (x == xF - 1) = drawField (0, y+1) <> color selectedCellColorDefault (drawCell (x, y) polygon) <> color black (drawCell (x, y) lineLoop)
  | otherwise = drawField (x+1, y) <> color selectedCellColorDefault (drawCell (x, y) polygon) <> color black (drawCell (x, y) lineLoop)
  where
    (xF, yF) = fieldSize

drawCell :: CellCoords -> (Path -> Picture) -> Field
drawCell cellCoords drawFunc = drawFunc (makeHexagonDotSet cellCoords hexSide)

drawUnits :: [Unit] -> Picture
drawUnits units = pictures (map drawUnit units)

drawUnit :: Unit -> Picture
drawUnit unit = renderUnit (getUnitCoords unit) unit getUnitPicture

renderUnit :: CellCoords -> Unit -> (Unit -> Picture) -> Picture
renderUnit (x, y) unit renderer = cell <> translate (realToFrac realX) (realToFrac realY) (renderer unit)
  where
    (realX, realY) = currentConversion (x, y)
    cellPlayerColor = case getType (getPlayer (getUnitState unit)) of 
        LeftPlayer -> leftPlayerFieldColor
        RightPlayer -> rightPlayerFieldColor

    cell = color cellPlayerColor (drawCell (x, y) polygon)

getUnitPicture :: Unit -> Picture
getUnitPicture (Unit unitType _unitState) =
  case unitType of
    -- ||| Castle fraction
    Archer -> color red (circleSolid 10)
    Pikeman -> color red (rectangleSolid 10 10)
    Swordsman -> color red (rectangleSolid 10 15)
    Monk -> color red (circleSolid 15)
    -- ||| Rampart fraction
    Dwarf -> color green (rectangleSolid 10 10)
    -- ||| Dungeon fraction
    Harpy -> color blue (circleSolid 10)
    _ -> blank

getSelectedUnitPicture :: Unit -> Picture
getSelectedUnitPicture (Unit unitType _unitState) =
  case unitType of
    Archer -> color yellow (circleSolid 12)
    Pikeman -> color yellow (rectangleSolid 12 12)
    Swordsman -> color yellow (rectangleSolid 12 17)
    Monk -> color yellow (circleSolid 17)
    Dwarf -> color yellow (rectangleSolid 12 12)
    Harpy -> color yellow (circleSolid 12)
    _ -> blank

loadBackground :: IO (Maybe DynamicImage)
loadBackground = do
  img <- readImage "sprites/background.png"
  return $ case img of
    Left _ -> Nothing
    Right img -> Just img

convertBackground :: DynamicImage -> Picture
convertBackground = fromImageRGBA8 . convertRGBA8

getBackground :: IO Picture
getBackground = do
    processImage <$> loadBackground
  where
    processImage = maybe (color yellow (circleSolid 10)) convertBackground