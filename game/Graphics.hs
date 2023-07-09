{-# LANGUAGE BlockArguments #-}
module Graphics where
import Graphics.Gloss
import GameHandler
import GameInternal
import Utils
--offset :: Offset
--offset = (1.6, 2.5)
--hexSide :: Double
--hexSide = 2.4
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
      upRight = (realToFrac (x + sqrtSide), realToFrac(y + halfSide))
      downRight = (realToFrac (x + sqrtSide), realToFrac(y - halfSide))
      down = (realToFrac x, realToFrac(y-side))
      downLeft = (realToFrac (x - sqrtSide), realToFrac(y - halfSide))
      upLeft = (realToFrac(x - sqrtSide), realToFrac(y + halfSide))

--type FieldSize = (Int, Int)
type Offset = (Double, Double)
--type CellCoords = (Int, Int)
type Field = Picture
--type DoubleCoords = (Double, Double)
data CellPart = UR | UL | L | DL | DR | R 


-- Constants
sizeOfField = (15, 11)

-- RENDERERS
-- We are rendering the whole situation right here.
renderState :: State -> Picture
renderState (NoSelected (GameState units _turn)) = renderField units
renderState (Selected (GameState units turn) unit) = renderSelection (GameState units turn) unit <> renderField units

renderSelection :: GameState -> Unit -> Picture
renderSelection gameState unit = pictures (map selectedCell (getCellsToMove unit gameState)) <> selectedCell (getUnitCoords unit)

selectedCell :: CellCoords -> Picture
selectedCell coords = color (greyN 0.5) (drawCell coords polygon)
renderField :: [Unit] -> Picture
renderField units = drawUnits units <> drawField (0, 0)

drawField :: CellCoords -> Picture
drawField (x, y)
  | (x == xF && y == yF) = drawCell (x, y) lineLoop
  | (x == xF) = drawField (0, y+1) <> drawCell (x, y) lineLoop
  | otherwise = drawField (x+1, y) <> drawCell (x, y) lineLoop
  where
    (xF, yF) = sizeOfField

drawCell :: CellCoords -> (Path -> Picture) -> Field
drawCell cellCoords drawFunc = drawFunc (makeHexagonDotSet cellCoords hexSide)

drawUnits :: [Unit] -> Picture
drawUnits units = pictures (map drawUnit units)

drawUnit :: Unit -> Picture
drawUnit unit = renderUnit (getUnitCoords unit) unit

renderUnit :: CellCoords -> Unit -> Picture
renderUnit (x, y) unit = translate (realToFrac realX) (realToFrac realY) (getUnitPicture unit)
  where
    (realX, realY) = currentConversion (x, y)

getUnitPicture :: Unit -> Picture
getUnitPicture (Unit unitType _unitState) =
  case unitType of 
  Archer -> color red (circleSolid 10)
  Pikeman -> color red (rectangleSolid 10 10)
  Swordsman -> color red (rectangleSolid 10 15)
  Monk -> color red (circleSolid 15)
  Dwarf -> color green (rectangleSolid 10 10)
  _ -> blank