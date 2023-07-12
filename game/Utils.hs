module Utils where

type DoubleCoords = (Double, Double)
type CellCoords = (Int, Int)
type Offset = (Double, Double)
type FieldSize = (Int, Int)
data CellPart = UR | UL | L | DL | DR | R 

offset :: Offset
offset = (-20, -20)

hexSide :: Double
hexSide = 24

-- | Field coordinates
type Coords = (Int, Int)

getPath
  :: Coords -- | From
  -> Coords -- | To
  -> [Coords]
getPath (x1, y1) (x2, y2) =
  map (\i -> (i, y1)) (getPath' x1 x2) ++ map (\i -> (i, x2)) (getPath' y1 y2)
  where
    getPath' :: Int -> Int -> [Int]
    getPath' x1' x2'
      | x1' == x2' = []
      | x2' > x1'  = [x1'..x2']
      | otherwise  = reverse [x2'..x1']

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace xOld xNew (x:xs) 
  | xOld == x = xNew : xs
  | otherwise = x : replace xOld xNew xs  

sign :: Int -> Int
sign num
  | num > 0   = 1
  | num < 0   = -1
  | otherwise = 0
