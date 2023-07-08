module Utils where


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

sign :: Int -> Int
sign num
  | num > 0   = 1
  | num < 0   = -1
  | otherwise = 0
