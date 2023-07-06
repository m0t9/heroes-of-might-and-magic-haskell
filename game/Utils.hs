module Utils where

getPath :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getPath (x1, y1) (x2, y2) =
  map (, y1) (getPath' x1 x2) ++ map (, x2) (getPath' y1 y2)
  where
    getPath' :: Int -> Int -> [Int]
    getPath' x1' x2'
      | x1' == x2' = []
      | x2' > x1'  = [x1'..x2']
      | otherwise  = reverse [x2'..x1']
