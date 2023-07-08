module Main where
import Graphics
import Graphics.Gloss
window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)
background :: Color
background = white
sizeOfField = (3, 4)
main :: IO ()
main = display window background (drawField (0, 0) sizeOfField)
