module Main where
import Graphics
import Graphics.Gloss
window :: Display
window = InWindow "/DaniilNikulin" (200, 200) (10, 10)
background :: Color
background = white
sizeOfField = (15, 11)
main :: IO ()
main = display window background (drawField (0, 0) sizeOfField) --(polygon [(0, 0), (0, 1), (1, 1), (1, 0)])--
