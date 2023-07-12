module Main where
import Graphics
import Graphics.Gloss
import Game
import GameHandler
import Random (JavaRandom(JavaRandom))

window :: Display
window = InWindow "/DaniilNikulin" (1200, 800) (100, 100)

world :: State
world = Selected (GameState units firstPlayer sortedUnits 0) firstUnit
  where
    firstUnit = getFirstUnit sortedUnits
    firstPlayer = determineTheFirst sortedUnits
    sortedUnits = sortUnits units

main :: IO ()
main = do
  background <- getBackground
  play window white 3 world (renderState background) gameHandler timeHandler

units :: [Unit]
units = [
    createUnit Harpy player1 (4, 4) 10,
    createUnit Harpy player2 (0, 0) 10,
    createUnit Dwarf player1 (4, 5) 10,
    createUnit Dwarf player2 (1, 0) 10,
    createUnit Archer player1 (5, 4) 10,
    createUnit Archer player2 (0, 1) 10
  ]

player1 :: Player
player1 = Player LeftPlayer

player2 :: Player
player2 = Player RightPlayer