module Main where
import Graphics
import Graphics.Gloss
import Game
import GameHandler

window :: Display
window = InWindow "/DaniilNikulin" (1200, 800) (100, 100)

background :: Color
background = white

world :: State
world = Selected (GameState units firstPlayer sortedUnits) firstUnit
  where
    firstUnit = getFirstUnit sortedUnits
    firstPlayer = determineTheFirst sortedUnits
    sortedUnits = sortUnits units

main :: IO ()
main = play window background 2 world renderState gameHandler timeHandler

units :: [Unit]
units = [
    createUnit Harpy player1 (0, 0) 1,
    createUnit Dwarf player1 (0, 1) 1,
    createUnit Archer player2 (1, 0) 1,
    createUnit Archer player2 (1, 1) 1
  ]

player1 :: Player
player1 = Player LeftPlayer

player2 :: Player
player2 = Player RightPlayer