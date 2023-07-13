module Main where
import Graphics
import Graphics.Gloss
import Game
import GameHandler

window :: Display
window = InWindow "/DaniilNikulin" (1024, 712) (100, 100)

world :: State
world = Selected (GameState units firstPlayer sortedUnits 0) firstUnit Nothing
  where
    firstUnit = getFirstUnit sortedUnits
    firstPlayer = determineTheFirst sortedUnits
    sortedUnits = sortUnits units


main :: IO ()
main = do
  background <- getImage "background"
  statsBackground <- getImage "stats"
  getArcher <- getImage "archer"
  getPikeman <- getImage "pikeman"
  getSwordsman <- getImage "swordsman"
  getMonk <- getImage "monk"
  --
  getDwarf <- getImage "dwarf"
  getWoodelf <- getImage "woodelf"
  getDenroidGuard <- getImage "dendroidguard"
  --
  getTroglodyte <- getImage "troglodyte"
  getHarpy <- getImage "harpy"
  getBeholder <- getImage "beholder"
  getMinotaur <- getImage "minotaur"
  getGameover <- getImage "gameover"
  getLeftPlayer <- getImage "leftplayer"
  getRightPlayer <- getImage "rightplayer"
  getStats <- getImage "stats"
  let assets = [
        (Archer, getArcher),
        (Pikeman, getPikeman),
        (Swordsman, getSwordsman),
        (Monk, getMonk),
        (Dwarf, getDwarf), 
        (WoodElf, getWoodelf), 
        (DenroidGuard, getDenroidGuard), 
        (Troglodyte, getTroglodyte),
        (Harpy, getHarpy),
        (Beholder, getBeholder),
        (Minotaur, getMinotaur)]
  let assetsScreen = [
        ("gameover", getGameover),
        ("leftplayer", getLeftPlayer),
        ("rightplayer", getRightPlayer),
        ("stats", getStats)]
  play window white 10 world (renderState background assets assetsScreen) gameHandler timeHandler


units :: [Unit]
units = [
    createUnit Pikeman player1 (0, 0) 80,
    createUnit Archer player1 (0, 1) 10,
    createUnit Swordsman player1 (0, 2) 15,
    createUnit Monk player1 (0, 3) 10,
    createUnit Dwarf player1 (0, 4) 33,
    createUnit WoodElf player1 (0, 5) 25,
    createUnit DenroidGuard player1 (0, 6) 23,
    createUnit Troglodyte player1 (0, 7) 1,
    createUnit Harpy player1 (0, 8) 20,
    createUnit Beholder player1 (0, 9) 29,
    createUnit Minotaur player1 (0, 10) 10,

    createUnit Pikeman player2 (14, 0) 80,
    createUnit Archer player2 (14, 1) 10,
    createUnit Swordsman player2 (14, 2) 15,
    createUnit Monk player2 (14, 3) 10,
    createUnit Dwarf player2 (14, 4) 33,
    createUnit WoodElf player2 (14, 5) 25,
    createUnit DenroidGuard player2 (14, 6) 23,
    createUnit Troglodyte player2 (14, 7) 1,
    createUnit Harpy player2 (14, 8) 20,
    createUnit Beholder player2 (14, 9) 29,
    createUnit Minotaur player2 (14, 10) 10
  ]

player1 :: Player
player1 = Player LeftPlayer

player2 :: Player
player2 = Player RightPlayer