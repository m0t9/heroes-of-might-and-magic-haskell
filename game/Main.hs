module Main where
import Graphics
import Graphics.Gloss
import GameInternal
import GameHandler
window :: Display
window = InWindow "/DaniilNikulin" (1200, 800) (100, 100)
background :: Color
background = white
world :: State
world = NoSelected (GameState units player)
main :: IO ()
--main = display window background (drawField (0, 0) units) --(polygon [(0, 0), (0, 1), (1, 1), (1, 0)])--
main = play window background 1 world renderState gameHandler timeHandler

units :: [Unit]
units = [Unit Archer (UnitState (getInitialProps Archer) player (1, 1) 0 0 0), Unit Archer (UnitState (getInitialProps Archer) player (0, 0) 0 0 0), Unit Archer (UnitState (getInitialProps Archer) player (1, 0) 0 0 0), Unit Archer (UnitState (getInitialProps Archer) player (0, 1) 0 0 0)]

player :: Player
player = Player True

timeHandler :: (Float -> world -> world)
timeHandler _dt wrld = wrld 