module Prot where

data GameConf = GameConf {
    _height    :: Int,
    _width     :: Int,
    _num_mines :: Int
  }
  deriving (Show, Read)

-- Initial board is encoded in a string.
-- ?      denotes closed cell (it's unknown)
-- F or * denotes cell that has mine under it
-- 0-8    denotes open cell and the number of mines next to it
-- all other characters are ignored
type InitialBoard = String

-- (x, y) coordinate
type Coord = (Int, Int)

data Open = Open Coord Int
  deriving (Show, Read)

data GameOutcome = Win | Loss
  deriving (Show, Read)

data ClientMessage = Clicks [Coord]
  deriving (Show, Read)

data ServerMessage
  = NewGame GameConf InitialBoard
  | Opened [Open]
  | GameOver GameOutcome
  | End
  deriving (Show, Read)