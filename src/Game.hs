module Game (ClickResult(..), Game, clicks, showGame, readGame, randomGame, getGameConf)
  where

import Common
import Prot

import Control.Monad (guard)
import Data.Array
import Data.Char (intToDigit)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Set as S
import System.Random

{--
 - This is internal to server.
 --}

data C = C
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  deriving (Show, Eq, Ord, Ix)

data Cell = Cell {
    _cell_surr     :: !Int,
    _cell_has_mine :: !Bool,
    _cell_is_open  :: !Bool
  }

data Game = Game {
    _game_cells       :: !(Array C Cell),
    _game_num_mines   :: !Int,
    _game_closed_left :: !Int
  }

getGameConf :: Game -> Prot.GameConf
getGameConf game = Prot.GameConf {
    _height = 1 + (y1 - y0),
    _width  = 1 + (x1 - x0) ,
    _num_mines = _game_num_mines game
  }
  where (C x0 y0, C x1 y1) = bounds (_game_cells game)

surrounding :: C -> [C]
surrounding (C x y) = [C (x-1) (y-1), C (x-1) y, C (x-1) (y+1),
                       C x     (y-1),            C x     (y+1),
                       C (x+1) (y-1), C (x+1) y, C (x+1) (y+1)]
{-# INLINE surrounding #-}

data ClickResult
  = GameFinished GameOutcome
  | GameContinue Game [Open]

clicks :: [Coord] -> Game -> ClickResult
clicks coords game
  | any isBadCell cs = GameFinished Loss
  | _game_closed_left newGame == _game_num_mines newGame = GameFinished Win
  | otherwise = GameContinue newGame (zipWith mkOpen cs' newCells)
  where
    arr = _game_cells game
    bnds = bounds arr
    isBadCell c = not (inRange bnds c) || _cell_has_mine (arr ! c)

    cs = map (uncurry C) coords
    cs' = expandCoords S.empty cs
    numOpened = length cs'

    newCells = [openCell (arr ! c) | c <- cs']
    newGame = game {
      _game_closed_left = _game_closed_left game - numOpened,
      _game_cells = arr // zip cs' newCells
    }

    openCell cell = cell { _cell_is_open = True }

    mkOpen (C x y) cell = Open (x, y) (_cell_surr cell)

    expandCoords seen [] = S.elems seen
    expandCoords seen (i : is)
      | not (inRange bnds i) = expandCoords seen is
      | i `S.member` seen = expandCoords seen is
      | _cell_is_open cell = expandCoords seen is
      | _cell_surr cell == 0 = expandCoords seen' (surrounding i ++ is)
      | otherwise = expandCoords seen' is
      where
        cell = arr ! i
        seen' = S.insert i seen

showGame :: Game -> String
showGame game = [showCell x y | y <- [y1, y1 - 1 .. y0], x <- [x0 .. x1]]
  where
    (C x0 y0, C x1 y1) = bounds (_game_cells game)
    showCell x y
      | not (_cell_is_open cell) = '?'
      | _cell_has_mine cell = 'F'
      | otherwise = intToDigit (_cell_surr cell)
      where cell = _game_cells game ! C x y

computeSurrCounts :: Game -> Game
computeSurrCounts game = game { _game_cells = arr // update }
  where
    arr = _game_cells game
    update = [(c, updateSurr c) | c <- indices arr]
    updateSurr c = (arr ! c) {
      _cell_surr = length [() | c' <- surrounding c, inRange (bounds arr) c', _cell_has_mine (arr ! c')]
    }

{--
 -- Reading predefined game, the format is as follows:
 -- <heigh> <width> <mines> <opens>
 -- where height and width are integers and mines and opens are sequences of 0s and 1s.
 --}
readGame :: String -> Maybe Game
readGame inp = do
    ns <- mapM maybeRead (words inp)
    guard (verifyInput ns)
    let ~(h : w : bs) = ns
    let ~(ms, os) = splitAt (h*w) bs
    return $ mkGame h w ms os

verify01 :: Int -> Bool
verify01 0 = True
verify01 1 = True
verify01 _ = False

verifyInput :: [Int] -> Bool
verifyInput (h : w : bs)
    | h <= 0 || w <= 0 = False
    | length bs /= 2*h*w = False
    | any (not.verify01) bs = False
    | otherwise = True
verifyInput _ = False

mkGame :: Int -> Int -> [Int] -> [Int] -> Game
mkGame h w mines opens = computeSurrCounts game
  where
    n = sum mines
    cells = zipWith mkCell mines opens
    coords = [C x y | y <- [h - 1, h - 2 .. 0], x <- [0 .. w - 1]]
    arr = array (C 0 0, C (w - 1) (h - 1)) (zip coords cells)
    mkCell hasMine isOpen = Cell {
      _cell_surr = 0,
      _cell_has_mine = toEnum hasMine,
      _cell_is_open = toEnum isOpen
    }

    game = Game {
      _game_cells = arr,
      _game_num_mines = n,
      _game_closed_left = h*w - sum opens
    }

{-
 - Generate random game
 -}

-- This is not perfect shuffle and it doesn't really matter!
shuffle :: [a] -> StdGen -> [a]
shuffle xs gen = map snd . sortBy (comparing fst) $ zip ns xs
  where ns = randoms gen :: [Int]

randomBools :: Int -> Int -> StdGen -> [Bool]
randomBools n k = shuffle (take n $ replicate k True ++ repeat False)

randomGame :: Int -> Int -> Int -> StdGen -> Game
randomGame h w n gen = computeSurrCounts game
  where
    arr = listArray (C 0 0, C (w - 1) (h - 1)) initCell
    initCell = map mkCell $ randomBools (h*w) n gen
    mkCell hasMine = Cell {
      _cell_surr = 0,
      _cell_has_mine = hasMine,
      _cell_is_open = False
    }

    game = Game {
      _game_cells = arr,
      _game_num_mines = n,
      _game_closed_left = h*w
    }
