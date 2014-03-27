module Game (ClickResult(..), Game, clicks, showGame, readGame, randomGame)
  where

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

data CellState
  = CellOpen
  | CellFlagged
  | CellClosed
  deriving (Eq, Ord, Show, Read)

data Cell = Cell {
    _cell_surr     :: !Int,
    _cell_state    :: !CellState,
    _cell_has_mine :: !Bool
  }

data Game = Game {
    _game_cells       :: !(Array C Cell),
    _game_num_mines   :: !Int,
    _game_closed_left :: !Int
  }

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
  | numOpened == _game_closed_left game = GameFinished Win
  | otherwise = GameContinue updateGame (zipWith mkOpen cs' newCells)
  where
    arr = _game_cells game
    bnds = bounds arr
    isBadCell c = not (inRange bnds c) || _cell_has_mine (arr ! c)

    cs = map (uncurry C) coords
    cs' = expandCoords S.empty cs
    numOpened = length cs'

    newCells = [openCell (arr ! c) | c <- cs']
    updateGame = game {
      _game_closed_left = _game_closed_left game - numOpened,
      _game_cells = arr // zip cs' newCells
    }

    openCell cell = cell { _cell_state = CellOpen }

    mkOpen (C x y) cell = Open (x, y) (_cell_surr cell)

    expandCoords seen [] = S.elems seen
    expandCoords seen (i : is)
      | not (inRange bnds i) = expandCoords seen is
      | i `S.member` seen = expandCoords seen is
      | _cell_surr (arr ! i) == 0 = expandCoords seen' (surrounding i ++ is)
      | otherwise = expandCoords seen' is
      where seen' = S.insert i seen

showGame :: Game -> String
showGame game = [showCell x y | y <- [h - 1, h - 2 .. 0], x <- [0 .. w-1]]
  where
    (_, C w h) = bounds (_game_cells game)
    showCell x y = case _cell_state cell of
      CellOpen -> intToDigit (_cell_surr cell)
      CellFlagged -> 'F'
      CellClosed -> '?'
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
readGame inp = case readsGame inp of
  (x, _) :_ -> Just x
  _         -> Nothing

readsGame :: ReadS Game
readsGame s0 = do
  (h, s1) <- reads s0
  (w, s2) <- reads s1
  (mines, s3) <- reads01s s2
  (opens, s4) <- reads01s s3
  return (mkGame h w mines opens, s4)

reads01s :: ReadS [Int]
reads01s s0 = do
  (n, s1) <- reads s0
  guard (n == 0 || n == 1)
  (ns, s2) <- reads01s s1
  return (n : ns, s2)

mkGame :: Int -> Int -> [Int] -> [Int] -> Game
mkGame h w mines opens = computeSurrCounts (Game arr n n)
  where
    n = sum mines
    cells = zipWith mkCell mines opens
    arr = listArray (C 0 0, C (w - 1) (h - 1)) cells
    mkCell hasMine isOpen = Cell 0 (if isOpen == 1 then CellOpen else CellClosed) (toEnum hasMine)

{-
 - Generate random game
 -}

shuffle :: [a] -> StdGen -> [a]
shuffle xs gen = map snd . sortBy (comparing fst) $ zip ns xs
  where ns = randoms gen :: [Int]

randomBools :: Int -> Int -> StdGen -> [Bool]
randomBools n k = shuffle (take n $ replicate k True ++ repeat False)

randomGame :: Int -> Int -> Int -> StdGen -> Game
randomGame h w n gen = computeSurrCounts (Game arr n n)
  where
    arr = listArray (C 0 0, C (w - 1) (h - 1)) initCell
    initCell = map mkCell $ randomBools (h*w) n gen
    mkCell = Cell 0 CellClosed