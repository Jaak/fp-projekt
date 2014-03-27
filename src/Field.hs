{-# LANGUAGE BangPatterns #-}
module Field
    (ClickResult(..), Field, clicks, showField, readField, randomField)
  where

import Prot

import Control.Monad (guard)
import Data.Array
import Data.Char (intToDigit)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Set as S
import System.Random


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

data Field = Field {
    _field_cells       :: !(Array C Cell),
    _field_num_mines   :: !Int,
    _field_closed_left :: !Int
  }

surrounding :: C -> [C]
surrounding (C x y) = [C (x-1) (y-1), C (x-1) y, C (x-1) (y+1),
                       C x     (y-1),            C x     (y+1),
                       C (x+1) (y-1), C (x+1) y, C (x+1) (y+1)]
{-# INLINE surrounding #-}

data ClickResult
  = GameFinished GameOutcome
  | GameContinue Field [Open]

clicks :: [Coord] -> Field -> ClickResult
clicks coords field
  | any isBadCell cs = GameFinished Loss
  | numOpened == _field_closed_left field = GameFinished Win
  | otherwise = GameContinue updatedField (zipWith mkOpen cs' newCells)
  where
    arr = _field_cells field
    bnds = bounds arr
    isBadCell c = not (inRange bnds c) || _cell_has_mine (arr ! c)

    cs = map (uncurry C) coords
    cs' = expandCoords S.empty cs
    numOpened = length cs'

    newCells = [openCell (arr ! c) | c <- cs']
    updatedField = field {
      _field_closed_left = _field_closed_left field - numOpened,
      _field_cells = arr // zip cs' newCells
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

showField :: Field -> String
showField field = [showCell x y | y <- [h - 1, h - 2 .. 0], x <- [0 .. w-1]]
  where
    (_, C w h) = bounds (_field_cells field)
    showCell x y = case _cell_state cell of
      CellOpen -> intToDigit (_cell_surr cell)
      CellFlagged -> 'F'
      CellClosed -> '?'
      where cell = _field_cells field ! C x y

computeSurrCounts :: Field -> Field
computeSurrCounts field = field { _field_cells = arr // update }
  where
    arr = _field_cells field
    update = [(c, updateSurr c) | c <- indices arr]
    updateSurr c = (arr ! c) {
      _cell_surr = length [() | c' <- surrounding c, inRange (bounds arr) c', _cell_has_mine (arr ! c')]
    }

{--
 -- Reading predefined field, the format is as follows:
 -- <heigh> <width> <mines> <opens>
 -- where height and width are integers and mines and opens are sequences of 0s and 1s.
 --}
readField :: String -> Maybe Field
readField inp = case readsField inp of
  (x, _) :_ -> Just x
  _         -> Nothing

readsField :: ReadS Field
readsField s0 = do
  (h, s1) <- reads s0
  (w, s2) <- reads s1
  (mines, s3) <- reads01 s2
  (opens, s4) <- reads01 s3
  return (mkField h w mines opens, s4)

reads01 :: ReadS [Int]
reads01 s0 = do
  (n, s1) <- reads s0
  guard (n == 0 || n == 1)
  (ns, s2) <- reads01 s1
  return (n : ns, s2)

mkField :: Int -> Int -> [Int] -> [Int] -> Field
mkField h w mines opens = computeSurrCounts (Field arr n n)
  where
    n = sum mines
    cells = zipWith mkCell mines opens
    arr = listArray (C 0 0, C (w - 1) (h - 1)) cells
    mkCell hasMine isOpen = Cell 0 (if isOpen == 1 then CellOpen else CellClosed) (toEnum hasMine)

{-
 - Generate random field
 -}

shuffle :: [a] -> StdGen -> [a]
shuffle xs gen = map snd . sortBy (comparing fst) $ zip ns xs
  where ns = randoms gen :: [Int]

randomBools :: Int -> Int -> StdGen -> [Bool]
randomBools n k = shuffle (take n $ replicate k True ++ repeat False)

randomField :: Int -> Int -> Int -> StdGen -> Field
randomField h w n gen = computeSurrCounts (Field arr n n)
  where
    arr = listArray (C 0 0, C (w - 1) (h - 1)) initCell
    initCell = map mkCell $ randomBools (h*w) n gen
    mkCell = Cell 0 CellClosed