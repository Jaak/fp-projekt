{-# LANGUAGE BangPatterns #-}
module Field
    (ClickResult(..), Field, clicks, showField, randomField)
  where

import Prot

import Data.Array
import Data.Char (intToDigit)
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
    surrMines :: !Int,
    cellState :: !CellState,
    hasMine   :: !Bool
  }

data Field = Field {
    coordArray :: !(Array C Cell),
    numMines   :: !Int,
    closedLeft :: !Int
  }

-- NB! (x, y) itself is excluded
surrounding :: C -> [C]
surrounding (C x y) = [C (x-1) (y-1), C (x-1) y, C (x-1) (y+1),
                       C x     (y-1),            C x     (y+1),
                       C (x+1) (y-1), C (x+1) y, C (x+1) (y+1)]
{-# INLINE surrounding #-}

data ClickResult
  = GameFinished GameOutcome
  | GameContinue Field [Open]

openCell :: Cell -> Cell
openCell cell = cell { cellState = CellOpen }

mkOpen :: C -> Cell -> Open
mkOpen (C x y) cell = Open (x, y) (surrMines cell)

clicks :: [Coord] -> Field -> ClickResult
clicks coords field
  | any isBadCell cs = GameFinished Loss
  | numOpened == closedLeft field = GameFinished Win
  | otherwise = GameContinue updatedField (zipWith mkOpen cs' newCells)
  where
    arr = coordArray field
    bnds = bounds arr
    isBadCell c = not (inRange bnds c) || hasMine (arr ! c)

    cs = map (uncurry C) coords
    cs' = expandCoords S.empty cs
    numOpened = length cs'

    newCells = [openCell (arr ! c) | c <- cs']
    updatedField = field {
      closedLeft = closedLeft field - numOpened,
      coordArray = arr // zip cs' newCells
    }

    expandCoords seen [] = S.elems seen
    expandCoords seen (i : is)
      | not (inRange bnds i) = expandCoords seen is
      | i `S.member` seen = expandCoords seen is
      | surrMines (arr ! i) == 0 = expandCoords seen' (surrounding i ++ is)
      | otherwise = expandCoords seen' is
      where seen' = S.insert i seen

showField :: Field -> String
showField field = [showCell x y | x <- [0 .. w-1], y <- [0 .. h-1]]
  where
    (_, C w h) = bounds (coordArray field)
    arr = coordArray field
    showCell x y = case cellState cell of
      CellOpen -> intToDigit (surrMines cell)
      CellFlagged -> 'F'
      CellClosed -> '?'
      where cell = arr ! C x y

randomField :: Int -> Int -> Int -> StdGen -> Field
randomField h w n = undefined