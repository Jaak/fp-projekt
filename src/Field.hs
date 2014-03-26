{-# LANGUAGE BangPatterns #-}
module Field
    (ClickResult(..), Field, clicks, showField, randomField)
  where

import Prot

import System.Random
import Data.Array
import Data.Maybe
import Data.Char (intToDigit)


data C = C {-# UNPACK #-} !Int {-# UNPACK #-} !Int
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
    coordArray :: Array C Cell,
    numMines   :: Int,
    closedLeft :: Int,
    flagsLeft  :: Int
  }

getCells :: Field -> [C] -> [(C, Cell)]
getCells field = mapMaybe (\coord -> (,) coord `fmap` getCell field coord)

getCell :: Field -> C -> Maybe Cell
getCell field coord
  | inRange (bounds arr) coord = Just (arr ! coord)
  | otherwise = Nothing
  where
    arr = coordArray field

setCells :: [(C, Cell)] -> Field -> Field
setCells xs field = field { coordArray = coordArray field // xs }

surr :: Field -> C -> [(C, Cell)]
surr field !coord = loop (getSurr coord)
  where
    arr = coordArray field
    bnds = bounds arr
    loop [] = []
    loop (!x : xs)
      | inRange bnds x = let !cell = arr ! x
	                     !p = (x, cell)
                           in p : loop xs
      | otherwise = loop xs

getSurr :: C -> [C]
getSurr (C h w) = [C (h-1) (w-1), C (h-1) w, C (h-1) (w+1),
                   C h (w-1),     C h w,     C h (w+1),
                   C (h+1) (w-1), C (h+1) w, C (h+1) (w+1)]
{-# INLINE getSurr #-}

data ClickResult
  = GameFinished GameOutcome
  | GameContinue Field [Open]

clicks :: [Coord] -> Field -> ClickResult
clicks coords field = undefined

showField :: Field -> String
showField field = showsField field ""

showFieldShort :: Field -> String
showFieldShort field = [showCell x y | x <- [0 .. w-1], y <- [0 .. h-1]]
  where
    (_, C w h) = bounds (coordArray field)
    showCell x y = case getCell field (C x y) of
      Just cell -> case cellState cell of
        CellOpen -> intToDigit (surrMines cell)
        CellFlagged -> 'F'
        CellClosed -> '?'

showsField :: Field -> ShowS
showsField field =
  shows (flagsLeft field) . showChar '\n' .
  shows (closedLeft field) . showChar '\n' .
  showLine [showLine [showChar ' ' . c (C i j) . showChar ' ' | j <- [-1..w]] | i <- [-1..h]]
  where
    (_, C h w) = bounds (coordArray field)
    showLine [] = showChar '\n'
    showLine (x : xs) = x . showLine xs
    c coord = case getCell field coord of
      Nothing -> showChar '#'
      Just cell -> case cellState cell of
        CellOpen -> if hasMine cell
          then showChar '*'
          else shows (surrMines cell)
        CellFlagged -> showChar 'F'
        CellClosed -> showChar '?'

randomField :: Int -> Int -> Int -> StdGen -> Field
randomField h w n = undefined