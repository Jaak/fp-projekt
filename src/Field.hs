{-# LANGUAGE BangPatterns #-}
module Field 
    (clicks, flags, GameState(..), Field(gameState), countCorrect, buildField, showField)
  where

import Prot

import Data.List (foldl')
import Text.Printf
import Data.Array
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S

----
-- Hmm, looks like that unpacking fields makes things a lot more efficient. 
----

data C = C {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving Show

instance Eq C where
  C x y == C a b = x == a && y == b

instance Ord C where
  compare (C x y) (C a b) = case compare x a of
    LT -> LT
    EQ -> compare y b
    GT -> GT

instance Ix C where
  range (C x y, C a b) = [C i j | i <- [x..a], j <- [y..b]]
  inRange (C x y, C a b) (C i j) = x <= i && i <= a && y <= j && j <= b
  rangeSize (C x y, C a b) = (a - x + 1)*(b - y + 1)
  index (C x y, C a b) (C i j) = (b - y + 1)*(i - x) + j - y

----
-- Cells
----

data CellState
  = CellOpen
  | CellFlagged
  | CellClosed
  deriving (Eq, Ord, Show, Read)

data Cell = Cell {
    numMines  :: !Int,
    cellState :: !CellState,
    hasMine   :: !Bool
  }

isOpen :: Cell -> Bool
isOpen cell = case cellState cell of
  CellOpen -> True
  _ -> False

isClosed :: Cell -> Bool
isClosed cell = case cellState cell of
  CellClosed -> True
  _ -> False

isFlagged :: Cell -> Bool
isFlagged cell = case cellState cell of
  CellFlagged -> True
  _ -> False

countMines :: Cell -> Maybe Int
countMines cell = case cellState cell of
  CellOpen -> Just (numMines cell)
  _ -> Nothing

----
-- Field
----

data GameState
  = GameWon
  | GameDunno
  | GameLost
  deriving (Eq, Ord, Show, Read)

data Field = Field {
    coordArray :: Array C Cell,
    gameState  :: GameState,
    closedLeft :: Int,
    flagsLeft  :: Int
  }

getCells :: Field -> [C] -> [(C, Cell)]
getCells field = mapMaybe (\coord -> (,) coord `fmap` getCell field coord)

getCell :: Field -> C -> Maybe Cell
getCell field !coord
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

clicks :: [Coord] -> Field -> (Field, [Open])
clicks coords field = (updateState . updateSets $ setCells xs field, map mkOpen xs)
  where

    xs = mapMaybe openCell $ collect S.empty $ S.fromList $ map (uncurry C) coords

    mkOpen (C x y, cell) = Open (x, y) (numMines cell)

    openCell (coord, cell)
      | isClosed cell = Just (coord, cell { cellState = CellOpen })
      | otherwise = Nothing

    updateSets field = field {
	closedLeft = closedLeft field - length xs
      }

    updateState field
      | any (hasMine . snd) xs = field { gameState = GameLost }
      | closedLeft field == 0 && flagsLeft field == 0 = field { gameState = GameWon }
      | otherwise = field
    
    collect visited xs = case S.minView xs of
      Nothing -> []
      Just (x, xs)
	| S.member x visited -> collect visited xs
        | otherwise -> case getCell field x of
	    Nothing -> collect visited xs
	    Just cell -> let
		ys = if numMines cell == 0 then getSurr x else []
		visited' = S.insert x visited
	      in (x, cell) : collect visited' (foldr S.insert xs ys)

flags :: [Coord] ->  Field -> Field
flags coords field
  | length coords > n = error "ooops"
  | otherwise = updateState (setCells xs field')
  where
    xs = mapMaybe flagCell $ getCells field $ S.toList $ S.fromList $ map (uncurry C) coords
    n = length xs
    flagCell (coord, cell)
      | isClosed cell = Just (coord, cell { cellState = CellFlagged })
      | otherwise = Nothing
    updateState field
      | closedLeft field == 0 && flagsLeft field == 0 = field { gameState = GameWon }
      | otherwise = field
    field' = field {
	flagsLeft = flagsLeft field - n,
	closedLeft = closedLeft field - n
      }

showField :: Field -> ShowS
showField field = 
  shows (flagsLeft field) . showChar '\n' .
  shows (closedLeft field) . showChar '\n' . 
  showLine [showLine [showChar ' ' . c (C i j) . showChar ' ' | j <- [0..w+1]] | i <- [0..h+1]]
  where
    (_, C h w) = bounds (coordArray field)
    showLine [] = showChar '\n'
    showLine (x : xs) = x . showLine xs
    c coord = case getCell field coord of
      Nothing -> showChar '#'
      Just cell -> case cellState cell of
	CellOpen -> if hasMine cell
	  then showChar '*'
	  else shows (numMines cell)
	CellFlagged -> showChar 'F'
	CellClosed -> showChar ' '

buildField :: [[Int]] -> Int -> Int -> Int -> Maybe Field
buildField grid h w n
  | not valid = Nothing
  | otherwise = Just $ setCells (map update $ range bnds) field
  where

    field = Field {
      coordArray = array bnds cs,
      gameState = GameDunno,
      flagsLeft = n,
      closedLeft = h*w
    }

    bnds = (C 1 1, C h w)

    cs = zip (range bnds) (map mkCell $ concat grid)

    mkCell mine = Cell { cellState = CellClosed, hasMine = toEnum mine, numMines = 0 }
    
    valid = n == (sum . map fromEnum $ concat grid) &&
	    all (\n -> n == 1 || n == 0) (concat grid) &&
	    all (\row -> w == length row) grid &&
            h == length grid
    
    update x = let
	k = sum $ map (fromEnum . hasMine . snd) $ surr field x
	Just cell = getCell field x
      in (x, cell { numMines = k })

countCorrect :: Field -> Double
countCorrect = end . foldl' step (0, 0) . elems . coordArray
  where
    step (correct, count) cell = case cellState cell of
      CellOpen | not (hasMine cell) -> (correct + 1, count + 1)
      CellFlagged | hasMine cell -> (correct + 1, count + 1)
      _ -> (correct, count + 1)

    end (correct, 0) = error "This is NOT supposed to happen"
    end (correct, count) = fromIntegral correct / fromIntegral count
