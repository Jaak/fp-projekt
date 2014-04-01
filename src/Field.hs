module Field (Cell(..), Field, emptyField, getCell, setCell, setCells,
              surroundingCoords, surroundingCells, showField)
  where

import Prot

data Cell
  = Closed
  | Open Int
  | Flagged

type Field = ()

emptyField :: Int -> Int -> Int -> Field
emptyField = undefined

getCell :: Field -> Coord -> Maybe Cell
getCell = undefined

setCell :: Coord -> Cell -> Field -> Field
setCell = undefined

setCells :: [(Coord, Cell)] -> Field -> Field
setCells = undefined

surroundingCoords :: Coord -> [Coord]
surroundingCoords = undefined

surroundingCells :: Field -> Coord -> [(Coord, Cell)]
surroundingCells = undefined

showField :: Field -> String
showField = undefined