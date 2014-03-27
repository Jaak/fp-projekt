module Field (Cell(..), Field, emptyField, getCell,
              surroundingCoords, surroundingCells, showField, update)
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

surroundingCoords :: Coord -> [Coord]
surroundingCoords = undefined

surroundingCells :: Field -> Coord -> [(Coord, Cell)]
surroundingCells = undefined

showField :: Field -> String
showField = undefined

update :: [(Coord, Cell)] -> Field -> Field
update = undefined
