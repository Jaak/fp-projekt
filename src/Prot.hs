module Prot 
  where

----
-- Kliendi nimi
type Name = String

----
-- Kliendi poolt genereeritud miinivälja esitus.
-- Iga tabeli väärtus peab kas olema 1 või 0 mis tähistavad
-- vastavalt kas antud positsioonis asub miin või mitte.
type Board = [[Int]]

----
-- Koordinaat.
-- Esimene komponent määrab tabeli rea (kõrgused) ning teine veeru (laiuse).
-- Ülemine vasak = (1, 1)
-- Parem alumine = (height, width)
type Coord = (Int, Int)

----
-- Avatud koordinaar. Peale positsiooni teame nüüd ka kui palju miine antud
-- kordinaadi ümbruses leidub.
data Open = Open Coord Int
  deriving (Show, Read)

----
-- Mängu konfiguratsioon.
data GameConf = GameConf {
    height :: Int,
    width :: Int,
    nMines :: Int
  }
  deriving (Show, Read)

----
-- Mängu tulemus.
-- Loodetavasti viike on võimalikult vähe.
data GameOutcome
  = Win
  | Tie
  | Loss
  deriving (Show, Read)

----
-- Turnriiri tulemus.
-- Hetkel salvestame siin ainult saavutatud positsiooni. Selle andmetüübi
-- väljad võivad tulevikus muutuda.
data Statistics = Statistics {
    position :: (Int, Int)
  }
  deriving (Show, Read)

----
-- Sõnumid mida klient võib serverile saata.
data ClientMessage
  = Hello Name
  | Games [Board] -- Games <viiskümmend mängu>
  | Clicks [Coord] [Coord] -- Clicks <vasakud klõpsud> <paremad klõpsud>
  deriving (Show, Read)

{-
 - Kliendi poolt saadetavate sõnumite järjekord peaks välja
 - nägema järgmine:
 -
 - Hello -> (Games -> Clicks*) x 50
 -}

----
-- Sõnumida mida server võib kliendile saata.
data ServerMessage
  = NewGame Name GameConf
  | NewRound
  | Opened [Open]
  | RoundOver Double
  | GameOver GameOutcome
  | End Statistics
  deriving (Show, Read)

{-
 - Eeldusel, et kliendid järgivad protokolli saadab server
 - fikseeritud kliendile järgmiselt sõnumeid:
 -
 - (NewGame -> (NewRound -> Opened* -> RoundOver) x 50 -> GameOver)* -> End
 -}
