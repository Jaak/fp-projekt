# Sissejuhatus

Miiniotsija (minesweeper) on ühe mängija arvutimäng, kus eesmärgiks on avastada
abstraktsel miiniväljal kõikide "miinide" positsioonid vältides nende
lõhkamist.

Mäng algab ettemääratud väljakul, mille kohta teab mängija selle mõõtmeid,
sellel asuvate miinide arvu ning iga väljaku lahtri kohta kas see on avatud,
suletud või on selle lahtri all miin.  Igal sammul peab väljaku lahendaja
märkima mõne suletud lahtri ohtlikuks või kontrollima, kas selle lahtri all on
miin. Kui osutub, et lahtri all tõepoolest on miin, siis mängija kaotab. Kui
tuleb välja, et seal miini ei ole, siis avatakse lahter ning mängijale antakse
teada seda ümbritsevate miinide arv. Lahtri ohtlikuks märkimine ei anna
mängijale mingit tagasisidet. Mäng võidetakse, kui kõik miiniga lahtrid on
märgitud ohtlikuks ning ülejäänud on avatud.

Kõiki miiniväljakuid ei ole võimalik lahendada, sest näiteks tühja väljaku
kohta ei ole mängijal informatsiooni võimalike miinide positsioonide kohta.
Arvestades, et lahtri ohtlikuks märkimine ei anna väljaku kohta informatsiooni
juurde, siis väga tõenäoliselt mingil hetkel peab mängija tegema pimeda otsuse
ja kontrollima, kas mingi lahtri all on miin või mitte.

Esimesel käigul kaotamine on ebameeldiv ja sellel põhjusel näiteks Windows
keskkonna "minesweeper", erinevalt meie implementatsioonist, genereerib miinide
asukohad alles pärast esimest käiku ning väldib miini valitud lahtrisse
paigutamist. Kui avatud lahtri ümbruses ei ole ühtegi miini, avatakse
automaatselt kõik seda ümbritsevad suletud lahtrid.  Näiteks, avades keskmise
lahtri täielikult suletud väljakul mõõtmetega 5 x 5:
    # # # # # # #
    #           #
    #           #
    #     0     #
    #           #
    #           #
    # # # # # # #
avatakse automaatselt lisaks 8 lahtrit:
    # # # # # # #
    #           #
    #   1 1 1   #
    #   1 0 1   #
    #   1 1 1   #
    #           #
    # # # # # # #
Sellist lahtrite avamist jätkatakse rekursiivselt. Automaatne rekursiivne
lahtrite avamine on realiseeritud ka meie miiniotsija mängu implementatsioonis.

On teada, et miiniväljakute lahendamine on NP-keeruline probleem [1]. Ilma
detailidesse laskumata tähendab see muuhulgas, et leidub lahenduvate väljakute
klass, mille lahendamiseks ei ole teada polünomiaalse keerukusega algoritmi.
Projektis osalejate üheks ülesandeks on kirjutada võimalikult tugev
miiniväljakute lahendaja.

Projekti eest on võimalik kokku saada kuni 50 punkti ning punktid jaotuvad
järgmiselt:
1. 10 punkti töötava lahenduse eest,
2. 30 punkti lahendaja tugevuse eest ja
3. 10 punkti koodi loetavuse eest.

Ülesannete lahendamiseks võite kasutada HaskellPlatform [2] teeki kuuluvaid
mooduleid. Lubatud ei ole "unsafe" prefiksiga funktsioonide kasutamine.
Lahenduste saatmise viimane tähtaeg on 24. mai. Kui saadate lahenduse vähemalt
nädal varem annan Teile tagasisidet ning võimaluse lahendust parandada.
Ülesannete lahendamisel ei ole vaja muretseda programmi efektiivsuse pärast
liiga palju, küll tasub mõelda algoritmide ajalise keerukuse peale.

# Esimene ülesanne

Esimeseks ülesandeks on miiniväljaku esitamine ning selle sõneks teisendamine.
Implementeerima peab järgmise mooduli:

```haskell
module Field (Cell(..), Coord(..), Field, emptyField, getCell, 
              surroundingCoords, surroundingCells, showField, update)
  where
```

Mooduli, funktsioonide ja tüüpide nimed võite valida oma voli järgi, aga
ülesande kirjelduses kasutame inglise keelseid nimetusi.

Alustame lahtrite esitamisega algebralise andmetüübiga:
```haskell
data Cell 
  = Closed
  | Open Int
  | Flagged
```
Iga mängu lahter on ühes kolmest olekust: suletud, avatud või märgitud.  Lisaks
on vaja arvestada sellega, et avatud lahtrite kohta teame palju miine selle
ümbruses on.  Seda esitab andmekonstruktori "Open" argument.  Ei ole otsest
vajadust esitada miiniga lahtreid, kuna ühe leidmine tähenda mängu kaotamist.

Edasi tuleb realiseerida andmetüüp või tüübisünonüüm koordinaatide ja väljaku
esitamiseks. Tüübid "Coord" ja "Field" peavad toetama järgmist liidest:

1. Tühja väljaku loomist. Argumendid on vastavalt: väljaku kõrgus, väljaku
   laius, miinide arv. Võite eeldada, et seda funktsiooni ei kutsuta kunagi
   välja halbade argumentide väärtustega.
```haskell
emptyField :: Int -> Int -> Int -> Field
emptyField = undefined
```

2. Koordinaadi järgi väljaku lahtri otsimist. Funktsioon peab tagastama
"Nothing" väärtuse, kui koordinaat on väljaku piiridest väljas.
```haskell
getCell :: Field -> Coord -> Maybe Cell
getCell = undefined
```
3. Koordinaadi järgi väljaku lahtri otsimist. Funktsioon peab tagastama
"Nothing" väärtuse, kui koordinaat on väljaku piiridest väljas.
```haskell
getCell :: Field -> Coord -> Maybe Cell
getCell = undefined
```
 
4. Naaberkoordinaatide otsimist. Loeme koordinaati iseenda naabriks.
```haskell
surroundingCoords :: Coord -> [Coord]
surroundingCoords = undefined
```

5. Naaberlahtrite otsimist. Lahtreid, mis ei jää väljaku piiridesse, ei tagastata.
```haskell
surroundingCells :: Field -> Coord -> [(Coord, Cell)]
surroundingCells = undefined
```

6. Väljaku sõneks teisendamist.
```haskell
showField :: Field -> String
showField = undefined
```
Antud funktsiooni realiseerimisel võib kasuks tulla prelüüdi funktsioon
"intersperse :: a -> [a] -> [a]".

7. Väljaku lahtrite uuendamist.
```haskell
update :: [(Coord, Cell)] -> Field -> Field
update = undefined
```

# Teine ülesanne

Ülesandeks on realiseerida serveriga suhtlus ning väljakute lahendaja.

## Serveriga suhtlus

Serveriga suhtluse protokoll on kirjeldatud failis "Prot.hs". Tuleb mainida, et
server ei kontrolli väga usinalt kas klient järgib protokolli ning ei tegele ka
vigadest taastumisega eriti hästi. Võib juhtuda, et serveri koodis on vigu ning
kui arvate, et olete mõne leidnud, andke kindlasti teada.

Ühe väljaku lahendamine toimub järgmiselt:
1. Server saadab kliendile algse mänguväljaku konfiguratsiooni.
2. Klient saadab serverile koordinaatide nimekirja lahtritest mis tuleb avada ja ohtlikuks märkida (ülimalt üks nimekirjadest võib olla tühi).
3. Kui mäng ei ole läbi saadab server vastuseks avatud koordinaatide nimekirja
   ning minnakse tagasi punkti 2.
4. Kui mäng on läbi antakse sellest kliendile teada.

Alustame kliendi baaskoodiga. Peame importima protokolli, Teie realiseeritud
mänguväljaku mooduli ning mõned süsteemi moodulid.

Alustame kliendi baaskoodiga. Peame importima protokolli, Teie realiseeritud
mänguväljaku mooduli ning mõned süsteemi moodulid.
```haskell
module Main (main) where

import qualified Prot
import Field

import Control.Exception (finally)
import System.IO 
import Network

port :: PortID
port = PortNumber 49267

host :: HostName
host = "127.0.0.1"
```

Protokolli moodul "Prot" on imporditud 'qualified' võtmesõnaga, et vältida
nimekonflikte.

Realiseerima peab järgmised funktsioonid:
1. Teie väljaku uuendamine serverilt tulnud infoga. Kindlasti kasutada eelmises
   osas defineeritud väljaku uuendamise funktsiooni.
```haskell
updateField :: [Prot.Open] -> [Prot.Coord] -> Field -> Field
updateField = undefined
```

2. Väljaku lahendamine. Kui näiteks soovite lahendajas teha
   mittedeterministlikke otsuseid või kanda lahendamisel kaasas mingit
   seisundite siis võite funktsiooni signatuuri ning järgnevat koodi vastavalt
   muuta.
```haskell
solveField :: Field -> ([Prot.Coord], [Prot.Coord])
solveField = undefined
```
