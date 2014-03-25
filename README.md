# Sissejuhatus

Miiniotsija ([minesweeper][3]) on ühe mängija arvutimäng, kus eesmärgiks on avastada abstraktsel miiniväljal kõikide "miinide" positsioonid ning selle käigus vältides nende lõhkamist.

Mäng algab ettemääratud väljakul, mille kohta teab mängija selle mõõtmeid, sellel asuvate miinide arvu ning iga väljaku lahtri kohta kas see on avatud, suletud või kas selle lahtri all asub miin. Igal sammul peab väljaku lahendaja märkima mõne suletud lahtri ohtlikuks või kontrollima, kas selle lahtri all on miin. Kui osutub, et lahtri all tõepoolest on miin, siis mängija kaotab. Kui tuleb välja, et seal miini ei ole, siis avatakse lahter ning mängijale antakse teada seda ümbritsevate miinide arv. Lahtri ohtlikuks märkimine ei anna mängijale mingit tagasisidet. Mäng võidetakse, kui kõik miiniga lahtrid on märgitud ohtlikuks ning ülejäänud on avatud.

Kõiki miiniväljakuid ei ole võimalik lahendada, sest näiteks tühja väljaku kohta ei ole mängijal informatsiooni võimalike miinide positsioonide kohta. Arvestades, et lahtri ohtlikuks märkimine ei anna väljaku kohta informatsiooni juurde, siis väga tõenäoliselt mingil hetkel peab mängija tegema pimeda otsuse ja kontrollima, kas mingi lahtri all on miin või mitte.

Esimesel käigul kaotamine on ebameeldiv ja sellel põhjusel näiteks Windows keskkonna "minesweeper", erinevalt meie implementatsioonist, genereerib miinide asukohad alles pärast esimest käiku ning väldib miini valitud lahtrisse paigutamist. Kui avatud lahtri ümbruses ei ole ühtegi miini, avatakse automaatselt kõik seda ümbritsevad suletud lahtrid.  Näiteks, avades keskmise lahtri täielikult suletud väljakul mõõtmetega 5 x 5:
```
# # # # # # #
#           #
#           #
#     0     #
#           #
#           #
# # # # # # #
```
avatakse automaatselt lisaks 8 lahtrit:
```
# # # # # # #
#           #
#   1 1 1   #
#   1 0 1   #
#   1 1 1   #
#           #
# # # # # # #
```
Sellist lahtrite avamist jätkatakse rekursiivselt. Automaatne rekursiivne lahtrite avamine on realiseeritud ka meie miiniotsija mängu implementatsioonis.

On teada, et miiniväljakute lahendamine on [NP-keeruline probleem][1]. Ilma detailidesse laskumata tähendab see muuhulgas, et leidub lahenduvate väljakute klass, mille lahendamiseks ei ole teada polünomiaalse keerukusega algoritmi. Projektis osalejate üheks ülesandeks on kirjutada võimalikult tugev miiniväljakute lahendaja.

Projekti eest on võimalik kokku saada kuni 50 punkti ning punktid jaotuvad järgmiselt:

* 10 punkti töötava lahenduse eest,
* 30 punkti lahendaja tugevuse eest ja
* 10 punkti koodi loetavuse eest.

Ülesannete lahendamiseks võite kasutada [HaskellPlatform][2] teeki kuuluvaid mooduleid. Lubatud ei ole "unsafe" prefiksiga funktsioonide kasutamine. Lahenduste saatmise viimane tähtaeg on 24. mai. Kui saadate lahenduse vähemalt nädal varem annan Teile tagasisidet ning võimaluse lahendust parandada. Ülesannete lahendamisel ei ole vaja muretseda programmi efektiivsuse pärast liiga palju, küll tasub mõelda algoritmide ajalise keerukuse peal.

# Esimene ülesanne

Esimeseks ülesandeks on miiniväljaku esitamine ning selle sõneks teisendamine. Implementeerima peab järgmise mooduli:
```haskell
module Field (Cell(..), Coord(..), Field, emptyField, getCell,
              surroundingCoords, surroundingCells, showField, update)
  where
```
Mooduli, funktsioonide ja tüüpide nimed võite valida oma voli järgi, aga ülesande kirjelduses kasutame inglise keelseid nimetusi.

Alustame lahtrite esitamisega algebralise andmetüübiga:
```haskell
data Cell
  = Closed
  | Open Int
  | Flagged
```
Iga mängu lahter on ühes kolmest olekust: suletud, avatud või märgitud. Lisaks on vaja arvestada sellega, et avatud lahtrite kohta teame palju miine selle ümbruses on. Seda esitab andmekonstruktori "Open" argument. Ei ole otsest vajadust esitada miiniga lahtreid, kuna ühe leidmine tähenda mängu kaotamist.

Edasi tuleb realiseerida andmetüüp või tüübisünonüüm koordinaatide ja väljaku esitamiseks. Tüübid `Coord` ja `Field` peavad toetama järgmist liidest:

1. Tühja väljaku loomist. Argumendid on vastavalt: väljaku kõrgus, väljaku laius, miinide arv. Võite eeldada, et seda funktsiooni ei kutsuta kunagi välja halbade argumentide väärtustega.

   ```haskell
   emptyField :: Int -> Int -> Int -> Field
   emptyField = undefined
   ```

2. Koordinaadi järgi väljaku lahtri otsimist. Funktsioon peab tagastama "Nothing" väärtuse, kui koordinaat on väljaku piiridest väljas.

    ```haskell
    getCell :: Field -> Coord -> Maybe Cell
    getCell = undefined
    ```

3. Naaberkoordinaatide otsimist. Loeme koordinaati iseenda naabriks.

    ```haskell
    surroundingCoords :: Coord -> [Coord]
    surroundingCoords = undefined
    ```

4. Väljakult naaberlahtrite otsimist. Lahtreid, mis ei jää väljaku piiridesse, ei tagastata.

    ```haskell
    surroundingCells :: Field -> Coord -> [(Coord, Cell)]
    surroundingCells = undefined
    ```

5. Väljaku sõneks teisendamist.

    ```haskell
    showField :: Field -> String
    showField = undefined
    ```

    Antud funktsiooni realiseerimisel võib kasuks tulla prelüüdi funktsioon "intersperse :: a -> [a] -> [a]".

6. Väljaku lahtrite uuendamist.

    ```haskell
    update :: [(Coord, Cell)] -> Field -> Field
    update = undefined
    ```

# Teine ülesanne

Ülesandeks on realiseerida serveriga suhtlus ning väljakute lahendaja.

## Serveriga suhtlus

Serveriga suhtluse protokoll on kirjeldatud failis "Prot.hs". Tuleb mainida, et server ei kontrolli väga usinalt kas klient järgib protokolli ning ei tegele ka vigadest taastumisega eriti hästi. Võib juhtuda, et serveri koodis on vigu ning kui arvate, et olete mõne leidnud, andke kindlasti teada.

Ühe väljaku lahendamine toimub järgmiselt:

1. Server saadab kliendile algse mänguväljaku konfiguratsiooni.
2. Klient saadab serverile koordinaatide nimekirja lahtritest mis tuleb avada ja ohtlikuks märkida (ülimalt üks nimekirjadest võib olla tühi).
3. Kui mäng ei ole läbi saadab server vastuseks avatud koordinaatide nimekirja ning minnakse tagasi punkti 2.
4. Kui mäng on läbi antakse sellest kliendile teada.

Alustame kliendi baaskoodiga. Peame importima protokolli, Teie realiseeritud mänguväljaku mooduli ning mõned süsteemi moodulid.
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
Protokolli moodul `Prot` on imporditud `qualified` võtmesõnaga, et vältida nimekonflikte.

Realiseerima peab järgmised funktsioonid:

1. Teie väljaku uuendamine serverilt tulnud infoga. Kindlasti kasutada eelmises osas defineeritud väljaku uuendamise funktsiooni.

    ```haskell
    updateField :: [Prot.Open] -> [Prot.Coord] -> Field -> Field
    updateField = undefined
    ```

2. Väljaku lahendamine. Kui näiteks soovite lahendajas teha mittedeterministlikke otsuseid, logida või kanda lahendamisel kaasas mingit seisundit siis võite funktsiooni signatuuri ning järgnevat koodi vastavalt muuta.

    ```haskell
    solveField :: Field -> ([Prot.Coord], [Prot.Coord])
    solveField = undefined
    ```
Järgnev protseduur seob kokku eeldefineeritud funktsioonid ning realiseerib serveriga suhtluse. Kindlasti lisage protseduuri silumisel kasuks tulevaid sõnumeid. Näiteks tuleb abiks kaotamisele eelneva väljaku seisundi ja tehtud otsuse välja printimine.

```haskell
game :: Handle -> IO ()
game handle = do
```

Protseduuris `main` loome serveriga ühenduse ning kutsume protseduuri `game`.

```haskell
main :: IO ()
main = withSocketsDo $ do
  handle <- connectTo host port
  hSetBuffering handle LineBuffering
  (game handle `finally` hClose handle)
```

Projekti teise osa raam on veidi põhjalikumalt kommenteeritud failis 'Skeleton.hs'.

## Lahendajate ideid

Konkreetse lahendaja realiseerimine jääb Teie ülesandeks. Toon siin ära hulga ideid mida võib, ja oleks soovitatav, omavahel kombineerida. Näiteks võite alustada lihtsa lahendajaga ning kui see ei anna tulemusi siis proovida õnne keerukamaga ning kui ka see ei oska midagi ära teha siis tuleks langeda naiivse lahendaja algoritmi peale.

Pakun välja järgnevaid algoritme:

* Naiivne lahendaja.

    Alati avab suvalise suletud lahtri.

* Lihtne lahendaja.

    Kui leidub avatud lahter märgendiga 'k', mille ümbruses on k ohtlikuks määratud väljakut siis võime avada kõik ümbritsevad kinnised lahtrid.

    Kui leidub avatud lahter märgendiga 'k', mille ümbruses on l (l <= k) suletud lahtrit ja k - l ohtliku lahtrit siis võime kõik suletud lahtrid ohtlikuks märkida.


* Tõenäosuslik.

    Võite üritada hinnata tõenäosust, et mingi suletud lahter on ohtlik ning avada kõige ohutum lahter. Täpse tõenäosuse hindamine ilmselt nõuab kõikide kombinatsioonide läbi vaatamist, küll aga võib tõenäosuslik lahendaja olla märgatav edasiminek naiivsest.

* Lineaarvõrrandisüsteeme lahendades.

    Miiniväljakute lahendamise saab teisendada lineaarvõrrandisüsteemi lahendamiseks.  Iga k miiniga avatud lahtri:

    ```
    x1 x2 x3
    x4  k x5
    x6 x7 x8
    ```

    korral lisame süsteemi juurde võrrandi:
    x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 = k

    Näiteks (tähistades kinniseid lahtreid x'idega):

    ```
    # #  #  #
    # 0  1  x1
    # 0  1  x2
    # 0  1  x3
    # 1  1  x4
    # x7 x6 x5
    ```

    Saame võrrandite süsteemi:

    ```
    x1 + x2                          = 1
    x1 + x2 + x3                     = 1
         x2 + x3 + x4                = 1
              x3 + x4 + x5 + x6 + x7 = 1
	                         x6 + x7 = 1
    ```

    Lahutades esimese võrrandi teisest saame kohe teada, et x3 = 0 ehk, et see
    lahter on kindlasti ohutu. Edasi lihtsustades saame:

    ```
    x4 = 0 ja x5 = 0 (sest x4 + x5 = 0)
    x2 = 1
    x1 = 0
    x6 = 1 või x7 = 1 (sest x6 + x7 = 1)
    ```

    Nüüd saame serverile teada anda, et ohtlikuks tuleb määrata lahter x2 ja
    avada võib lahtrid x1, x3, x4 ja x5.

* Alternatiivne.

    Alati võib ise välja mõelda huvitavaid lahendamise algoritme.

    Näiteks. Loome mänguväljakust kaks koopiat. Ühes eeldame, et mingi positsioon on ohtlik ning teises, et see on ohutu.  Lahendame ja lihtsustame mõlemat väljakut lihtsa lahendajaga. Kui mõlemal väljakul jõuame mingi positsiooni kohta samale järeldusele siis see peab nii olema ka algsel väljakul.  Kui jõuame ühe väljakuga võimatu olukorrani siis peab teine kajastama tõde.  Võib tähele panna, et seda lahendajat saab rakendada ka rekursiivselt. Toodud idee on küll tunduvalt nõrgem korralikust lineaarvõrrandisüsteemile baseeruvast lahendajast aga suudab siiski teha päris häid valikuid ning seda on palju lihtsam realiseerida.

* Jõu meetodil.

    Kui suletud lahtreid on piisavalt vähe võib väljakut lahendada vaadates läbi kõik võimalikud kombinatsioonid.

[1]: http://for.mat.bham.ac.uk/R.W.Kaye/minesw/ordmsw.htm
[2]: http://hackage.haskell.org/platform/
[3]: http://en.wikipedia.org/wiki/Minesweeper_%28video_game%29
