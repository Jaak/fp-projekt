# Sissejuhatus

Miiniotsija ([minesweeper][3]) on ühe mängija arvutimäng, kus eesmärgiks on avastada abstraktsel miiniväljal kõikide "miinide" asukohad ning selle käigus vältides nende lõhkamist.

Mäng algab ettemääratud väljakul, mille kohta teab mängija selle mõõtmeid, sellel asuvate miinide arvu ning iga väljaku lahtri kohta kas see on avatud, suletud või kas selle lahtri all asub miin. Igal sammul peab väljaku lahendaja avama mõne suletud lahtri. Kui osutub, et lahtri all tõepoolest on miin, siis mängija kaotab. Kui tuleb välja, et seal miini ei ole, siis avatakse lahter ning mängijale antakse teada seda ümbritsevate miinide arv. Mäng võidetakse kui kõik ohutud lahtrid on avatud.

Loomulikult ei ole kõiki miiniväljakuid võimalik lahendada, sest näiteks tühja väljaku kohta ei ole mängijal informatsiooni võimalike miinide positsioonide kohta ning seega mängija peab tegema pimeda valikud ja kontrollima, kas mingi lahtri all on miin või mitte.

Esimesel käigul kaotamine on ebameeldiv ja sellel põhjusel näiteks Windows keskkonna "minesweeper", erinevalt meie teostusest, genereerib miinide asukohad alles pärast esimest käiku ning väldib miini valitud lahtrisse paigutamist. Kui avatud lahtri ümbruses ei ole ühtegi miini, avatakse automaatselt kõik seda ümbritsevad suletud lahtrid (nii käitub ma meie teostus).  Näiteks, avades keskmise lahtri täielikult suletud väljakul mõõtmetega 5 x 5:
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
Sellist lahtrite avamist jätkatakse rekursiivselt.

On teada, et miiniväljakute lahendamine on [NP-keeruline probleem][1]. Detailidesse laskumata tähendab see muuhulgas, et leidub lahenduvate väljakute klass, mille lahendamiseks ei ole teada polünomiaalse keerukusega algoritmi. Projektis osalejate peamiseks ülesandeks on teostada võimalikult tugev lahendaja.

Projekti eest on võimalik kokku saada kuni 50 punkti ning punktid jaotuvad järgmiselt:

* 10 punkti töötava lahenduse eest,
* 30 punkti lahendaja tugevuse eest ja
* 10 punkti koodi loetavuse eest.

Ülesannete lahendamiseks võite kasutada [HaskellPlatform][2] teeki kuuluvaid mooduleid. Lubatud ei ole "unsafe" prefiksiga funktsioonide kasutamine. Lahenduste esitamise viimane tähtaeg on 30. mai. Kui saadate lahenduse vähemalt nädal varem annan Teile tagasisidet ning võimaluse lahendust parandada. Mida varem Te oma lahenduse esitate seda rohkem võimalusi on Teil minu tagasisidet saada ja oma lahendust parandada.

Ülesanne on jagatud kaheks alamosaks, et pakkuda natuke struktuuri ning ideid kuidas alustada. Seda struktuuri ei pea järgima ning Teie lahendaja võib välja näha hoopis teistsugune.

# Esimene ülesanne

Esimeseks ülesandeks on miiniväljaku esitamine ning selle sõneks teisendamine. Esimese osa raam asub failis `Field.hs`. Implementeerima peab järgmise mooduli:
```haskell
module Field (Cell(..), Field, emptyField, getCell,
              surroundingCoords, surroundingCells, showField, update)
  where
```
Mooduli, funktsioonide ja tüüpide nimed võite valida oma voli järgi, aga ülesande kirjelduses kasutame inglisekeelseid nimetusi.

Väljaku lahtrit esitame järgneva algebralise andmetüübiga:
```haskell
data Cell
  = Closed
  | Open Int
  | Flagged
```
Iga mängu lahter on ühes kolmest olekust: suletud, avatud või märgitud. Lisaks on vaja arvestada sellega, et avatud lahtrite kohta teame kui palju miine selle ümbruses on. Seda esitab andmekonstruktori `Open` argument. Andmekonstruktor `Flagged` tähistab, et oleme tuvastanud, et antud lahtri all on kindlasti miin.

Edasi tuleb defineerida andmetüüp (või tüübisünonüüm) väljaku esitamiseks. Andmestruktuuri valik on vaba aga üks hea valik on `Data.Map` moodulis asuv `Map` andmestruktuur. Andmetüüp `Field` peab toetama järgmist liidest:

1. Tühja väljaku loomist. Tühjaks väljakuks loeme sellist mille lahtrite kohta ei ole meil mingit informatsiooni teada (kõik lahtrid on suletud). Argumendid on vastavalt: väljaku kõrgus, väljaku laius, miinide arv. Võite eeldada, et seda funktsiooni ei kutsuta kunagi välja halbade argumentide väärtustega.

   ```haskell
   emptyField :: Int -> Int -> Int -> Field
   emptyField = undefined
   ```

2. Koordinaadi järgi väljaku lahtri otsimist. Funktsioon peab tagastama `Nothing` väärtuse, kui koordinaat on väljaku piiridest väljas. Koordinaat `Coord` on defineeritud failis `Prot.hs` kui tüübisünonüüm täisarvude paari jaoks.

    ```haskell
    getCell :: Field -> Coord -> Maybe Cell
    getCell = undefined
    ```

3. Väljaku lahtri uuendamist.

    ```haskell
    setCell :: Coord -> Cell -> Field -> Field
    setCell = undefined
    ```

    Kasuks tuleb ka versioon funktsioonist mis uuendab palju lahtreid korraga.

    ```haskell
    setCells :: [(Coord, Cell)] -> Field -> Field
    setCells = undefined
    ```


4. Naaberkoordinaatide otsimist.

    ```haskell
    surroundingCoords :: Coord -> [Coord]
    surroundingCoords = undefined
    ```

5. Väljakult naaberlahtrite otsimist. Lahtreid, mis ei jää väljaku piiridesse, ei tagastata.

    ```haskell
    surroundingCells :: Field -> Coord -> [(Coord, Cell)]
    surroundingCells = undefined
    ```

6. Väljaku sõneks teisendamist. See funktsioon võib osutuda kasulikuks lahendaja silumisel.

    ```haskell
    showField :: Field -> String
    showField = undefined
    ```

    Antud funktsiooni teostamisel tulevad kasuks prelüüdi funktsioonid `unwords` ja `unlines`.

# Teine ülesanne

Ülesandeks on teostada serveriga suhtlus ning väljakute lahendaja. Projekti teise osa raam asub failis `Client.hs`.

## Serveriga suhtlus

Kõik serveriga suhtluse kasutatavad andmestruktuurid on defineeritud failis `Prot.hs`. Tuleb mainida, et server ei kontrolli väga usinalt kas klient järgib protokolli ning ei tegele ka vigadest taastumisega eriti hästi. Võib juhtuda, et serveri koodis on vigu ning kui arvate, et olete mõne leidnud, andke kindlasti teada.

Ühe väljaku lahendamine toimub järgmise protokolli alusel:

1. Server saadab kliendile kas algse mänguväljaku konfiguratsiooni või teate, et töö on lõpetatud. Kui töö on lõpetatud siis server lõpetab kliendiga suhtlemise.
2. Klient saadab serverile koordinaatide nimekirja avatavatest lahtritest.
3. Kui mäng ei ole läbi saadab server vastuseks avatud koordinaatide nimekirja ning minnakse tagasi punkti 2. Serveri poolt saadetud koordinaatide nimikiri sisaldab kliendi poolt saadetud koordinaate aga võib ka enam koordinaate sisaldada kui tuli avada mõni lahter mille naabruses ei olnud ühtegi miini.
4. Kui mäng on läbi antakse sellest kliendile teada.

Alustame kliendi baaskoodiga. Peame importima protokolli, Teie teostatud mänguväljaku mooduli ning mõned süsteemsed moodulid.
```haskell
import qualified Prot
import Field

import Prelude hiding (ioError)
import Control.Exception (finally, ioError)
import Control.Applicative
import System.IO
import Network

port :: PortID
port = PortNumber 49267

host :: HostName
host = "127.0.0.1"
```
Protokolli moodul `Prot` on imporditud `qualified` võtmesõnaga, et vältida nimekonflikte.

Teie ülesandeks jääb teostada järgmine (teostused ei pea asuma `Client.hs` failis):

1. Väljaku initsialiseerimine algseisundiga. Andmetüüp `Prot.InitialBoard` on sünonüüm sõnede tüübile `String` ning selle formaat on järgmine:

    * selle pikkuseks on väljaku lahtrite arv,
    * esimene märk vastab ülemisele vasakule lahtrile ning viimane alumisele paremale,
    * tähemärk `F` tähistab miini, `?` tähistab suletud lahtrit ning numbrid `0` kuni `8` tähistavad avatud lahtrit vastavate ümbritsevate miinide arvuga.

    Selle funktsiooni teostamisel luua esmalt tühi väljak ning seejärel sättida lahtrid kasutades `setCells` funktsiooni.

    ```haskell
    initField :: Prot.GameConf -> Prot.InitialBoard -> Field
    initField = undefined
    ```

2. Mänguväljaku uuendamine serverilt tulnud infoga. Kindlasti kasutada eelmises osas defineeritud väljaku uuendamise funktsiooni.

    ```haskell
    updateField :: [Prot.Open] -> Field -> Field
    updateField = undefined
    ```

2. Väljaku lahendamine. Kui näiteks soovite lahendajas teha mittedeterministlikke otsuseid, logida või kanda lahendamisel kaasas mingit seisundit siis võite funktsiooni signatuuri ning järgnevat koodi vastavalt muuta. Soovitan silumise eesmärgil mitte kasutada `IO` monaadi kuna ajutisi sõnumeid saab printida kasutades ka `Debug.Trace` moodulist asuvat `trace` funktsiooni.

    ```haskell
    solveField :: Field -> [Prot.Coord]
    solveField = undefined
    ```

Protseduur `game` seob kokku eeldefineeritud funktsioonid ning teostab serveriga suhtluse. Kindlasti täiendage seda protseduuri silumisel kasuks tulevate sõnumitega. Näiteks tuleb abiks kaotamisele eelneva väljaku seisundi ja tehtud otsuse välja printimine.

```haskell
game :: Handle -> IO ()
game handle = startGame
  where

    startGame = do
      msg <- recv
      case msg of
        Prot.NewGame cfg initBoard -> playGame (initField cfg initBoard)
        Prot.End -> return ()
        _ -> raiseError "Expected \"NewGame\" or \"End\" message."

    playGame currentBoard = do
      let coords = solveField currentBoard
      send (Prot.Clicks coords)
      resp <- recv
      case resp of
        Prot.Opened os -> playGame (updateField os currentBoard)
        Prot.GameOver status -> print status >> startGame
        _ -> raiseError "Expected \"Opened\" or \"GameOver\" message."

    send msg = hPutStrLn handle (show msg)
    recv = read <$> hGetLine handle
    raiseError = ioError . userError
```

Protseduuris `main` loome serveriga ühenduse ja kutsume protseduuri `game`.

```haskell
main :: IO ()
main = withSocketsDo $ do
  handle <- connectTo host port
  hSetBuffering handle LineBuffering
  game handle `finally` hClose handle
```

## Lahendajate ideid

Konkreetse lahendaja teostamine jääb Teie ülesandeks aga toon siin ära hulga ideid mida võib, ja oleks soovitatav, omavahel kombineerida. Näiteks käigu valimisel võite alustada lihtsa lahendajaga ning kui see ei anna tulemust siis proovida õnne keerukamaga ning kui ka see ei oska midagi ära teha siis tuleks teha juhuslik valik.

Tõeliselt tugevad lahendajad suudavad keskmiselt lahendada 30% ekspert taseme (99 miini 16x30) ning 70% keskmise taseme (40 miini 16x16) väljakutest. Kui soovite saada maksimum punkte siis 10% ekspert taseme väljakutest on piisav.

### Naiivne lahendaja

Avab esimese ettejuhtuva (või juhuslikult valitud) suletud lahtri.

### Lihtne lahendaja

Kui leidub avatud lahter mille ümbruses on 'k' miini ning täpselt 'k' miini on juba ohtlikuks määratud siis võime avada kõik ümbritsevad kinnised lahtrid.

Kui leidub avatud lahter märgendiga 'k', mille ümbruses on l (l <= k) suletud lahtrit ja k - l ohtliku lahtrit siis võime kõik suletud lahtrid ohtlikuks märkida.


### Tõenäosuslik lahendaja

Võite üritada hinnata tõenäosust, et mingi suletud lahter on ohtlik ning avada kõige ohutum lahter. Täpset tõenäosust saab näiteks hinnata kõikide võimalike kombinatsioonide läbi vaatamisega. Väga oluline on märkida, et miini esinemise tõenäosus on erinev tõenäosusest, et seda miini avades mäng kaotatakse kuna arvestada tuleb ka edasiste valikutega. Seega, ainult miini esinemise tõenäosust arvestav lahendaja ei ole kõige tugevam. Parimad lahendajad on need, mis suudavad iga lahtri jaoks hinnata selle avamisel võitmise (kaotamise) tõenäosust.

Näiteks, kui väljakul esineb selline konfiguratsioon (kus F tähistab, et antud lahtri all on miin):

```
F F F
F   F
3   F
F   F
F F F
```

siis iga suletud lahtri all on tõenäosusega 1/3 miin aga keskmise lahtri avamisel on tõenäosus kaotada 2/3 ning teisel kahel lahtril ainult 1/3.

### Lineaarvõrrandisüsteeme lahendav lahendaja

Miiniväljakute lahendamise saab teisendada lineaarvõrrandisüsteemi lahendamiseks. Iga k miiniga avatud lahtri:

```
x1 x2 x3
x4  k x5
x6 x7 x8
```

korral lisame süsteemi võrrandi: `x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 = k` Kui mõni neist muutujates on avatud või ohtlikuks määratud siis asendame selle vastavalt kas väärtusega 0 või 1.

Näiteks (tähistades kinniseid lahtreid x'idega) väljakust:

```
# #  #  #
# 0  1  x1
# 0  1  x2
# 0  1  x3
# 1  1  x4
# x7 x6 x5
```

konstrueerime võrrandite süsteemi:

```
x1 + x2                          = 1
x1 + x2 + x3                     = 1
     x2 + x3 + x4                = 1
          x3 + x4 + x5 + x6 + x7 = 1
                         x6 + x7 = 1
```

Lahutades esimese võrrandi teisest saame kohe teada, et x3 = 0 ehk, et see lahter on kindlasti ohutu. Edasi lihtsustades saame:

```
x4 = 0 ja x5 = 0 (sest x4 + x5 = 0)
x2 = 1
x1 = 0
x6 = 1 XOR x7 = 1 (sest x6 + x7 = 1)
```

Nüüd saame serverile teada anda, et ohtlikuks tuleb määrata lahter x2 ja avada võib lahtrid x1, x3, x4 ja x5.

Pange tähele, et nii konstrueeritud lineaarvõrrandisüsteemide muutujad võtavad ainult väärtusi 0 ja 1. Seega klassikalised lineaarvõrrandisüsteemide lahendamise meetodid (näiteks [Gaussi meetod][4]) ei anna alati õigeid vastuseid (võivad tagastada piiridest väljas olevaid tulemusi) ning seega on märksa nõrgemad kui peaksid. Mõnevõrra üldisema probleemi nimi on [0-1 lineaarne programmeerimine][5] ning täpse lahendaja teostamine on suhteliselt keeruline ülesanne (teada on ainult halvimal juhul eksponentsiaalsed algoritmid).

### Alternatiivne lahendaja

Alati võib ise välja mõelda huvitavaid lahendamise algoritme.

Näiteks. Loome mänguväljakust kaks koopiat. Ühes eeldame, et mingi positsioon on ohtlik ning teises, et see on ohutu.  Lahendame ja lihtsustame mõlemat väljakut lihtsa lahendajaga. Kui mõlemal väljakul jõuame mingi positsiooni kohta samale järeldusele siis see peab nii olema ka algsel väljakul.  Kui jõuame ühe väljakuga võimatu olukorrani siis peab teine kajastama tõde.  Võib tähele panna, et seda lahendajat saab rakendada ka rekursiivselt. Toodud idee on küll tunduvalt nõrgem korralikust lineaarvõrrandisüsteemile baseeruvast lahendajast aga suudab siiski teha päris häid valikuid ning seda on palju lihtsam teostada.

### Jõu meetodil lahendaja

Kui suletud lahtreid on piisavalt vähe võib väljakut lahendada vaadates läbi kõik võimalikud kombinatsioonid.

# Alternatiivülesanded

Teil on võimalik ülaltood projekti asendada siin toodud alternatiivsete ülesannetega, Igat lisaülesannet saab lahendada ainult üks tudeng. Hinne kujuneb järgmiselt: vähemalt 10 punkti saab nõuetele vastava lahenduse eest ning ülejäänud punktid kujunevad lahenduse kvaliteedi (jõudlus, väljanägemine, töökindlus) ning koodi loetavuse pealt. Kuna alternatiivsed ülesanded on keerukamad kui põhiülesanne siis täidan piiratud koguses juhendaja rolli.

## Graafiline kasutajaliides

Ülesandeks on teostada graafiline kasutajaliides miiniotsija mängule mis integreerub meie serveriga. See oleks väga kasulik testimiseks. Teostus peab kasutajaga interakteeruma analoogselt Windows keskkonna minesweeperiga: lahtreid peab saama avada vasaku hiireklõpsuga, ohtlikuks märkida parema hiireklõpsuga ning kesmine hiireklõps peab avama kõik ohtlikuks märgitud lahtrit ümbritsevad lahtrid.

Selle ülesande lahendamisel ei saa piirduda Haskell platform teegistikuga ning kasutada võib mistahes linux keskkonda toetavat graafikateeki (vaata http://hackage.haskell.org/packages/#cat:Graphics ja http://hackage.haskell.org/packages/#cat:GUI). Soovitan peale vaadata graafikateegile [Gloss][8].

## Täieliku informatsiooniga miiniotsija

Ülesandeks on teostada käsureaprogramm mis saab sisendiks arvude tabeli ning alati annab väljundiks kas ühe miiniväljaku konfiguratsiooni (0-1 tabeli) mille iga lahtri naabrite arvud vastavad sisendtabelile või teatab, et vastavat väljakut ei leidu. Soovin algoritmi mis töötaks kiiresti ka suurte sisendite puhul.

## Loogikavalemitest väljakute genereerija

Ülesandeks on teostada käsureaprogramm mis saab sisendiks loogikavalemi (vabad muutujad, konjunktsioon, disjunktsioon ning eitus) ning väljundiks annab miiniväljaku konfiguratsiooni mis on lahenduv parajasti siis kui sisendvalem on kehtestatav. Eeskiri kuidas valemeid teisendada miiniväljakuteks on ette [määratud][7]. Peamine keerukus seisneb selles kuidas väikesed väljakutükid omavahel kombineerida.



[1]: http://for.mat.bham.ac.uk/R.W.Kaye/minesw/ordmsw.htm
[2]: http://hackage.haskell.org/platform/
[3]: http://en.wikipedia.org/wiki/Minesweeper_%28video_game%29
[4]: http://en.wikipedia.org/wiki/Gauss_elimination_method
[5]: http://en.wikipedia.org/wiki/Integer_linear_programming
[6]: http://en.wikipedia.org/wiki/Monte_carlo_method
[7]: http://for.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.pdf
[8]: http://gloss.ouroborus.net/
