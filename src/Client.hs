import Prelude hiding (ioError)
import qualified Prot as Prot

import Control.Exception (finally, ioError)
import Control.Applicative
import System.IO
import Network

port :: PortID
port = PortNumber 49267

host :: HostName
host = "127.0.0.1"

type Field = ()

initField :: Prot.GameConf -> Prot.InitialBoard -> Field
initField = undefined

updateField :: [Prot.Open] -> Field -> Field
updateField = undefined

solveField :: Field -> [Prot.Coord]
solveField = undefined

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

main :: IO ()
main = withSocketsDo $ do
  handle <- connectTo host port
  hSetBuffering handle LineBuffering
  (game handle `finally` hClose handle)
