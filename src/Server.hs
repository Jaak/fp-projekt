import Game
import Prot

import Control.Applicative
import Control.Arrow (second)
import Control.Concurrent
import Control.Exception (SomeException, bracket, catch, ioError, finally)
import Control.Monad
import Control.Monad.Reader
import Data.Maybe (catMaybes, isNothing)
import Network
import Prelude hiding (ioError)
import System.Environment (getArgs)
import System.IO
import System.Random

{---------------------------------
 - Some rather generic functions -
 ---------------------------------}

maybeRead :: Read a => String -> Maybe a
maybeRead str = case reads str of
  [(x, "")] -> Just x
  _         -> Nothing

raiseUserError :: MonadIO m => String -> m a
raiseUserError = liftIO . ioError . userError

-- n-way split of StdDen
splitStdGenN :: Int -> StdGen -> [StdGen]
splitStdGenN k = take k . splits
  where splits = uncurry (:) . second splits . split

{---------
 - Types -
 ---------}

-- Client state
type ClientHandle = Handle

-- Server state
data ServerState = ServerState {
    _gen ::  MVar StdGen
  }

type ServerM = ReaderT ServerState IO

data Flag
  = FlagRandom Int Int Int Int
  | FlagPreset [FilePath]

readFlag :: [String] -> Maybe Flag
readFlag args = tryRandom args `mplus` tryPreset args
  where
    tryRandom ["--random", n, m, h, w] = FlagRandom <$> maybeRead n <*> maybeRead m <*> maybeRead h <*> maybeRead w
    tryRandom _ = Nothing

    tryPreset ("--preset" : rest) = Just (FlagPreset rest)
    tryPreset _ = Nothing

-- TODO: no error checking whatsoever
-- Load games based on the set flags.
-- We are returning a closure under the io action because we need to consider two cases:
-- 1. We generate a random sequence of games. This sequence is different for every client.
-- 2. We load a preset sequence of games from a file and always return those. In this case the StdGen is ignored.
loadGames :: Flag -> IO (StdGen -> [Game])
loadGames (FlagRandom n m h w) = return $ map (randomGame h w m) . splitStdGenN n
loadGames (FlagPreset files) = do
  gamesList <- forM files $ \file -> do
    gs <- map readGame . lines <$> readFile file
    when (any isNothing gs) $
      hPutStrLn stderr $ "Failed to read some preset games from \"" ++ file ++ "\"."
    return $ catMaybes gs
  let games = concat gamesList
  return $ const games

mkServerState :: IO ServerState
mkServerState = ServerState <$> (newMVar =<< newStdGen)

clientThread :: ClientHandle -> (StdGen -> [Game]) -> ServerM ()
clientThread client generateGames = do
  games <- generateGames <$> getGen
  mapM_ (singleGame client) games
  kickClient client

singleGame :: ClientHandle -> Game -> ServerM ()
singleGame client initGame = do
   sendMsg client (NewGame (getGameConf initGame) (showGame initGame))
   go initGame
   where

    go game = do
      Clicks cs <- recvMsg client
      when (null cs) $
        raiseUserError "Received empty list of clicks."
      case clicks cs game of
        GameFinished outcome -> sendMsg client (GameOver outcome)
        GameContinue game' opened -> do
          sendMsg client (Opened opened)
          go game'

kickClient :: ClientHandle -> ServerM ()
kickClient client = sendMsg client End

recvMsg :: ClientHandle -> ServerM ClientMessage
recvMsg client = liftIO $ do
  maybeMsg <- maybeRead <$> hGetLine client
  case maybeMsg of
    Nothing -> raiseUserError "Received incorrectly formatted message from user!"
    Just x -> return x

sendMsg :: ClientHandle -> ServerMessage -> ServerM ()
sendMsg client = liftIO . hPrint client

getGen :: ServerM StdGen
getGen = do
  mgen <- asks _gen
  liftIO $ modifyMVar mgen (return . split)

actualMain :: Flag -> IO ()
actualMain flag = withFile "log.txt" WriteMode $ \logHandle -> do
  hSetBuffering logHandle LineBuffering
  generateGames <- loadGames flag
  server <- mkServerState
  withListenOn $ \soc -> forever $ do
    (handle, hostName, _) <- accept soc
    hPutStrLn logHandle ("Accepted connection from " ++ hostName)
    hSetBuffering handle LineBuffering
    void . forkIO $
      runReaderT (clientThread handle generateGames) server
        `catch` logException logHandle
        `finally` hPutStrLn logHandle ("Closed connection with " ++ hostName)
  where
    logException :: Handle -> SomeException -> IO ()
    logException = hPrint

main :: IO ()
main = withSocketsDo $ do
  maybeFlag <- readFlag <$> getArgs
  case maybeFlag of
    Just opts -> actualMain opts
    Nothing -> do
      hPutStrLn stderr $ unlines
        [ "Invalid command line arguments. Options:"
        , "   --random <num> <mines> <height> <width>"
        , "   --preset [FILE..]"]


port :: PortID
port = PortNumber 49267

withListenOn :: (Socket -> IO a) -> IO a
withListenOn = bracket (listenOn port) sClose
