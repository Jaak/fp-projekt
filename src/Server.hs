import Prot
import Field

import Prelude hiding (ioError)
import System.Random
import System.IO
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Control.Applicative
import Control.Monad.Reader
import Network
import Control.Exception (SomeException, bracket, catch, ioError, finally)

-- Client state
type ClientHandle = Handle

-- Server state
data Server = Server {
    _gen ::  MVar StdGen
  }

type ServerM = ReaderT Server IO

mkServer :: IO Server
mkServer = Server <$> (newMVar =<< newStdGen)

clientThread :: ClientHandle -> ServerM ()
clientThread client = do
  replicateM_ 100 (singleGame client)
  kickClient client

defaultConf :: GameConf
defaultConf = GameConf {
    _height = 16,
    _width = 30,
    _num_mines = 99
  }

raiseUserError :: MonadIO m => String -> m a
raiseUserError = liftIO . ioError . userError

singleGame :: ClientHandle -> ServerM ()
singleGame client = do
   let GameConf h w n = defaultConf
   initialBoard <- randomField h w n <$> getGen
   sendMsg client (NewGame defaultConf (showField initialBoard))
   go initialBoard
   where

    go currentBoard = do
      Clicks cs <- recvMsg client
      when (null cs) $ do
        raiseUserError "Received empty list of clicks."
      case clicks cs currentBoard of
        GameFinished outcome -> sendMsg client (GameOver outcome)
        GameContinue newBoard revealed -> do
          sendMsg client (Opened revealed)
          go newBoard


kickClient :: ClientHandle -> ServerM ()
kickClient client = sendMsg client End

maybeRead :: Read a => String -> Maybe a
maybeRead str = case reads str of
  [(x, "")] -> Just x
  _         -> Nothing

recvMsg :: ClientHandle -> ServerM ClientMessage
recvMsg client = liftIO $ do
  maybeMsg <- maybeRead <$> hGetLine client
  case maybeMsg of
    Nothing -> raiseUserError "Received incorrectly formatted message from user!"
    Just x -> return x

sendMsg :: ClientHandle -> ServerMessage -> ServerM ()
sendMsg client msg = liftIO $ hPutStrLn client (show msg)

getGen :: ServerM StdGen
getGen = do
  mgen <- asks _gen
  liftIO $ modifyMVar mgen (return . split)

main :: IO ()
main = withSocketsDo $ do
  withFile "log.txt" WriteMode $ \logHandle -> do
    hSetBuffering logHandle LineBuffering
    server <- mkServer
    withListenOn $ \soc -> forever $ do
      (handle, hostName, _) <- accept soc
      hPutStrLn logHandle ("Accepted connection from " ++ hostName)
      hSetBuffering handle LineBuffering
      void . forkIO $ do
        runReaderT (clientThread handle) server
          `catch` logException logHandle
          `finally` hPutStrLn logHandle ("Closed connection with " ++ hostName)
  where
    logException :: Handle -> SomeException -> IO ()
    logException hndl e = hPutStrLn hndl (show e)

port :: PortID
port = PortNumber 49267

withListenOn :: (Socket -> IO a) -> IO a
withListenOn = bracket (listenOn port) sClose
