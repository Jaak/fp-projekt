import Prot
import Field

import System.Random
import System.IO
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Control.Applicative
import Control.Monad.Reader
import Network
import Control.Exception (bracket)

-- Client state
type ClientHandle = Handle

-- Server state
data Server = Server {
    _logHandle  ::  MVar Handle,
    _gen        ::  MVar StdGen
  }

mkServer :: Handle -> IO Server
mkServer logHandle = Server <$> newMVar logHandle <*> (newMVar =<< newStdGen)

type ServerM = ReaderT Server IO

clientThread :: ClientHandle -> ServerM ()
clientThread client = do
  replicateM_ 100 (singleGame client)
  kickClient client

singleGame :: ClientHandle -> ServerM ()
singleGame = undefined

kickClient :: ClientHandle -> ServerM ()
kickClient = undefined

logLn :: String -> ServerM ()
logLn msg = do
  mhandle <- asks _logHandle
  liftIO $ withMVar mhandle $ \handle -> do
    hPutStrLn handle msg

recvMsg :: ClientHandle -> ServerM ClientMessage
recvMsg = undefined

sendMsg :: ClientHandle -> ServerMessage -> ServerM ()
sendMsg = undefined

getGen :: ServerM StdGen
getGen = do
  mgen <- asks _gen 
  liftIO $ modifyMVar mgen (return . split)

main :: IO ()
main = withSocketsDo doServer

doServer :: IO ()
doServer = withFile "log.txt" WriteMode $ \logHandle -> do
  hSetBuffering logHandle LineBuffering
  server <- mkServer logHandle
  waitForClient server

waitForClient :: Server -> IO ()
waitForClient server = withListenOn $ \soc -> forever $ do
  (handle, host, _) <- accept soc
  hSetBuffering handle LineBuffering
  void . forkIO $ do
    runReaderT (clientThread handle) server

port :: PortID
port = PortNumber 49267

withListenOn :: (Socket -> IO a) -> IO a
withListenOn = bracket (listenOn port) sClose
