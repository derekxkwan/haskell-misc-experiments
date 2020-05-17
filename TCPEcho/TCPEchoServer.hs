import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Network.Socket{- network -}
import Network.Socket.ByteString (recv, sendAll, send)
import Control.Concurrent
import Control.Monad

hostTuple :: (Word8, Word8, Word8, Word8)
hostTuple = (127,0,0,1)

inPort :: PortNumber
inPort = 8080

maxCnx :: Int
maxCnx = 1024

maxRecv :: Int
maxRecv = 1024 -- max bytes received

main :: IO ()
main = do
  sock <- runTCPServ inPort
  mainLoop sock
  


runTCPServ :: PortNumber -> IO Socket
runTCPServ portno = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock $ SockAddrInet portno $ tupleToHostAddress hostTuple
  listen sock maxCnx
  return sock

mainLoop :: Socket -> IO ()
mainLoop sock = forever $ do
  cnx <- accept sock
  forkIO $ cnxHandler cnx
  

cnxHandler :: (Socket, SockAddr) -> IO ()
cnxHandler (sock, _) = do
  putStrLn "socket accepted!"
  cur_str <- recv sock maxRecv
  send sock $ cur_str
  close sock
