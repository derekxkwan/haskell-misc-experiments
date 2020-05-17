import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Network.Socket{- network -}
import Network.Socket.ByteString (recv, sendAll, send)
import Control.Concurrent
import Control.Monad

hostTuple :: (Word8, Word8, Word8, Word8)
hostTuple = (127,0,0,1)

outPort :: PortNumber
outPort = 8080

maxCnx :: Int
maxCnx = 1024

maxRecv :: Int
maxRecv = 1024 -- max bytes received

main :: IO ()
main = do
  sock <- runTCPClient outPort
  socketHandler sock
  


runTCPClient :: PortNumber -> IO Socket
runTCPClient portno = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  connect sock $ SockAddrInet portno $ tupleToHostAddress hostTuple
  return sock

socketHandler :: Socket -> IO ()
socketHandler sock = do
  putStr "Input: "
  ipt <- getLine
  sendAll sock $ BSU.fromString ipt
  cur_str <- recv sock maxRecv
  putStrLn $ "Received: " <> (BSU.toString cur_str)
  close sock
  
  
