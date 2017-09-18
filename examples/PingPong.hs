import Control.Concurrent.Fiber
import qualified Control.Concurrent.MVar as MVar

pingpong :: String -> MVar () -> MVar () -> Fiber ()
pingpong msg sourceChan sinkChan = go 0
 where go n = do
         takeMVar sourceChan
         liftIO $ putStrLn $ show n ++ ": " ++ msg
         putMVar sinkChan ()
         go (n + 1)

main :: IO ()
main = do
  pingChan <- newEmptyMVar
  pongChan <- newEmptyMVar
  forkFiber $ pingpong "Ping" pingChan pongChan
  forkFiber $ pingpong "Pong" pongChan pingChan
  -- Start the chain from the main thread!
  MVar.putMVar pingChan ()
  -- Wait 1 second
  threadDelay 1000000
  return ()
