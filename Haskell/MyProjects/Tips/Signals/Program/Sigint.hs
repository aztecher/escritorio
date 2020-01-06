import Control.Concurrent
import System.Posix.Signals

-- use these library in cleanly
import Control.Concurrent.MVar


-- ==============================
--  Simple Example
-- ==============================
simple :: IO ()
simple = do
    tid <- myThreadId
    let handler = do
        putStrLn "goodbye!"
        killThread tid
    installHandler keyboardSignal (Catch handler) Nothing

    let loop n = do
        putStr $ show n ++ ", "
        threadDelay 1000000
        loop (n + 1)
    loop 0

-- ==============================
--  Cleanly Example
-- ==============================

handler :: MVar Int -> IO ()
handler s_interrupted = modifyMVar_ s_interrupted (return . (+1))

main :: IO ()
main = withContext $ \ctx ->
  withSocket ctx Rep $ \socket -> do
    bind socket "tcp://*:5555"
    s_interrupted <- newMVar 0
    installHandler sigINT (Catch $ handler s_interrupted) Nothing
    installHandler sigTERM (Catch $ handler s_interrupted) Nothing
    recvFunction s_interrupted socket

recvFunction :: (Ord a, Num a, Receiver b) => MVar a -> Socket b -> IO ()
recvFunction mi sock = do
  receive sock
  withMVar mi (\val -> if val > 0
    then putStrLn "W: Interrupt Received. Kill Server"
    else recvFunction mi sock)

cleanly :: IO ()

