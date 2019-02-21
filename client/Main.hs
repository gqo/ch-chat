module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)

main :: IO ()
main = do
    -- Boilerplate code until client is written
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    runProcess node $ do
        echoPid <- spawnLocal $ forever $ do
            receiveWait [match logMessage, match replyBack]

        say "send some messages!"
        send echoPid "client"
        self <- getSelfPid
        send echoPid (self, "client")

        m <- expectTimeout 1000000
        case m of
            Nothing -> die "nothing came back!"
            Just s -> say $ "got " ++ s ++ " back!"
        
        liftIO $ threadDelay 2000000