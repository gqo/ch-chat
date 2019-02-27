module Main where

import Control.Distributed.Process
import Control.Distributed.Process.Node ( initRemoteTable
                                        , runProcess
                                        , newLocalNode
                                        )
import Network.Transport (EndPointAddress(EndPointAddress))
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Concurrent (threadDelay)

import Control.Monad (forever, void)

import Data.ByteString.Char8 (pack)
import System.Environment (getArgs)

import Messages

main :: IO ()
main = do
    [host, port, serverAddrArg] <- getArgs
    let serverAddr = serverAddrArg ++ ":0"
    maybeT <- createTransport host port defaultTCPParameters
    case maybeT of
        Right t -> do
            client <- newLocalNode t initRemoteTable
            runProcess client $ do
                say "Starting client..."
                serverPID <- discoverServer $ addrToNodeId serverAddr
                -- monitor serverPID
                link serverPID

                (sp, rp) <- newChan :: Process (SendPort ChatMessage, ReceivePort ChatMessage)
                
                say "Enter your username: "
                username <- liftIO getLine
                send serverPID $ JoinMessage username sp

                void $ spawnLocal $ forever $ do
                    -- reason <- receiveWait [match serverFailHandler]
                    -- case reason of
                    --     Died -> die "Chat server failure detected. Shutting down client..."
                    msg <- receiveChan rp
                    say $ ppChat msg
                forever $ do
                    msg <- liftIO getLine
                    send serverPID $ ChatMessage username msg
                    liftIO $ threadDelay 1000000
        Left error -> print error

ppChat :: ChatMessage -> String
ppChat (ChatMessage username msg) =
    username ++ ": " ++ msg

addrToNodeId :: String -> NodeId
addrToNodeId addr = NodeId $ EndPointAddress (pack addr)

discoverServer :: NodeId -> Process ProcessId
discoverServer serverID = do
    whereisRemoteAsync serverID "serverPID"
    reply <- expectTimeout 100 :: Process (Maybe WhereIsReply)
    case reply of
        Just (WhereIsReply _ maybeServerPID) ->
            case maybeServerPID of
                Just serverPID -> return serverPID
                Nothing -> discoverServer serverID
        Nothing -> discoverServer serverID

-- serverFailHandler :: ProcessMonitorNotification -> Process DiedReason
-- serverFailHandler (ProcessMonitorNotification _ _ r) =
--     return r