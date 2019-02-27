{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Distributed.Process
import Control.Distributed.Process.Node ( initRemoteTable
                                        , runProcess
                                        , newLocalNode
                                        )
import Network.Transport.TCP (createTransport, defaultTCPParameters)

import Control.Concurrent (threadDelay)

import Control.Monad (forever, forM_)

import qualified Data.Map as Map

import Messages
import Server

main :: IO ()
main = do
    maybeT <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    case maybeT of
        Right t -> do
            server <- newLocalNode t initRemoteTable
            runProcess server $ do
                say "Starting server..."
                self <- getSelfPid
                selfNodeID <- getSelfNode
                say "Got selfPID and selfNodeID"
                registerRemoteAsync selfNodeID "serverPID" self
                say "Registered self on remote table"
                let serverName = "Server" :: Username
                    messages = [] :: [ChatMessage]
                runServer $ ServerState serverName Map.empty messages
                liftIO $ forever $ threadDelay 1000000
        Left error -> print error