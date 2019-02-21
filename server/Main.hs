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

type Clients = Map.Map Username (SendPort ChatMessage)

data ServerState = ServerState {
    serverName :: Username,
    clientMap :: Clients,
    messages :: [ChatMessage]
} deriving (Show)

broadcast :: Clients -> ChatMessage -> Process ()
broadcast clients msg = forM_ clients (`sendChan` msg)

joinHandler :: ServerState -> JoinMessage -> Process ServerState
joinHandler (ServerState name clients messages) (JoinMessage user port) = do
    let clients' = Map.insert user port clients
        msg = user ++ " has joined the server..."
        chatMsg = ChatMessage name msg
        messages' = chatMsg:messages
        state' = ServerState name clients' messages'
    
    -- Log message
    -- print $ show chatMsg

    broadcast clients chatMsg
    return state'

chatHandler :: ServerState -> ChatMessage -> Process ServerState
chatHandler (ServerState name clients messages) msg = do
    let messages' = msg:messages
        state' = ServerState name clients messages'

    -- Log message
    -- print $ show chatMsg
    
    broadcast clients msg
    return state'

runServer :: ServerState -> Process ()
runServer state = do
    state' <- receiveWait [
          match $ joinHandler state
        , match $ chatHandler state]
    runServer state'

main :: IO ()
main = do
    maybeT <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    case maybeT of
        Right t -> do
            server <- newLocalNode t initRemoteTable
            runProcess server $ do
                let serverName = "Server" :: Username
                    messages = [] :: [ChatMessage]
                runServer $ ServerState serverName Map.empty messages
                liftIO $ forever $ threadDelay 1000000
        Left error -> print error
    
    
    -- server <- newLocalNode t initRemoteTable
    -- runProcess server $ spawnLocal $ do
    --     self <- getSelfPid
    --     liftIO $


-- data ServerConfig = ServerConfig {
--     _serverName :: Username,
--     _serverPID :: ProcessId
-- } deriving (Show)

-- newtype ServerAction a = ServerAction {
--     runAction :: RWS ServerConfig [ChatMessage] ServerState a
-- } deriving ( Functor
--            , Applicative
--            , Monad
--            )

-- broadcast :: ChatMessage -> ServerAction ()
-- broadcast msg = do 
--     forM_ clientMap (`sendChan` msg)
--     tell [msg]

-- joinHandler :: JoinMessage -> ServerAction ()
-- joinHandler (JoinMessage user port) = do
--     ServerConfig serverName _ <- ask
    
--     -- Map.insert user port clientMap


--     let msg = user ++ " has joined the server..."
--     -- logMessage $ ChatMessage serverName msg
--     broadcast clientMap $ ChatMessage serverName msg

-- joinHandler :: JoinMessage -> ServerAction ()
-- joinHandler (JoinMessage user port) = do

-- execute :: Monad m => (t -> ServerAction a) -> t -> m (ServerConfig -> ServerState -> (ServerState, [ChatMessage]))
-- execute msgHandler msg = return $ execRWS (runAction $ msgHandler msg)

-- runServer :: ServerConfig -> ServerState -> Process ()
-- runServer config state = do
--     (state', _) <- receiveWait [match $ execute joinHandler]
--     runServer config state'

-- chatHandler :: ChatMessage -> ServerState
-- chatHandler msg = say $ "Chat message received: " ++ show msg

-- joinHandler :: JoinMessage -> Process ()
-- joinHandler msg = say $ "Join message received: " ++ show msg

-- runServer :: ServerState -> Process ()
-- runServer state = do
--     (state') <- receiveWait [
--           match $ chatHandler msg)
--         , match $ (\msg -> joinHandler msg)
--         , matchUnknown $ runServer state
--         ]
--     say $ "Current state: " ++ show state'
--     runServer state'