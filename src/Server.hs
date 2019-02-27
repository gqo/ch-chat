module Server where

import Control.Distributed.Process

import Control.Monad (forM_)
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

sendHistory :: [ChatMessage] -> SendPort ChatMessage -> Process ()
sendHistory messages port = forM_ (reverse messages) (port `sendChan`)

joinHandler :: ServerState -> JoinMessage -> Process ServerState
joinHandler (ServerState name clients messages) (JoinMessage user port) = do
    say "Received a join message..."
    let clients' = Map.insert user port clients
        msg = user ++ " has joined the server..."
        chatMsg = ChatMessage name msg
    
    sendHistory messages port

    say $ show chatMsg
    broadcast clients chatMsg

    let messages' = chatMsg:messages
        state' = ServerState name clients' messages'
        
    return state'

chatHandler :: ServerState -> ChatMessage -> Process ServerState
chatHandler (ServerState name clients messages) msg = do
    say "Received a chat message..."
    let messages' = msg:messages
        state' = ServerState name clients messages'

    say $ show msg
    
    broadcast clients msg
    return state'

runServer :: ServerState -> Process ()
runServer state = do
    state' <- receiveWait [
          match $ joinHandler state
        , match $ chatHandler state]
    runServer state'