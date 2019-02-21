{-# LANGUAGE DeriveGeneric #-}

module Messages where

import Control.Distributed.Process (SendPort)
import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable (Typeable)

type Username = String

data JoinMessage = JoinMessage {
    user :: Username,
    port :: SendPort ChatMessage
} deriving (Typeable, Generic, Show)

instance Binary JoinMessage

data ChatMessage = ChatMessage {
    from :: Username,
    body :: String
} deriving (Typeable, Generic, Show)

instance Binary ChatMessage