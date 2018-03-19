{-# LANGUAGE DeriveGeneric #-}
module Types(
    AccountMode(ADD,EDIT),
    encodedCommand,
    decodeCommand
) where

import Data.Serialize as S
import GHC.Generics

-- Account Mode
data AccountMode = ADD | EDIT
    deriving Eq

data Command = Update String
    deriving (Show, Generic)

instance Serialize Command

encodedCommand = S.encode $ Update "update"
decodeCommand bytes = either (const $ error "Invalid command") match $ S.decode bytes
    where match (Update str) = str == "update"