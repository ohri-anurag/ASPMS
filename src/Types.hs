{-# LANGUAGE DeriveGeneric #-}
module Types(
    AccountMode(ADD,EDIT),
    Command(Update, Acknowledgement)
    -- encodedCommand,
    -- decodeCommand
) where

import Data.Serialize as S
import GHC.Generics

-- Account Mode
data AccountMode = ADD | EDIT
    deriving Eq

data Command = Update | Acknowledgement
    deriving (Show, Generic)

instance Serialize Command

-- encodedCommand = S.encode Update
-- decodeCommand bytes = either (const $ error "Invalid command") match $ S.decode bytes
--     where 
--         match Update = True
--         match _ = False