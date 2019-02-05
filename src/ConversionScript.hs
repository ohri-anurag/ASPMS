{-# LANGUAGE DeriveGeneric #-}

-- A script that converts previous data into new format
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B

import qualified SP6.Data.Account as A
import SP6.Data.ID
import SP6.Data.Command

import Data.Array.Unboxed
import qualified Data.Text as T
import Data.Time.Clock
import Data.Serialize (Serialize (..), encode, decode)
import GHC.Generics (Generic)

-- Previous Format
data AccountAndSystemParameterConfigp = AccountAndSystemParameterConfigp
    { systemParameter :: A.SystemParameter
    , accountConfig   :: M.Map UserID2 Accountp
    } deriving (Show, Eq, Ord, Generic)

instance Serialize AccountAndSystemParameterConfigp

data Accountp = Accountp
    { accountPassword :: String
    , accountName     :: String
    , accountACR      :: UArray OC_ID Bool
    , accountAOC      :: AreaOfControlp
    } deriving (Show, Eq, Ord, Generic)

instance Serialize Accountp

data AreaOfControlp = AreaOfControlp
    { aocLineOverview               :: Maybe LineOverviewConfigp
    , aocMaintenanceMonitor         :: Bool
    , aocTimetableManagement        :: Bool
    , aocEventMonitor               :: Bool
    , aocLineOverviewPlayback       :: Bool
    , aocMaintenanceMonitorPlayback :: Bool
    , aocRollingStockController     :: Bool
    , aocCrewController             :: Bool
    , aocRollingStockManagement     :: Bool
    , aocHandleFaultData            :: Bool
    } deriving (Show, Eq, Ord, Generic)

instance Serialize AreaOfControlp

data LineOverviewConfigp = LineOverviewConfigp
    { enableGlobalCommand    :: !Bool
    , enableRegulation :: !Bool
    } deriving (Show, Eq, Ord, Generic)

instance Serialize LineOverviewConfigp

transform :: AccountAndSystemParameterConfigp -> A.AccountAndSystemParameterConfig
transform old = A.AccountAndSystemParameterConfig
    { A.systemParameter   = systemParameter old
    , A.accountConfig  = M.mapWithKey transformAccount $ accountConfig old
    }
    where
        transformAccount userId old = A.Account
            { A.accountPassword = accountPassword old
            , A.accountName     = accountName old
            , A.accountACR      = accountACR old
            , A.accountAOC      = transformAOC userId $ accountAOC old
            }
        transformAOC userId old = A.AreaOfControl
            { A.aocLineOverview               = transformAocLineOverview userId <$> aocLineOverview old
            , A.aocMaintenanceMonitor         = aocMaintenanceMonitor old
            , A.aocTimetableManagement        = aocTimetableManagement old
            , A.aocEventMonitor               = aocEventMonitor old
            , A.aocLineOverviewPlayback       = aocLineOverviewPlayback old
            , A.aocMaintenanceMonitorPlayback = aocMaintenanceMonitorPlayback old
            , A.aocRollingStockController     = aocRollingStockController old
            , A.aocCrewManagement             = aocCrewController old
            , A.aocRollingStockManagement     = aocRollingStockManagement old
            , A.aocHandleFaultData            = aocHandleFaultData old
            }
        transformAocLineOverview userId old = A.LineOverviewConfig
            { A.enableGlobalCommand = enableGlobalCommand old
            , A.enableRegulation = regulationMode
            }
            where
                regulationMode = do
                    defaultAccount <- M.lookup userId A.defaultAccountConfig
                    defaultLineOverviewConfig <- A.aocLineOverview $ A.accountAOC defaultAccount
                    A.enableRegulation defaultLineOverviewConfig

convert :: String -> String -> IO ()
convert old new = do
    -- Read the old format file
    bstr <- B.readFile old

    either putStrLn (
        --Write to new format
        B.writeFile new
        -- Encode the new format to a bytestring
        . encode
        -- Transform the old format to a new bytestring
        . transform
        )
        -- Decode the file contents to the old format
        (decode bstr)
