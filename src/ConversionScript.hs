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
    { systemParameter :: SystemParameterp
    , accountConfig   :: M.Map UserID2 Accountp
    } deriving (Show, Eq, Ord, Generic)

instance Serialize AccountAndSystemParameterConfigp

data SystemParameterp = SystemParameterp
    { departureOffset               :: NominalDiffTime
    , routeTriggerOffset            :: NominalDiffTime
    , minimumDwellTime              :: NominalDiffTime
    , delayDetectionThreshHold      :: NominalDiffTime
    , interstationStopDetectionTime :: NominalDiffTime
    , tunnelLimit                   :: Int
    , runningTimeLists              :: A.RunningTimeLists
    , dwellTimeSets                 :: A.DwellTimeSets
    , alarmLevel                    :: M.Map EventTag AlarmLevel
    } deriving (Show, Eq, Ord, Generic)

instance Serialize SystemParameterp

data Accountp = Accountp
    { accountPassword :: String
    , accountName     :: String
    , accountACR      :: UArray OC_ID Bool
    , accountAOC      :: AreaOfControlp
    } deriving (Show, Eq, Ord, Generic)

instance Serialize Accountp

data AreaOfControlp = AreaOfControlp
    { aocLineOverview               :: Maybe A.LineOverviewConfig
    , aocMaintenanceMonitor         :: Bool
    , aocTimetableManagement        :: Bool
    , aocEventMonitor               :: Bool
    , aocLineOverviewPlayback       :: Bool
    , aocMaintenanceMonitorPlayback :: Bool
    , aocRollingStockController     :: Bool
    , aocCrewController             :: Bool
    , aocRollingStockManagement     :: Bool
    } deriving (Show, Eq, Ord, Generic)

instance Serialize AreaOfControlp

transform :: AccountAndSystemParameterConfigp -> A.AccountAndSystemParameterConfig
transform old = A.AccountAndSystemParameterConfig
    { A.systemParameter   = transformSystemParameter $ systemParameter old
    , A.accountConfig  = M.map transformAccount $ accountConfig old
    }
    where
        transformSystemParameter old = A.SystemParameter
            { A.departureOffset               = departureOffset old
            , A.routeTriggerOffset            = routeTriggerOffset old
            , A.minimumDwellTime              = minimumDwellTime old
            , A.delayDetectionThreshHold      = delayDetectionThreshHold old
            , A.interstationStopDetectionTime = interstationStopDetectionTime old
            , A.tunnelLimit                   = tunnelLimit old
            , A.wakeUpCommandOffset           = 600
            , A.runningTimeLists              = runningTimeLists old
            , A.dwellTimeSets                 = dwellTimeSets old
            , A.alarmLevel                    = alarmLevel old
            , A.rollingStockRoster            = M.fromList
                                              $ zip (map RakeID [1..])
                                              $ replicate 29
                                              $ T.pack "6 car"
            , A.crewRoster                    = M.empty
            }
        transformAccount old = A.Account
            { A.accountPassword = accountPassword old
            , A.accountName     = accountName old
            , A.accountACR      = accountACR old
            , A.accountAOC      = transformAOC $ accountAOC old
            }
        transformAOC old = A.AreaOfControl
            { A.aocLineOverview               = aocLineOverview old
            , A.aocMaintenanceMonitor         = aocMaintenanceMonitor old
            , A.aocTimetableManagement        = aocTimetableManagement old
            , A.aocEventMonitor               = aocEventMonitor old
            , A.aocLineOverviewPlayback       = aocLineOverviewPlayback old
            , A.aocMaintenanceMonitorPlayback = aocMaintenanceMonitorPlayback old
            , A.aocRollingStockController     = aocRollingStockController old
            , A.aocCrewController             = aocCrewController old
            , A.aocRollingStockManagement     = aocRollingStockManagement old
            , A.aocHandleFaultData            = False
            }

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
