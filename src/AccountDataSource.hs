module AccountDataSource (
    accountAndSystemParameterConfig
) where

-- Data Type Definitions
import SP6.Data.Account
import SP6.Data.ID
import qualified Data.Map.Strict as M
import qualified Data.Array.Unboxed as A

accountAndSystemParameterConfig :: AccountAndSystemParameterConfig
accountAndSystemParameterConfig = AccountAndSystemParameterConfig {
        accountConfig = fakeAccountConfig,
        systemParameter = fakeSystemParameter
    }

fakeAccountConfig :: M.Map UserID Account
fakeAccountConfig = M.fromList [
        (TTM_OFF, Account { accountPassword = "pass1", accountName = "name1", accountACR = fakeAccountACR, accountAOC = fakeAccountAOC }),
        (TTM_OFF_BCC, Account { accountPassword = "pass2", accountName = "name2", accountACR = fakeAccountACR, accountAOC = fakeAccountAOC })
    ]

fakeAccountACR :: A.UArray OC_ID Bool
fakeAccountACR = A.array (OC801, OC808) $ zip [OC801 ..] $ repeat False

fakeAccountAOC = AreaOfControl {
        aocLineOverview           = Just fakeLineOverviewConfig
        , aocMaintenanceMonitor     = False
        , aocTimetableManagement    = False
        , aocRollingStockController = False
        , aocCrewController         = False
        , aocRollingStockManagement = False
    }

fakeLineOverviewConfig = LineOverviewConfig{
        enableGlobalCommand     = False
        , enableEnableRegulation  = False
    }

fakeSystemParameter = SystemParameter
    { departureOffset = 0
    , routeTriggerOffset = 0
    , minimumDwellTime = 0
    , delayDetectionThreshHold = 0
    , intestationStopDetectionTime = 0
    , tunnelLimit = 0
    , runningTimeList = fakeRunningTimeList
    , dwellTimeSet = fakeDwellTime
    , alarmLevel = M.empty
    }

fakeRunningTimeList = RunningTimeLists
    { maximumPerformance   = M.empty
    , fivePercentCoasting  = M.empty
    , eightPercentCoasting = M.empty
    , energySaving         = M.empty
    , fullCoasting         = M.empty
    }

fakeDwellTime = DwellTimeSets
    { dwellTimeSet1 = M.empty
    , dwellTimeSet2 = M.empty
    , dwellTimeSet3 = M.empty
    }
