module AccountDataSource (
    accountAndSystemParameterConfig
) where

-- Data Type Definitions
import SP6.Data.Account
import SP6.Data.ID
import qualified Data.Map.Strict as M
import qualified Data.Array.IArray as A

accountAndSystemParameterConfig :: AccountAndSystemParameterConfig
accountAndSystemParameterConfig = AccountAndSystemParameterConfig {
        accountConfig = fakeAccountConfig,
        systemParameter = undefined
    }

fakeAccountConfig :: M.Map UserID Account
fakeAccountConfig = M.fromList [
        (TTM_OFF, Account { accountPassword = "pass1", accountName = "name1", accountACR = A.array (OC801,OC801) [], accountAOC = fakeAccountAOC }),
        (TTM_OFF_BCC, Account { accountPassword = "pass2", accountName = "name2", accountACR = A.array (OC801,OC801) [], accountAOC = fakeAccountAOC })
    ]

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

-- Account
--     { accountPassword :: String
--     , accountName     :: String
--     , accountACR      :: UArray OC_ID Bool
--     , accountAOC      :: AreaOfControl
--     }

-- Doubts
-- 1. Why maybe??