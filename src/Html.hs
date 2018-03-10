{-# LANGUAGE OverloadedStrings #-}

module Html where

-- HTML
-- import Text.Blaze
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text(renderHtml)

-- CSS
import qualified Css as CSS

-- JS
import qualified Js as JS

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

import qualified Data.Map.Strict as M

-- Account Data Types
import SP6.Data.Account
import SP6.Data.ID
import SP6.Data.Command
import Data.Time.Clock(NominalDiffTime)

import Text.Read(readMaybe)
import Data.Array.IArray(assocs)

--- Web Pages ---

-- Login Page HTML
login :: T.Text
login = LT.toStrict $ renderHtml $ H.docTypeHtml $ do
    H.head $ H.title "Login"
    H.body $ H.form ! action "login" ! method "post" $ input ! type_ "password" ! placeholder "Password" ! name "password"

-- Home Page HTML
home :: AccountAndSystemParameterConfig -> T.Text
home accountAndSystemParameterConfig = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Home"
        H.style $ toHtml CSS.homeCss
    H.body $
        H.div ! A.id "container" $ do
            H.div ! A.id "sidebar" $ do
                H.div ! A.id "accountsViewButton" $ "Accounts"
                H.div ! A.id "systemParamsViewButton" $ "System Parameters"
            H.div ! A.id "main" $ accountAndSystemParameterView accountAndSystemParameterConfig

-- Account Page HTML
account :: String -> AccountAndSystemParameterConfig -> T.Text
account uid accountAndSystemParameterConfig = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Account Details"
        H.style $ toHtml CSS.accountDetailsCss
        script ! type_ "text/javascript" $ toHtml $ JS.account
    H.body $ do
        toHtml ("Individual Account Details" :: String)
        H.span ! A.id "userID" $ toHtml uid
        H.div $ case (readMaybe uid :: Maybe UserID) of
            Nothing -> invalidMsg
            Just id -> case M.lookup id (accountConfig accountAndSystemParameterConfig) of
                Nothing -> invalidMsg
                Just acc -> accountDetailedView acc
    where invalidMsg = toHtml ("Invalid User ID" :: String)

-- Running Time List HTML
runningTimeLists :: AccountAndSystemParameterConfig -> T.Text
runningTimeLists accountAndSystemParameterConfig = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Running Time Lists"
    H.body $ do
        h1 "Running Time Lists"
        -- TODO Do this using lenses
        runningTimeListsView $ runningTimeList $ systemParameter accountAndSystemParameterConfig

-- Dwell Time Set HTML
dwellTimeSets :: AccountAndSystemParameterConfig -> T.Text
dwellTimeSets accountAndSystemParameterConfig = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Dwell Time Sets"
    H.body $ do
        h1 "Dwell Time Sets"
        -- TODO Do this using lenses
        dwellTimeSetsView $ dwellTimeSet $ systemParameter accountAndSystemParameterConfig

-- Alarm Levels HTML
alarmLevels :: AccountAndSystemParameterConfig -> T.Text
alarmLevels accountAndSystemParameterConfig = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Alarm Levels"
    H.body $ do
        h1 "Alarm Levels"
        -- TODO Do this using lenses
        mapM_ alarmLevelView $ M.toList $ alarmLevel $ systemParameter accountAndSystemParameterConfig

--- Helpers ---
-- General Helpers
checkbox :: Html -> AttributeValue -> Bool -> Html
checkbox label name' isChecked = H.div $ do
    markChecked $ input ! type_ "checkbox" ! name name' ! A.id name'
    H.label ! for name' $ label
    where
        markChecked elem = if isChecked
            then elem ! checked "checked"
            else elem

labelledInput :: (ToValue a) => Html -> AttributeValue -> a -> Html
labelledInput label' name' val = H.div ! class_ "row" $ do
    H.label ! for name' $ label'
    input ! type_ "text" ! A.id name' ! name name' ! value (toValue val)

-- Home page helpers
accountAndSystemParameterView :: AccountAndSystemParameterConfig -> Html
accountAndSystemParameterView (AccountAndSystemParameterConfig accountsConfig systemParams) = do
    H.div ! A.id "accountsView" $ do
        H.div ! class_ "row header" $ do
            H.div ! class_ "uid" $ "UID"
            H.div ! class_ "accountName" $ "Name"
        accountsView accountsConfig
    H.div ! A.id "systemParamsView" $ systemParamsView systemParams

accountsView :: M.Map UserID Account -> Html
accountsView accountsConfig = mapM_ accountView $ M.toList accountsConfig
    -- H.div $ "Accounts View Here"

accountView :: (UserID, Account) -> Html
accountView (uid, acc) = H.div ! class_ "row" $ do
    H.div ! class_ "uid" $ a ! href (toValue $ "/account/" ++ str) $ toHtml str
    H.div ! class_ "accountName" $ toHtml $ accountName acc
    where str = show uid
    -- toHtml $ "Account for" ++ (show uid)

-- Account Page Helpers
accountDetailedView :: Account -> Html
accountDetailedView (Account accountPassword accountName accountACR accountAOC) = H.form ! A.id "accountForm" ! class_ "form" $ do
    labelledInput "Account Name" "accountName" accountName

    labelledInput "Account Password" "accountPassword" accountPassword

    H.div ! class_ "row" $ do
        H.label ! for "accountACR" $ "Account ACR"
        H.div ! A.id "accountACR" $ mapM_ acrView (assocs accountACR)

    H.div ! class_ "row" $ do
        H.label ! for "accountAOC" $ "Account AOC"
        H.div ! A.id "accountAOC" $ areaOfControlView accountAOC

    -- TODO VALIDATION
    button ! class_ "submit" ! A.id "submit" $ "Edit Account"

acrView :: (OC_ID, Bool) -> Html
acrView (ocId, isAllowed) = checkbox (toHtml ocIdStr) (toValue ocIdStr) isAllowed
    where
        ocIdStr = show ocId

areaOfControlView :: AreaOfControl -> Html
areaOfControlView (AreaOfControl aocLineOverview aocMaintenanceMonitor aocTimetableManagement aocRollingStockController aocCrewController aocRollingStockManagement) = do
    lineOverviewConfigView aocLineOverview
    checkbox "AOC Maintenance Monitor" "aocMaintenanceMonitor" aocMaintenanceMonitor
    checkbox "AOC Timetable Management" "aocTimetableManagement" aocTimetableManagement
    checkbox "AOC Rolling Stock Controller" "aocRollingStockController" aocRollingStockController
    checkbox "AOC Crew Controller" "aocCrewController" aocCrewController
    checkbox "AOC Rolling Stock Management" "aocRollingStockManagement" aocRollingStockManagement

lineOverviewConfigView :: Maybe LineOverviewConfig -> Html
lineOverviewConfigView Nothing = H.div ! A.id "aocLineOverviewDiv" $ do
    checkbox "Line Overview Config" "aocLineOverview" False
    H.div $ do
        checkbox "Enable Global Command" "enableGlobalCommand" False ! disabled "disabled"
        checkbox "Enable Regulation" "enableEnableRegulation" False ! disabled "disabled"
lineOverviewConfigView (Just (LineOverviewConfig enableGlobalCommand enableEnableRegulation)) = H.div ! A.id "aocLineOverviewDiv" $ do
    checkbox "Line Overview Config" "aocLineOverview" True
    H.div ! class_ "row" $ do
        checkbox "Enable Global Command" "enableGlobalCommand" enableGlobalCommand
        checkbox "Enable Regulation" "enableRegulation" enableEnableRegulation

-- System Parameter Page Helpers
systemParamsView :: SystemParameter -> Html
systemParamsView (SystemParameter departureOffset routeTriggerOffset minimumDwellTime delayDetectionThreshHold intestationStopDetectionTime tunnelLimit runningTimeList dwellTimeSet alarmLevel) = H.div $ do
    h1 "System Parameters View Here"

    labelledInput "Departure Offset" "departureOffset" departureOffset
    labelledInput "Route Trigger Offset" "routeTriggerOffset" routeTriggerOffset
    labelledInput "Minimum Dwell Time" "minimumDwellTime" minimumDwellTime
    labelledInput "Delay Detection Threshold" "delayDetectionThreshHold" delayDetectionThreshHold
    -- TODO intestationStopDetectionTime should be interstationStopDetectionTime
    labelledInput "Interstation Stop Detection Time" "interstationStopDetectionTime" intestationStopDetectionTime
    labelledInput "Tunnel Limit" "tunnelLimit" tunnelLimit

    a ! href "/runningTimeLists" $ "Running Time Lists"
    br
    a ! href "/dwellTimeSets" $ "Dwell Time Sets"
    br
    a ! href "alarmLevels" $ "Alarm Levels"

-- Running Time Lists Page Helpers
runningTimeListsView :: RunningTimeLists -> Html
runningTimeListsView (RunningTimeLists maximumPerformance fivePercentCoasting eightPercentCoasting energySaving fullCoasting) = do
    runningTimeListView maximumPerformance
    runningTimeListView fivePercentCoasting
    runningTimeListView eightPercentCoasting
    runningTimeListView energySaving
    runningTimeListView fullCoasting

runningTimeListView :: M.Map (StopPointCode, StopPointCode) NominalDiffTime -> Html
runningTimeListView rtl = mapM_ runningTimeView $ M.toList rtl

runningTimeView :: ((StopPointCode, StopPointCode), NominalDiffTime) -> Html
runningTimeView ((stc1, stc2), diffTime) = labelledInput (toHtml label) (toValue name) (show diffTime)
    where
        stc1Str = show stc1
        stc2Str = show stc2
        label = stc1Str ++ " -> " ++ stc2Str
        name = stc1Str ++ "," ++ stc2Str

-- Dwell Time Sets Page Helpers
dwellTimeSetsView :: DwellTimeSets -> Html
dwellTimeSetsView (DwellTimeSets dwellTimeSet1 dwellTimeSet2 dwellTimeSet3) = do
    dwellTimeSetView dwellTimeSet1
    dwellTimeSetView dwellTimeSet2
    dwellTimeSetView dwellTimeSet3

dwellTimeSetView :: M.Map StopPointCode NominalDiffTime -> Html
dwellTimeSetView dts = mapM_ dwellTimeView $ M.toList dts

dwellTimeView :: (StopPointCode, NominalDiffTime) -> Html
dwellTimeView (spc, diffTime) = labelledInput (toHtml spcStr) (toValue spcStr) (show diffTime)
    where spcStr = show spc

-- Alarm Levels Page Helpers
-- TODO Create a select drop down for alarm level
alarmLevelView :: (EventTag, AlarmLevel) -> Html
alarmLevelView (eTag, aLevel) = labelledInput (toHtml $ eTagStr) (toValue $ eTagStr) (show aLevel)
    where eTagStr = show eTag
