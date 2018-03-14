{-# LANGUAGE OverloadedStrings #-}

module Html where

import Control.Monad(join)
import Data.Maybe(isJust,maybe,fromJust)

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
import Types
import SP6.Data.Account
import SP6.Data.ID
import SP6.Data.Command
import Data.Time.Clock(NominalDiffTime)

import Text.Read(readMaybe)
import Data.Array.Unboxed(assocs)

--- Web Pages ---

-- Login Page HTML
login :: T.Text
login = LT.toStrict $ renderHtml $ H.docTypeHtml $ do
    H.head $ H.title "Login"
    H.body $ H.form ! action "login" ! method "post" $ do
        input ! type_ "password" ! placeholder "Password" ! name "password"
        input ! type_ "submit" ! value "Login"

-- TODO Logout Button
-- Home Page HTML
home :: AccountAndSystemParameterConfig -> T.Text
home accountAndSystemParameterConfig = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Home"
        H.style $ toHtml CSS.homeCss
        script ! type_ "text/javascript" $ toHtml $ JS.home
    H.body $
        H.div ! A.id "container" $ do
            H.div ! A.id "sidebar" $ do
                H.div ! A.id "accountsViewButton" ! class_ "button" $ "Accounts"
                H.div ! A.id "systemParamsViewButton" ! class_ "button" $ "System Parameters"
                a ! href "/changePassword" ! A.id "changePassword" ! class_ "button" $ "Change Password"
                br
                a ! href "/logout" ! A.id "logout" ! class_ "button" $ "Logout"
            H.div ! A.id "main" $ accountAndSystemParameterView accountAndSystemParameterConfig

-- Change Password Page HTML
changePassword :: T.Text
changePassword = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Change Password"
    H.body $ H.form ! action "changePassword" ! method "post" $ do
        input ! type_ "password" ! placeholder "Password" ! name "password"
        input ! type_ "submit" ! value "Save Changes"

-- Account Page HTML
editAccount :: T.Text -> AccountAndSystemParameterConfig -> T.Text
editAccount uid = account EDIT (Just $ UserID2 $ T.unpack uid)

addAccount :: AccountAndSystemParameterConfig -> T.Text
addAccount = account ADD Nothing

account :: AccountMode -> Maybe UserID2 -> AccountAndSystemParameterConfig -> T.Text
account mode uid accountAndSystemParameterConfig = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title accountTitle
        H.style $ toHtml CSS.accountDetailsCss
        script ! type_ "text/javascript" $ toHtml $ JS.account mode
    H.body $ do
        userIdDisplay
        accountDetails
    where
        (UserID2 uid') = fromJust uid
        invalidMsg = toHtml ("Invalid User ID" :: String)
        accountTitle = if mode == EDIT then "Account Details" else "Add Account"
        userIdDisplay = if mode == EDIT
            then do
                toHtml ("Individual Account Details" :: String)
                H.span ! A.id "userID" $ toHtml uid'
            else labelledInput "User Id" "userID" "Enter User ID here" (Nothing :: Maybe String)
        accountDetails = if mode == EDIT
            then H.div $ case M.lookup (UserID2 uid') (accountConfig accountAndSystemParameterConfig) of
                Nothing -> invalidMsg
                Just acc -> accountDetailedView (Just acc)
            else accountDetailedView Nothing


-- Running Time List HTML
runningTimeLists :: AccountAndSystemParameterConfig -> T.Text
runningTimeLists accountAndSystemParameterConfig = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Running Time Lists"
        H.style $ toHtml CSS.runningTimeListsCss
        script ! type_ "text/javascript" $ toHtml $ JS.runningTimeLists
    H.body $ do
        h1 "Running Time Lists"
        -- TODO Do this using lenses
        H.div ! A.id "container" $ do
            H.div ! A.id "sidebar" $ do
                H.div ! A.id "maximumPerformanceButton" ! class_ "button" $ "Maximum Performance"
                H.div ! A.id "fivePercentCoastingButton" ! class_ "button" $ "Five Percent Coasting"
                H.div ! A.id "eightPercentCoastingButton" ! class_ "button" $ "Eight Percent Coasting"
                H.div ! A.id "energySavingButton" ! class_ "button" $ "Energy Saving"
                H.div ! A.id "fullCoastingButton" ! class_ "button" $ "Full Coasting"
                button ! A.id "saveButton" $ "Save Changes"
                H.div ! A.id "cumulativeError" ! class_ "error" $ "Changes could not be saved because form contains errors."
            H.div ! A.id "main" $ runningTimeListsView $ runningTimeList $ systemParameter accountAndSystemParameterConfig

-- Dwell Time Set HTML
dwellTimeSets :: AccountAndSystemParameterConfig -> T.Text
dwellTimeSets accountAndSystemParameterConfig = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Dwell Time Sets"
        H.style $ toHtml CSS.dwellTimeSetsCss
        script ! type_ "text/javascript" $ toHtml $ JS.dwellTimeSets
    H.body $ do
        h1 "Dwell Time Sets"
        -- TODO Do this using lenses
        H.div ! A.id "container" $ do
            H.div ! A.id "sidebar" $ do
                H.div ! A.id "dwellTimeSet1Button" ! class_ "button" $ "Dwell Time Set 1"
                H.div ! A.id "dwellTimeSet2Button" ! class_ "button" $ "Dwell Time Set 2"
                H.div ! A.id "dwellTimeSet3Button" ! class_ "button" $ "Dwell Time Set 3"
                button ! A.id "saveButton" $ "Save Changes"
                H.div ! A.id "cumulativeError" ! class_ "error" $ "Changes could not be saved because form contains errors."
        H.div ! A.id "main" $ dwellTimeSetsView $ dwellTimeSet $ systemParameter accountAndSystemParameterConfig

-- Alarm Levels HTML
alarmLevels :: AccountAndSystemParameterConfig -> T.Text
alarmLevels accountAndSystemParameterConfig = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Alarm Levels"
        H.style $ toHtml CSS.alarmLevelsCss
        script ! type_ "text/javascript" $ toHtml $ JS.alarmLevels
    H.body $ do
        h1 "Alarm Levels"
        -- TODO Do this using lenses
        mapM_ alarmLevelView $ M.toList $ alarmLevel $ systemParameter accountAndSystemParameterConfig
        button ! A.id "saveButton" $ "Save Changes"

--- Helpers ---
-- General Helpers
checkbox :: Html -> AttributeValue -> Bool -> Bool -> Html
checkbox label name' isChecked isDisabled = H.div $ do
    markDisabled $ markChecked $ input ! type_ "checkbox" ! name name' ! A.id name'
    H.label ! for name' $ label
    where
        markChecked elem = if isChecked
            then elem ! checked "checked"
            else elem
        markDisabled elem = if isDisabled
            then elem ! disabled "disabled"
            else elem

-- Creates a label input pair, and fills the input with the given value
labelledInput :: (ToValue a) => Html -> AttributeValue -> String -> Maybe a -> Html
labelledInput label' name' holder val = H.div ! class_ "row" $ do
    H.label ! class_ "rowElem" ! for name' $ label'
    addValue $ input ! class_ "rowElem" ! type_ "text" ! A.id name' ! name name' ! placeholder (toValue holder)
    where
        addValue elem = maybe elem ((!) elem . value . toValue) val

-- Home page helpers
accountAndSystemParameterView :: AccountAndSystemParameterConfig -> Html
accountAndSystemParameterView (AccountAndSystemParameterConfig accountsConfig systemParams) = do
    H.div ! A.id "accountsView" ! class_ "tab" $ do
        H.div ! class_ "row header" $ do
            H.div ! class_ "uid rowElem" $ "UID"
            H.div ! class_ "accountName rowElem" $ "Name"
        accountsView accountsConfig
        a ! href "/addAccount" $ button "Add Account"
    H.div ! A.id "systemParamsView" ! class_ "tab" $ systemParamsView systemParams

accountsView :: M.Map UserID2 Account -> Html
accountsView accountsConfig = mapM_ accountView $ M.toList accountsConfig

accountView :: (UserID2, Account) -> Html
accountView (UserID2 uid, acc) = H.div ! class_ "row" $ do
    H.div ! class_ "rowElem uid" $ a ! href (toValue $ "/account/" ++ uid) $ toHtml uid
    H.div ! class_ "rowElem accountName" $ toHtml $ accountName acc

-- Account Page Helpers
accountDetailedView :: Maybe Account -> Html
accountDetailedView account = H.form ! A.id "accountForm" ! class_ "form" $ do
    labelledInput "Account Name" "accountName" "Enter Account Name here" $ accountName <$> account

    labelledInput "Account Password" "accountPassword" "Enter Account Password here" $ accountPassword <$> account

    H.div ! class_ "row" $ do
        H.label ! for "accountACR" ! class_ "rowElem" $ "Account ACR"
        H.div ! A.id "accountACR" ! class_ "rowElem" $ mapM_ acrView $ acrList

    H.div ! class_ "row" $ do
        H.label ! for "accountAOC" ! class_ "rowElem" $ "Account AOC"
        H.div ! A.id "accountAOC" ! class_ "rowElem" $ areaOfControlView $ accountAOC <$> account

    -- TODO VALIDATION
    button ! class_ "submit" ! A.id "submit" $ maybe "Add Account" (const "Save Changes") account
    where
        acrList = maybe (zip [minBound..maxBound] $ repeat False) Prelude.id (assocs <$> accountACR <$> account)

acrView :: (OC_ID, Bool) -> Html
acrView (ocId, isAllowed) = checkbox (toHtml ocIdStr) (toValue ocIdStr) isAllowed False
    where
        ocIdStr = show ocId

areaOfControlView :: Maybe AreaOfControl -> Html
areaOfControlView areaOfControl = do
    lineOverviewConfigView $ aocLineOverview <$> areaOfControl
    checkbox "AOC Maintenance Monitor" "aocMaintenanceMonitor" (toBool aocMaintenanceMonitor) False
    checkbox "AOC Timetable Management" "aocTimetableManagement" (toBool aocTimetableManagement) False
    checkbox "AOC Rolling Stock Controller" "aocRollingStockController" (toBool aocRollingStockController) False
    checkbox "AOC Crew Controller" "aocCrewController" (toBool aocCrewController) False
    checkbox "AOC Rolling Stock Management" "aocRollingStockManagement" (toBool aocRollingStockManagement) False
    where
        toBool f = maybe False Prelude.id (f <$> areaOfControl)

lineOverviewConfigView :: Maybe (Maybe LineOverviewConfig) -> Html
lineOverviewConfigView lineOverviewConfig = H.div ! A.id "aocLineOverviewDiv" $ do
    checkbox "Line Overview Config" "aocLineOverview" isChecked False
    H.div ! class_ "row" $ do
        checkbox "Enable Global Command" "enableGlobalCommand" (toBool enableGlobalCommand $ join lineOverviewConfig) (not isChecked)
        checkbox "Enable Regulation" "enableRegulation" (toBool enableRegulation $ join lineOverviewConfig) (not isChecked)
    where
        toBool f v = maybe False Prelude.id (f <$> v)
        isChecked = toBool isJust lineOverviewConfig

-- System Parameter Page Helpers
systemParamsView :: SystemParameter -> Html
systemParamsView (SystemParameter departureOffset routeTriggerOffset minimumDwellTime delayDetectionThreshHold intestationStopDetectionTime tunnelLimit runningTimeList dwellTimeSet alarmLevel) = H.div $ do
    h1 "System Parameters View Here"

    H.div ! A.id "form" $ do
        labelledInput "Departure Offset" "departureOffset" "Enter Departure Offset here" $ Just departureOffset
        labelledInput "Route Trigger Offset" "routeTriggerOffset" "Enter Route Trigger Offset here" $ Just routeTriggerOffset
        labelledInput "Minimum Dwell Time" "minimumDwellTime" "Enter Minimum Dwell Time here" $ Just minimumDwellTime
        labelledInput "Delay Detection Threshold" "delayDetectionThreshHold" "Enter Delay Detection Threshold here" $ Just delayDetectionThreshHold
        -- TODO intestationStopDetectionTime should be interstationStopDetectionTime
        labelledInput "Interstation Stop Detection Time" "interstationStopDetectionTime" "Enter Interstation Stop Detection Time here" $ Just intestationStopDetectionTime
        labelledInput "Tunnel Limit" "tunnelLimit" "Enter Tunnel Limit here" $ Just tunnelLimit
        button ! A.id "saveButton" $ "Save"


    a ! href "/runningTimeLists" $ "Running Time Lists"
    br
    a ! href "/dwellTimeSets" $ "Dwell Time Sets"
    br
    a ! href "alarmLevels" $ "Alarm Levels"

-- Running Time Lists Page Helpers
runningTimeListsView :: RunningTimeLists -> Html
runningTimeListsView (RunningTimeLists maximumPerformance fivePercentCoasting eightPercentCoasting energySaving fullCoasting) = do
    H.div ! A.id "maximumPerformance" ! class_ "runningTimeList" $ do
        h2 "Maximum Performance"
        runningTimeListView "mpf" maximumPerformance
    H.div ! A.id "fivePercentCoasting" ! class_ "runningTimeList" $ do
        h2 "Five Percent Coasting"
        runningTimeListView "fpc" fivePercentCoasting
    H.div ! A.id "eightPercentCoasting" ! class_ "runningTimeList" $ do
        h2 "Eight Percent Coasting"
        runningTimeListView "epc" eightPercentCoasting
    H.div ! A.id "energySaving" ! class_ "runningTimeList" $ do
        h2 "Energy Saving"
        runningTimeListView "esg" energySaving
    H.div ! A.id "fullCoasting" ! class_ "runningTimeList" $ do
        h2 "Full Coasting"
        runningTimeListView "fcg" fullCoasting

-- ASSUMPTION - Code must have only 3 letters
runningTimeListView :: String -> M.Map (StopPointCode, StopPointCode) NominalDiffTime -> Html
runningTimeListView code rtl = mapM_ (runningTimeView code) $ M.toList rtl

runningTimeView :: String -> ((StopPointCode, StopPointCode), NominalDiffTime) -> Html
runningTimeView code ((stc1, stc2), diffTime) = labelledInput (toHtml label) (toValue name) "Enter Running Time here" (Just $ init $ show diffTime)
    where
        stc1Str = show stc1
        stc2Str = show stc2
        label = stc1Str ++ " -> " ++ stc2Str
        name = code ++ stc1Str ++ "," ++ stc2Str

-- Dwell Time Sets Page Helpers
dwellTimeSetsView :: DwellTimeSets -> Html
dwellTimeSetsView (DwellTimeSets dwellTimeSet1 dwellTimeSet2 dwellTimeSet3) = do
    H.div ! A.id "dwellTimeSet1" ! class_ "dwellTimeSet" $ do
        h2 "Dwell Time Set 1"
        dwellTimeSetView "d1" dwellTimeSet1
    H.div ! A.id "dwellTimeSet2" ! class_ "dwellTimeSet" $ do
        h2 "Dwell Time Set 2"
        dwellTimeSetView "d2" dwellTimeSet2
    H.div ! A.id "dwellTimeSet3" ! class_ "dwellTimeSet" $ do
        h2 "Dwell Time Set 3"
        dwellTimeSetView "d3" dwellTimeSet3

-- ASSUMPTION - Code must have only 2 letters
dwellTimeSetView :: String -> M.Map StopPointCode NominalDiffTime -> Html
dwellTimeSetView code dts = mapM_ (dwellTimeView code) $ M.toList dts

dwellTimeView :: String -> (StopPointCode, NominalDiffTime) -> Html
dwellTimeView code (spc, diffTime) = labelledInput (toHtml spcStr) (toValue $ code ++ spcStr) "Enter Dwell Time here" (Just $ init $ show diffTime)
    where spcStr = show spc

-- Alarm Levels Page Helpers
alarmLevelView :: (EventTag, AlarmLevel) -> Html
alarmLevelView (eTag, aLevel) = H.div ! class_ "row" $ do
    H.label ! class_ "rowElem" ! for (toValue eTagStr) $ toHtml eTagStr
    selectList
    where
        eTagStr = show eTag
        selectList = select ! class_ "rowElem" ! A.id (toValue eTagStr) $ mapM_ optionify [minBound..maxBound]
        optionify level = markSelected level $ option ! A.value (toValue lstr) $ (toHtml lstr)
            where lstr = show level
        markSelected level elem = if aLevel == level
            then elem ! selected "selected"
            else elem
