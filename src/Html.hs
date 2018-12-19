{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Html where

import Control.Monad(join)
import Data.Maybe(isJust,maybe,fromJust)
import Data.List(partition)

-- HTML
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text(renderHtml)

import qualified Data.Array as AR
-- CSS
import qualified Css as CSS

-- JS
import qualified Js as JS

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

import qualified Data.Map.Strict as M

-- Account Data Types
import Types
import Utility(version)
import SP6.Data.Account
import SP6.Data.ID
import SP6.Data.Command
import SP6.Data.Common((<!))
import Data.Time.Clock(NominalDiffTime)

import Data.Array.Unboxed(assocs)

--- Web Pages ---

--Dialog Box HTML
dialog :: Html
dialog = do
    H.div ! A.id "screen" $ ""
    H.div ! A.id "dialog" $ do
        H.span ! A.id "dialogText" $ ""
        button ! A.id "dialogButton" $ "OK"

-- Login Page HTML
login :: T.Text
login = LT.toStrict $ renderHtml $ H.docTypeHtml $ do
    H.head $ do
        H.title "Login"
        H.style $ toHtml CSS.loginCss
        script ! type_ "text/javascript" $ toHtml JS.login
    H.body $ H.form ! method "post" $ do
        input ! type_ "password" ! placeholder "Password" ! name "password"
        input ! type_ "submit" ! value "Login"

-- Home Page HTML
home :: AccountAndSystemParameterConfig -> T.Text
home accountAndSystemParameterConfig = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Home"
        H.style $ toHtml CSS.homeCss
        script ! type_ "text/javascript" $ toHtml JS.home
    H.body $ do
        dialog
        H.div ! A.id "confirm" $ do
            H.span ! A.id "confirmText" $ "Test"
            button ! A.id "yes" $ "Yes"
            button ! A.id "no" $ "No"
        H.div ! A.id "container" $ do
            H.div ! A.id "sidebar" $ do
                H.div ! A.id "tabContainer" $ do
                    H.div ! A.id "accountsViewButton" ! class_ "button" $ "Accounts"
                    H.div ! A.id "systemParamsViewButton" ! class_ "button" $ "System Parameters"
                H.div ! A.id "systemParamsRemaining" $ do
                    a ! href "/runningTimeLists" $ H.div ! A.id "runningTimeLists" $ "Running Time Lists"
                    a ! href "/dwellTimeSets" $ H.div ! A.id "dwellTimeSets" $ "Dwell Time Sets"
                    a ! href "/alarmLevels" $ H.div ! A.id "alarmLevels" $ "Alarm Levels"
                    a ! href "/rollingStockRoster" $ H.div ! A.id "rollingStockRoster" $ "Rolling Stock Roster"
                    a ! href "/crewRoster" $ H.div ! A.id "crewRoster" $ "Crew Roster"
                H.div ! A.id "addAccountDiv" $
                    a ! href "/addAccount" $ H.div ! A.id "addAccount" $ "Add Account"
                H.div ! A.id "saveLoadDataDiv" $ do
                    H.label ! for "loadDataButton" $ "Load Data"
                    input ! A.id "loadDataButton" ! type_ "file"
                    button ! A.id "saveDataButton" $ "Save Data"
                H.div ! A.id "applyDiv" $ button ! A.id "apply" $ "Apply Changes"
                H.div ! A.id "versionDiv" $ toHtml $ "Version : " ++ version
                H.div ! A.id "linkContainer" $ do
                    a ! href "/changePassword" $ H.div ! A.id "changePassword" $ "Change Password"
                    a ! href "/logout/cc" $ H.div ! A.id "logout" $ "Logout"
            H.div ! A.id "main" $ accountAndSystemParameterView accountAndSystemParameterConfig

-- Change Password Page HTML
changePassword :: T.Text
changePassword = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Change Password"
        H.style $ toHtml CSS.changePasswordCss
        script ! type_ "text/javascript" $ toHtml JS.changePassword
    H.body $ do
        dialog
        H.div ! A.id "container" $
            H.div ! A.id "sidebar" $
                H.div ! A.id "linkContainer" $ a ! href "/home" $ H.div ! A.id "home" $ "Home"
        H.div ! A.id "main" $ do
            H.div ! A.id "passwordDiv" $ do
                H.label ! for "password" ! class_ "rowElem" $ "Enter new Password"
                input ! type_ "password" ! class_ "rowElem" ! placeholder "Password" ! name "password" ! A.id "password"
            input ! type_ "submit" ! A.id "saveButton" ! value "Save Changes"

-- Account Page HTML
editAccount :: T.Text -> AccountAndSystemParameterConfig -> T.Text
editAccount uid = accountHtml EDIT (Just $ UserID2 $ T.unpack uid)

addAccount :: AccountAndSystemParameterConfig -> T.Text
addAccount = accountHtml ADD Nothing

accountHtml :: AccountMode -> Maybe UserID2 -> AccountAndSystemParameterConfig -> T.Text
accountHtml mode uid accountAndSystemParameterConfig = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title accountTitle
        H.style $ toHtml CSS.accountDetailsCss
        script ! type_ "text/javascript" $ toHtml $ JS.account mode
    H.body $ do
        dialog
        H.div ! A.id "container" $ do
            H.div ! A.id "sidebar" $
                H.div ! A.id "linkContainer" $ a ! href "/home" $ H.div ! A.id "home" $ "Home"
            H.div ! A.id "main" $ do
                h1 accountTitle
                userIdDisplay
                accountDetails
    where
        (UserID2 uid') = fromJust uid
        invalidMsg = toHtml ("Invalid User ID" :: String)
        accountTitle = if mode == EDIT then "Account Details" else "Add Account"
        userIdDisplay = if mode == EDIT
            then b $ do
                _ <- "Individual Account Details for "
                H.span ! A.id "userID" $ toHtml uid'
            else labelledInput "User Id" "userID" "Enter User ID here" (Nothing :: Maybe String)
        accountDetails = if mode == EDIT
            then H.div $ case M.lookup (UserID2 uid') (accountConfig accountAndSystemParameterConfig) of
                Nothing -> invalidMsg
                Just acc -> accountDetailedView (Just acc)
            else accountDetailedView Nothing


-- Running Time List HTML
runningTimeListsPage :: AccountAndSystemParameterConfig -> T.Text
runningTimeListsPage accountAndSystemParameterConfig = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Running Time Lists"
        H.style $ toHtml CSS.runningTimeListsCss
        script ! type_ "text/javascript" $ toHtml JS.runningTimeLists
    H.body $ do
        dialog
        H.div ! A.id "container" $ do
            H.div ! A.id "sidebar" $ do
                H.div ! A.id "tabContainer" $ do
                    H.div ! A.id "maximumPerformanceButton" ! class_ "button" $ "Maximum Performance"
                    H.div ! A.id "fivePercentCoastingButton" ! class_ "button" $ "Five Percent Coasting"
                    H.div ! A.id "eightPercentCoastingButton" ! class_ "button" $ "Eight Percent Coasting"
                    H.div ! A.id "energySavingButton" ! class_ "button" $ "Energy Saving"
                    H.div ! A.id "fullCoastingButton" ! class_ "button" $ "Full Coasting"
                H.div ! A.id "saveAndError" $ do
                    H.div ! A.id "cumulativeError" ! class_ "error" $ "Changes could not be saved because form contains errors."
                    button ! A.id "saveButton" $ "Save Changes"
                H.div ! A.id "linkContainer" $ a ! href "/home" $ H.div ! A.id "home" $ "Home"
                H.div ! A.id "import" $ do
                    H.label ! for "importButton" $ "Import Running Times"
                    input ! A.id "importButton" ! type_ "file"
            H.div ! A.id "main" $ do
                h1 "Running Time Lists"
                runningTimeListsView $ runningTimeLists $ systemParameter accountAndSystemParameterConfig

-- Dwell Time Set HTML
dwellTimeSetsPage :: AccountAndSystemParameterConfig -> T.Text
dwellTimeSetsPage accountAndSystemParameterConfig = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Dwell Time Sets"
        H.style $ toHtml CSS.dwellTimeSetsCss
        script ! type_ "text/javascript" $ toHtml JS.dwellTimeSets
    H.body $ do
        dialog
        H.div ! A.id "container" $
            H.div ! A.id "sidebar" $ do
                H.div ! A.id "tabContainer" $ do
                    H.div ! A.id "dwellTimeSet1Button" ! class_ "button" $ "Dwell Time Set 1"
                    H.div ! A.id "dwellTimeSet2Button" ! class_ "button" $ "Dwell Time Set 2"
                    H.div ! A.id "dwellTimeSet3Button" ! class_ "button" $ "Dwell Time Set 3"
                H.div ! A.id "saveAndError" $ do
                    H.div ! A.id "cumulativeError" ! class_ "error" $ "Changes could not be saved because form contains errors."
                    button ! A.id "saveButton" $ "Save Changes"
                H.div ! A.id "linkContainer" $ a ! href "/home" $ H.div ! A.id "home" $ "Home"
        H.div ! A.id "main" $ do
            h1 "Dwell Time Sets"
            dwellTimeSetsView $ dwellTimeSets $ systemParameter accountAndSystemParameterConfig

-- Alarm Levels HTML
alarmLevels :: AccountAndSystemParameterConfig -> T.Text
alarmLevels accountAndSystemParameterConfig = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Alarm Levels"
        H.style $ toHtml CSS.alarmLevelsCss
        script ! type_ "text/javascript" $ toHtml JS.alarmLevels
    H.body $ do
        dialog
        H.div ! A.id "container" $
            H.div ! A.id "sidebar" $ do
                H.div ! A.id "saveAndError" $
                    button ! A.id "saveButton" $ "Save Changes"
                H.div ! A.id "linkContainer" $ a ! href "/home" $ H.div ! A.id "home" $ "Home"
        H.div ! A.id "main" $ do
            h1 "Alarm Levels"
            mapM_ alarmLevelView $ M.toList $ alarmLevel $ systemParameter accountAndSystemParameterConfig

rollingStockRosterPage :: AccountAndSystemParameterConfig -> T.Text
rollingStockRosterPage accountAndSystemParameterConfig = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Rolling Stock Roster"
        H.style $ toHtml CSS.rollingStockRosterCss
        script ! type_ "text/javascript" $ toHtml JS.rollingStockRoster
    H.body $ do
        dialog
        H.div ! A.id "container" $
            H.div ! A.id "sidebar" $ do
                H.div ! A.id "saveAndError" $
                    button ! A.id "saveButton" $ "Save Changes"
                H.div ! A.id "linkContainer" $ a ! href "/home" $ H.div ! A.id "home" $ "Home"
        H.div ! A.id "main" $ do
            h1 "Rolling Stock Roster"
            H.div ! class_ "rowCover" $ do
                H.div ! class_ "check" $
                    input ! type_ "checkbox"
                          ! A.id "headerCheck"
                          ! name "headerCheck"
                H.div ! class_ "row header" $ do
                    H.div ! class_ "rowElem" $ "Rake ID"
                    H.div ! class_ "rowElem" $ "Description"
            mapM_ rollingStockView $ zip [0..] $ M.toList $ rollingStockRoster $ systemParameter accountAndSystemParameterConfig
            H.div $ do
                button ! A.id "addRow" $ "Add Rolling Stock"
                button ! A.id "delRow" $ "Delete Rolling Stock"

crewRosterPage :: AccountAndSystemParameterConfig -> T.Text
crewRosterPage accountAndSystemParameterConfig = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Crew Roster"
        H.style $ toHtml CSS.crewRosterCss
        script ! type_ "text/javascript" $ toHtml JS.crewRoster
    H.body $ do
        dialog
        H.div ! A.id "container" $
            H.div ! A.id "sidebar" $ do
                H.div ! A.id "saveAndError" $
                    button ! A.id "saveButton" $ "Save Changes"
                H.div ! A.id "linkContainer" $ a ! href "/home" $ H.div ! A.id "home" $ "Home"
        H.div ! A.id "main" $ do
            h1 "Crew Roster"
            H.div ! class_ "rowCover" $ do
                H.div ! class_ "check" $
                    input ! type_ "checkbox"
                          ! A.id "headerCheck"
                          ! name "headerCheck"
                H.div ! class_ "row header" $ do
                    H.div ! class_ "rowElem" $ "Crew ID"
                    H.div ! class_ "rowElem" $ "Description"
            mapM_ crewView $ zip [0..] $ M.toList $ crewRoster $ systemParameter accountAndSystemParameterConfig
            H.div $ do
                button ! A.id "addRow" $ "Add Crew"
                button ! A.id "delRow" $ "Delete Crew"

--- Helpers ---
-- General Helpers
checkbox :: Html -> AttributeValue -> Bool -> Bool -> Html
checkbox checkBoxLabel name' isChecked isDisabled = H.div $ do
    markDisabled $ markChecked $ input ! type_ "checkbox" ! name name' ! A.id name'
    H.label ! for name' $ checkBoxLabel
    where
        markChecked tag = if isChecked
            then tag ! checked "checked"
            else tag
        markDisabled tag = if isDisabled
            then tag ! disabled "disabled"
            else tag

simpleInput :: (ToValue a) =>AttributeValue -> String -> a -> Html
simpleInput name' holder val =
    input ! class_ "rowElem"
          ! type_ "text"
          ! A.id name'
          ! name name'
          ! placeholder (toValue holder)
          ! value (toValue val)

-- Creates a label input pair, and fills the input with the given value
labelledInput :: (ToValue a) => Html -> AttributeValue -> String -> Maybe a -> Html
labelledInput label' name' holder val = H.div ! class_ "row" $ do
    H.label ! class_ "rowElem" ! for name' $ label'
    addValue $ input ! class_ "rowElem" ! type_ "text" ! A.id name' ! name name' ! placeholder (toValue holder)
    where
        addValue tag = maybe tag ((!) tag . value . toValue) val

showStopPoint :: StopPointCode -> String
showStopPoint spc = maybe (show spc) (\(sc,pn) -> show sc ++ " " ++ show pn) (arrSpCodeToStCode AR.! spc)

-- Home page helpers
accountAndSystemParameterView :: AccountAndSystemParameterConfig -> Html
accountAndSystemParameterView (AccountAndSystemParameterConfig systemParams accountsConfig) = do
    H.div ! A.id "accountsView" ! class_ "tab" $ do
        h1 "Accounts"
        H.div ! class_ "row header" $ do
            H.div ! class_ "uid rowElem" $ "User ID"
            H.div ! class_ "accountName rowElem" $ "Name"
        accountsView accountsConfig
    H.div ! A.id "systemParamsView" ! class_ "tab" $ systemParamsView systemParams

accountsView :: M.Map UserID2 Account -> Html
accountsView accountsConfig = mapM_ accountView $ M.toList accountsConfig

accountView :: (UserID2, Account) -> Html
accountView (UserID2 uid, acc) = H.div ! class_ "row" $ do
    H.div ! class_ "rowElem uid" $ a ! href (toValue $ "/account/" ++ uid) $ toHtml uid
    H.div ! class_ "rowElem accountName" $ toHtml $ accountName acc
    H.div ! class_ "rowElem delete" $ button ! A.id (toValue uid) ! class_ "deleteButton" $ "Delete Account"

-- Account Page Helpers
accountDetailedView :: Maybe Account -> Html
accountDetailedView account = H.form ! A.id "accountForm" ! class_ "form" $ do
    labelledInput "Account Name" "accountName" "Enter Account Name here" $ accountName <$> account

    labelledInput "Account Password" "accountPassword" "Enter Account Password here" $ accountPassword <$> account

    H.div ! class_ "row" $ do
        H.label ! for "accountACR" ! class_ "rowElem" $ "Account ACR"
        H.div ! A.id "accountACR" ! class_ "rowElem" $ mapM_ acrView acrList

    H.div ! class_ "row" $ do
        H.label ! for "accountAOC" ! class_ "rowElem" $ "Account AOC"
        H.div ! A.id "accountAOC" ! class_ "rowElem" $ areaOfControlView $ accountAOC <$> account

    button ! class_ "submit" ! A.id "submit" $ maybe "Add Account" (const "Save Changes") account
    where
        acrList = maybe (zip [minBound..maxBound] $ repeat False) (assocs . accountACR) account

acrView :: (OC_ID, Bool) -> Html
acrView (ocId, isAllowed) = checkbox (toHtml labelStr) (toValue idStr) isAllowed False
    where
        idStr = show ocId
        labelStr = show $ arrOCToStCode AR.! ocId

areaOfControlView :: Maybe AreaOfControl -> Html
areaOfControlView areaOfControl = do
    lineOverviewConfigView $ aocLineOverview <$> areaOfControl
    checkbox "AOC Maintenance Monitor" "aocMaintenanceMonitor" (toBool aocMaintenanceMonitor) False
    checkbox "AOC Timetable Management" "aocTimetableManagement" (toBool aocTimetableManagement) False
    checkbox "AOC Event Monitor" "aocEventMonitor" (toBool aocEventMonitor) False
    checkbox "AOC Line Overview Playback" "aocLineOverviewPlayback" (toBool aocLineOverviewPlayback) False
    checkbox "AOC Maintenance Monitor Playback" "aocMaintenanceMonitorPlayback" (toBool aocMaintenanceMonitorPlayback) False
    checkbox "AOC Rolling Stock Controller" "aocRollingStockController" (toBool aocRollingStockController) False
    checkbox "AOC Crew Controller" "aocCrewController" (toBool aocCrewController) False
    checkbox "AOC Rolling Stock Management" "aocRollingStockManagement" (toBool aocRollingStockManagement) False
    checkbox "AOC Handle Fault Data" "aocHandleFaultData" (toBool aocHandleFaultData) False
    where
        toBool f = maybe False f areaOfControl

lineOverviewConfigView :: Maybe (Maybe LineOverviewConfig) -> Html
lineOverviewConfigView lineOverviewConfig = H.div ! A.id "aocLineOverviewDiv" $ do
    checkbox "AOC Line Overview Config" "aocLineOverview" isChecked False
    H.div ! class_ "form" $ do
        checkbox "Enable Global Command" "enableGlobalCommand" (toBool enableGlobalCommand $ join lineOverviewConfig) (not isChecked)
        checkbox "Enable Regulation" "enableRegulation" (toBool enableRegulation $ join lineOverviewConfig) (not isChecked)
    where
        toBool = maybe False
        isChecked = toBool isJust lineOverviewConfig

-- System Parameter Page Helpers
systemParamsView :: SystemParameter -> Html
systemParamsView SystemParameter{..} = H.div $ do
    h1 "System Parameters"

    H.div ! A.id "form" $ do
        labelledInput "Departure Offset(In seconds)" "departureOffset" "Enter Departure Offset here" $ Just $ init $ show departureOffset
        labelledInput "Route Trigger Offset(In seconds)" "routeTriggerOffset" "Enter Route Trigger Offset here" $ Just $ init $ show routeTriggerOffset
        labelledInput "Minimum Dwell Time(In seconds)" "minimumDwellTime" "Enter Minimum Dwell Time here" $ Just $ init $ show minimumDwellTime
        labelledInput "Delay Detection Threshold(In seconds)" "delayDetectionThreshHold" "Enter Delay Detection Threshold here" $ Just $ init $ show delayDetectionThreshHold
        labelledInput "Interstation Stop Detection Time(In seconds)" "interstationStopDetectionTime" "Enter Interstation Stop Detection Time here" $ Just $ init $ show interstationStopDetectionTime
        labelledInput "Tunnel Limit(Number of Trains)" "tunnelLimit" "Enter Tunnel Limit here" $ Just tunnelLimit
        labelledInput "WakeUp Command Offset(In seconds)" "wakeUpCommandOffset" "Enter WakeUp Command Offset here" $ Just $ init $ show wakeUpCommandOffset
        button ! A.id "saveButton" $ "Save Changes"

-- Running Time Lists Page Helpers
runningTimeListsView :: RunningTimeLists -> Html
runningTimeListsView RunningTimeLists{..} = do
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
runningTimeListView rtlCode rtl = do
    H.div ! class_ "header row" $ do
        H.div ! A.id "fromTo" ! class_ "rowElem" $ do
            H.div ! class_ "from" $ "From"
            H.div ! class_ "to" $ "To"
        H.div ! class_ "rowElem" $ "Running Time(In seconds)"
    mapM_ (runningTimeView rtlCode) $ sortPlatform $ M.toList rtl
    where
        sortPlatform xss = pl1 ++ reverse pl2 ++ cross
            where
                (same, cross) = partition (samePlatform . fst) xss
                (pl1, pl2) = partition ((==) PL1 . platform . fst . fst) same

        samePlatform (sp1, sp2) = platform sp1 == platform sp2

        platform spc = pl
            where
                (Just (_,pl)) = arrSpCodeToStCode <! spc

runningTimeView :: String -> ((StopPointCode, StopPointCode), NominalDiffTime) -> Html
runningTimeView rtlCode ((stc1, stc2), diffTime) = hide $ labelledInput rtLabel (toValue rtName) "Enter Running Time here" (Just $ init $ show diffTime)
    where
        stc1Str = showStopPoint stc1
        stc2Str = showStopPoint stc2
        rtLabel = do
            H.div ! class_ "from" $ toHtml stc1Str
            H.div ! class_ "to" $ toHtml stc2Str
        rtName = rtlCode ++ show stc1 ++ "," ++ show stc2
        hide tag
            -- Corner case, do not show or modify this field, but include it in the data
            | diffTime > 9990   = tag ! dataAttribute "noshow" "true"
            | otherwise         = tag

-- Dwell Time Sets Page Helpers
dwellTimeSetsView :: DwellTimeSets -> Html
dwellTimeSetsView DwellTimeSets{..} = do
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
dwellTimeSetView dtsCode dts = do
    H.div ! class_ "header row" $ do
        H.div ! class_ "rowElem" $ "Stop Point"
        H.div ! class_ "rowElem" $ "Dwell Time(In seconds)"
    mapM_ (dwellTimeView dtsCode) $ M.toList dts

dwellTimeView :: String -> (StopPointCode, NominalDiffTime) -> Html
dwellTimeView dtsCode (spc, diffTime) = labelledInput (toHtml labelStr) (toValue $ dtsCode ++ idStr) "Enter Dwell Time here" (Just $ init $ show diffTime)
    where
        labelStr = showStopPoint spc
        idStr = show spc

-- Alarm Levels Page Helpers
alarmLevelView :: (EventTag, AlarmLevel) -> Html
alarmLevelView (eTag, aLevel) = H.div ! class_ "row" $ do
    H.label ! class_ "rowElem" ! for (toValue eTag') $ toHtml (drop 8 eTag')
    selectList
    where
        eTag' = show eTag
        selectList = select ! class_ "rowElem" ! A.id (toValue eTag') $ mapM_ optionify [minBound..maxBound]
        optionify level = markSelected level $ option ! A.value (toValue lstr) $ toHtml lstr
            where lstr = show level
        markSelected level tag = if aLevel == level
            then tag ! selected "selected"
            else tag

-- Rolling Stock Roster Page Helpers
rollingStockView :: (Int, (RakeID, T.Text)) -> Html
rollingStockView (index, (rakeID, desc)) = H.div ! class_ "rowCover" $ do
    H.div ! class_ "check" $
        input ! type_ "checkbox"
              ! A.id checkIDStr
              ! name checkIDStr
    H.div ! class_ "row" $ do
        simpleInput rakeIDStr "Enter Rake ID Here" (show rakeID)
        simpleInput descIDStr "Enter Rake Description Here" desc
    where
        rakeIDStr = toValue $ "rake" ++ show index
        descIDStr = toValue $ "desc" ++ show index
        checkIDStr = toValue $ "check" ++ show index

-- Crew Roster Page Helpers
crewView :: (Int, (CrewID, T.Text)) -> Html
crewView (index, (crewID, desc)) = H.div ! class_ "rowCover" $ do
    H.div ! class_ "check" $
        input ! type_ "checkbox"
              ! A.id checkIDStr
              ! name checkIDStr
    H.div ! class_ "row" $ do
        simpleInput crewIDStr "Enter Crew ID Here" (show $ unCrewID crewID)
        simpleInput descIDStr "Enter Crew Description Here" desc
    where
        crewIDStr = toValue $ "crew" ++ show index
        descIDStr = toValue $ "desc" ++ show index
        checkIDStr = toValue $ "check" ++ show index
