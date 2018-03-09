{-# LANGUAGE OverloadedStrings #-}

module Html where

-- HTML
-- import Text.Blaze
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text(renderHtml)

-- CSS
import qualified Css as CSS

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

import qualified Data.Map.Strict as M

import Data.Char(toLower)

import SP6.Data.Account
import SP6.Data.ID
import AccountDataSource

import Text.Read(readMaybe)
import Data.Array.IArray(assocs)

--- Web Pages ---

-- Login Page HTML
login :: T.Text
login = LT.toStrict $ renderHtml $ H.docTypeHtml $ do
    H.head $ H.title "Login"
    H.body $ H.form ! action "login" ! method "post" $ input ! type_ "password" ! placeholder "Password" ! name "password"

-- Home Page HTML
home :: T.Text
home = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Home"
        H.style $ toHtml CSS.homeCss
    H.body $
        H.div ! A.id "container" $ do
            H.div ! A.id "sidebar" $ do
                H.div ! A.id "accountsViewButton" $ "Accounts"
                H.div ! A.id "systemParamsViewButton" $ "System Parameters"
            H.div ! A.id "main" $ accountAndSystemParameterView accountAndSystemParameterConfig

-- TODO Read JS from a static file
-- Account Page HTML
account :: String -> T.Text
account uid = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
        H.title "Account Details"
        H.style $ toHtml CSS.accountDetailsCss
        script ! type_ "text/javascript" $ toHtml $
            "window.onload = function() {" ++
            "   var checkbox = document.getElementById('lineOverviewConfig')," ++
            "       child1 = document.getElementById('enableGlobalCommand')," ++
            "       child2 = document.getElementById('enableRegulation');" ++
            "   checkbox.onchange = function() {" ++
            "      if (checkbox.checked) {" ++
            "          child1.removeAttribute('disabled');" ++
            "          child2.removeAttribute('disabled');" ++
            "      }" ++
            "      else {" ++
            "          child1.checked = false;" ++
            "          child2.checked = false;" ++
            "          child1.setAttribute('disabled', 'disabled');" ++
            "          child2.setAttribute('disabled', 'disabled');" ++
            "      }" ++
            "   };" ++
            "};"
    H.body $ do
        toHtml ("Individual Account Details" ++ uid)
        H.div $ case (readMaybe uid :: Maybe UserID) of
            Nothing -> invalidMsg
            Just id -> case M.lookup id (accountConfig accountAndSystemParameterConfig) of
                Nothing -> invalidMsg
                Just acc -> accountDetailedView acc
    -- TODO
    where invalidMsg = toHtml ("Invalid User ID" :: String)

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
accountDetailedView (Account accountPassword accountName accountACR accountAOC) = H.form ! class_ "form" ! action "account" ! method "post" $ do
    labelledInput "Account Name" "accountName" accountName

    labelledInput "Account Password" "accountPassword" accountPassword

    H.div ! class_ "row" $ do
        H.label ! for "accountACR" $ "Account ACR"
        H.div ! A.id "accountACR" $ mapM_ acrView (assocs accountACR)

    H.div ! class_ "row" $ do
        H.label ! for "accountAOC" $ "Account AOC"
        H.div ! A.id "accountAOC" $ areaOfControlView accountAOC

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
lineOverviewConfigView Nothing = H.div $ do
    checkbox "Line Overview Config" "lineOverviewConfig" False
    H.div $ do
        checkbox "Enable Global Command" "enableGlobalCommand" False ! disabled "disabled"
        checkbox "Enable Regulation" "enableRegulation" False ! disabled "disabled"

lineOverviewConfigView (Just (LineOverviewConfig enableGlobalCommand enableEnableRegulation)) = H.div $ do
    checkbox "Line Overview Config" "lineOverviewConfig" True
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

    a ! href "/runningTimeList" $ "Running Time Lists"
    br
    a ! href "/dwellTimeSet" $ "Dwell Time Sets"
    br
    a ! href "alarmLevel" $ "Alarm Levels"
