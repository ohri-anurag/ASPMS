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

-- Account Page HTML
account :: String -> T.Text
account uid = LT.toStrict $ renderHtml $ docTypeHtml $ do
    H.head $ do
      H.title "Account Details"
      H.style $ toHtml CSS.accountDetailsCss
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
    -- H.label ! for "accountName" $ toHtml ("Account Name" :: String)
    -- input ! type_ "text" ! name "accountName" ! value (toValue accountName)
    labelledInput "Account Name" accountName

    labelledInput "Account Password" accountPassword

    H.label ! for "accountACR" $ "Account ACR"
    H.div $ mapM_ acrView (assocs accountACR)
    where
        labelledInput :: String -> String -> Html
        labelledInput labelStr val = H.div ! class_ "row" $ do
            H.label ! for nameStr $ toHtml labelStr
            input ! type_ "text" ! name nameStr ! value (toValue val)
            where nameStr = toValue $ filter (' ' /=) $ Prelude.map toLower labelStr

acrView :: (OC_ID, Bool) -> Html
acrView (ocId, isAllowed) = H.div $ do
    markChecked $ input ! type_ "checkbox" ! name (toValue ocIdStr)
    H.label ! for (toValue ocIdStr) $ toHtml ocIdStr
    where
        ocIdStr = show ocId
        markChecked elem = if isAllowed
            then elem ! checked "checked"
            else elem

-- System Parameter Page Helpers
systemParamsView :: SystemParameter -> Html
systemParamsView _ = H.div "System Parameters View Here"
