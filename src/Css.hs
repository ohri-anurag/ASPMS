{-# LANGUAGE OverloadedStrings #-}

module Css(
    loginCss,
    homeCss,
    accountDetailsCss,
    runningTimeListsCss,
    dwellTimeSetsCss,
    alarmLevelsCss,
    changePasswordCss
) where

import Clay as C
import Prelude hiding ((**),div)
import Data.Monoid((<>))
import qualified Data.Text.Lazy as T

-- HELPER CSS --
validation :: Css
validation = ".error" ? do
    color $ rgb 153 0 0
    padding (px 10) (px 10) (px 10) (px 10)
    margin (px 3) (px 3) (px 3) (px 3)

dialogCss :: Css
dialogCss = do
    "#screen" ? do
        position fixed
        width $ pct 100
        height $ pct 100
        backgroundColor black
        opacity 0
        zIndex (-1)
    "#dialog" ? do
        position fixed
        width $ pct 16
        height $ pct 10
        top $ pct 45
        left $ pct 42
        zIndex (-1)
        opacity 0
        backgroundColor gray
    "#dialogText" ? do
        fontSize $ px 18
        position absolute
        padding (px 10) (px 10) (px 10) (px 10)
        margin (px 0) auto (px 0) auto
        top $ px 0
        left $ px 0
        width $ pct 100
    "#dialogButton" ? do
        position absolute
        padding (px 2) (px 2) (px 2) (px 2)
        bottom $ px 10
        left $ pct 45
        width $ pct 10

zeroPM :: Css
zeroPM = star ? do
    margin (px 0) (px 0) (px 0) (px 0)
    padding (px 0) (px 0) (px 0) (px 0)

common :: Css
common = body ? do
    fontFamily ["arial", "helvetica"] [sansSerif]
    backgroundColor grey
    star ? do
        boxSizing borderBox
        textAlign center

sidebar :: Css
sidebar = do
    "#container" ? do
        position absolute
        height $ pct 100
        width $ pct 100
        display flex
    "#main" ? do
        position absolute
        width $ pct 84
        height $ pct 100
        right $ px 0
    "#sidebar" ? do
        position fixed
        width $ pct 15
        height $ pct 100
        borderRight solid (px 2) black
    "#sidebar" ** div ? do
        fontSize $ px 18
    "#tabContainer" <> "#linkContainer" ? do
        position fixed
        width $ pct 15
    "#tabContainer" ? top (px 20)
    "#linkContainer" ? bottom (px 20)
    ".button" <> "#linkContainer" ** div ? padding (px 5) (px 5) (px 5) (px 5)
    ".button" # hover ? do
        cursor pointer
        backgroundColor $ grayish 96
        textDecoration underline

labelledInputCss :: Css
labelledInputCss = do
    ".row" ? do
        marginLeft $ pct 30
        width $ pct 40
        display flex
    ".header" ? do
        fontWeight bold
        marginTop (px 30)
    ".rowElem" ? do
        background $ grayish 220
        padding (px 10) (px 10) (px 10) (px 10)
    ".header" |> div <> ".row" |> label <> ".row" |> input ? do
        width (pct 50)
        margin (px 3) (px 3) (px 3) (px 3)

saveAndError :: Css
saveAndError = do
    "#saveAndError" ? do
        position fixed
        height $ pct 20
        width $ pct 15
        top $ pct 40
    "#saveButton" ? padding (px 5) (px 5) (px 5) (px 5)
    "#cumulativeError" ? visibility hidden

-- PAGE CSS --
loginCss :: T.Text
loginCss = render $ do
    common
    "input[type=password]" ? do
        position absolute
        width $ pct 10
        height $ px 30
        top $ pct 45
        left $ pct 45
    "input[type=submit]" ? do
        position absolute
        width $ pct 4
        height $ px 30
        top $ pct 50
        left $ pct 48

homeCss :: T.Text
homeCss = render $ do
    validation
    dialogCss
    zeroPM
    common
    sidebar
    "#applyDiv" ? do
        position fixed
        width $ pct 15
        height $ pct 10
        top $ pct 45
    "#apply" ? do
        padding (px 10) (px 10) (px 10) (px 10)
        fontSize $ px 18
    "#apply" # hover ? cursor pointer
    "#versionDiv" ? do
        position fixed
        width $ pct 15
        top $ pct 70
    ".tab" ? do
        position absolute
        height $ pct 100
        width $ pct 100
    "#systemParamsView" <> "#systemParamsRemaining" ? display none
    "#accountsView .row" ? do
        marginLeft $ pct 30
        width $ pct 60
        display flex
    "#systemParamsView .row" ? do
        marginLeft $ pct 30
        width $ pct 40
        display flex
    ".header" ? fontWeight bold
    ".header" <> "#form" ? marginTop (px 30)
    ".header" ** ".accountName" ? marginRight (pct 33)
    ".rowElem" ? do
        background $ grayish 220
        padding (px 10) (px 10) (px 10) (px 10)
    ".uid" <> ".accountName" <> ".delete" ? width (pct 33)
    ".uid" ? margin (px 3) (px 3) (px 3) (px 0)
    ".accountName" ? margin (px 3) (px 0) (px 3) (px 3)
    ".delete" ? background grey
    "#form label" <> "#form input" ? do
        width (pct 50)
        margin (px 3) (px 3) (px 3) (px 3)
    "#saveButton" ? do
        marginTop $ px 10
        fontSize $ px 15
    ".deleteButton" <> "#saveButton" ? padding (px 2) (px 2) (px 2) (px 2)
    "#systemParamsRemaining" <> "#addAccountDiv" ? do
        position fixed
        top $ pct 10
        width $ pct 15
    "#systemParamsRemaining" ** div ? padding (px 5) (px 5) (px 5) (px 5)
    "#confirm" ? do
        position fixed
        width $ pct 20
        height $ pct 12
        top $ pct 44
        left $ pct 40
        zIndex (-1)
        opacity 0
        backgroundColor gray
    "#confirmText" ? do
        fontSize $ px 18
        position absolute
        padding (px 10) (px 10) (px 10) (px 10)
        margin (px 0) auto (px 0) auto
        top $ px 0
        left $ px 0
        width $ pct 100
    "#yes" <> "#no" ? do
        position absolute
        padding (px 2) (px 2) (px 2) (px 2)
        bottom $ px 10
        width $ pct 10
    "#yes" ? left (pct 25)
    "#no" ? left (pct 65)

accountDetailsCss :: T.Text
accountDetailsCss = render $ do
    validation
    dialogCss
    zeroPM
    common
    sidebar
    labelledInputCss
    ".rowElem" ? do
        width (pct 50)
        margin (px 3) (px 3) (px 3) (px 3)
    ".form" ? do
        display flex
        flexDirection column
    ("#accountAOC" <> "#accountACR") ** div ? textAlign (alignSide sideLeft)
    "#accountAOC" |> div ? marginLeft (px 20)
    "#aocLineOverviewDiv .form" ? do
        marginLeft $ px 30
    "#accountACR" ** div ? do
        width $ pct 30
        margin (px 0) auto (px 0) auto
    "#submit" ? do
        width $ px 150
        padding (px 5) (px 5) (px 5) (px 5)
        margin (px 10) auto (px 0) auto
        fontSize $ px 15
    "input[type=checkbox]" ? margin (px 2) (px 2) (px 2) (px 2)

runningTimeListsCss :: T.Text
runningTimeListsCss = render $ do
    validation
    dialogCss
    zeroPM
    common
    sidebar
    labelledInputCss
    saveAndError
    "#import" ? do
        position fixed
        width $ pct 15
        top $ pct 60
    "#import" ** label ? fontWeight bold
    "#importButton" ? do
        marginTop (px 10)
        padding (px 10) (px 0) (px 10) (px 0)
        borderTop solid (px 2) black
        borderBottom solid (px 2) black
    label # ".rowElem" <> "#fromTo" ? do
        backgroundColor grey
        display flex
        padding (px 0) (px 0) (px 0) (px 0)
    ".from" <> ".to" ? do
        backgroundColor (grayish 220)
        width $ pct 50
        padding (px 10) (px 10) (px 10) (px 10)
        margin (px 0) (px 0) (px 0) (px 0)
    ".from" ? marginRight (px 3)
    ".to" ? marginLeft (px 3)
    ".runningTimeList" ? do
        position absolute
        width $ pct 100
        height $ pct 100
    "#maximumPerformance" ? visibility visible
    "#fivePercentCoasting" ? visibility hidden
    "#eightPercentCoasting" ? visibility hidden
    "#energySaving" ? visibility hidden
    "#fullCoasting" ? visibility hidden
    "div" # ("data-noshow" *= "true") ? display none

dwellTimeSetsCss :: T.Text
dwellTimeSetsCss = render $ do
    validation
    dialogCss
    zeroPM
    common
    sidebar
    labelledInputCss
    saveAndError
    ".dwellTimeSet" ? do
        position absolute
        width $ pct 100
        height $ pct 100
    "#dwellTimeSet1" ? visibility visible
    "#dwellTimeSet2" ? visibility hidden
    "#dwellTimeSet3" ? visibility hidden

alarmLevelsCss :: T.Text
alarmLevelsCss = render $ do
    dialogCss
    zeroPM
    common
    sidebar
    labelledInputCss
    saveAndError
    select ? do
        width $ pct 30
        margin (px 3) (px 3) (px 3) (px 3)
    label # ".rowElem" ? width (pct 70)

changePasswordCss :: T.Text
changePasswordCss = render $ do
    zeroPM
    common
    validation
    dialogCss
    sidebar
    ".rowElem" ? do
        background $ grayish 220
        padding (px 10) (px 10) (px 10) (px 10)
    "#passwordDiv" ? do
        position absolute
        width $ pct 40
        top $ pct 45
        left $ pct 30
        display flex
    "#passwordDiv" ** star ? do
        width $ pct 50
        margin (px 3) (px 3) (px 3) (px 3)
    "input[type=submit]" ? do
        position absolute
        width $ pct 8
        height $ px 30
        top $ pct 55
        left $ pct 46
