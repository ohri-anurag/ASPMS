{-# LANGUAGE OverloadedStrings #-}

module Css(
    loginCss,
    homeCss,
    accountDetailsCss,
    runningTimeListsCss,
    dwellTimeSetsCss,
    alarmLevelsCss
) where

import Clay as C
import Prelude hiding ((**),div)
import Data.Monoid((<>))
import qualified Data.Text.Lazy as T

-- HELPER CSS --
validation :: Css
validation = ".error" ? do
    color red
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
        textAlign center
        position absolute
        padding (px 10) (px 10) (px 10) (px 10)
        margin (px 0) auto (px 0) auto
        top $ px 0
        width $ pct 100
    "#dialogButton" ? do
        position absolute
        padding (px 2) (px 2) (px 2) (px 2)
        margin (px 0) auto (px 0) auto
        bottom $ px 10
        left $ pct 45
        width $ pct 10

zeroPM :: Css
zeroPM = star ? do
    margin (px 0) (px 0) (px 0) (px 0)
    padding (px 0) (px 0) (px 0) (px 0)

-- PAGE CSS --
loginCss :: T.Text
loginCss = render $ do
    body ? backgroundColor grey
    "input[type=password]" ? do
        position absolute
        width $ pct 10
        height $ px 30
        top $ pct 45
        left $ pct 45
        textAlign center
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
    "#container" ? do
        position absolute
        height $ pct 100
        width $ pct 100
        display flex
    "#sidebar" ? do
        width $ pct 15
        borderRight solid (px 2) black
    "#sidebar" ** div ? do
        padding (px 5) (px 5) (px 5) (px 5)
    "#sidebar" ** div # hover ? do
        cursor pointer
        backgroundColor grey
    a ? do
        textDecoration none
        padding (px 0) (px 0) (px 0) (px 0)
        color black
    a # visited ? color black
    a # active ? color black
    a # hover ? color black
    "#main" ?  do
        position absolute
        width $ pct 84
        height $ pct 100
        right $ px 0
    ".tab" ? do
        position absolute
        height $ pct 100
        width $ pct 100
    "#accountsView" ? visibility visible
    "#systemParamsView" ? visibility hidden
    -- ".button" # hover ? cursor pointer
    ".row" ? do
        width $ pct 100
        display flex
    ".header" ? fontWeight bold
    ".rowElem" ? do
        background $ grayish 220
        padding (px 10) (px 10) (px 10) (px 10)
        margin (px 3) (px 3) (px 3) (px 3)
    ".uid" ? width (pct 20)
    ".accountName" ? width (pct 20)
    "#form label" ? width (pct 20)
    "#form input" ? width (pct 20)

accountDetailsCss :: T.Text
accountDetailsCss = render $ do
    validation
    dialogCss
    zeroPM
    ".row" ? do
        width $ pct 100
        display flex
    ".rowElem" ? do
        background $ grayish 220
        padding (px 10) (px 10) (px 10) (px 10)
        margin (px 3) (px 3) (px 3) (px 3)
    ".form" ? do
        display flex
        flexDirection column

runningTimeListsCss :: T.Text
runningTimeListsCss = render $ do
    validation
    dialogCss
    zeroPM
    ".row" ? do
        width $ pct 100
        display flex
    ".rowElem" ? do
        background $ grayish 220
        padding (px 10) (px 10) (px 10) (px 10)
        margin (px 3) (px 3) (px 3) (px 3)
    "#container" ? do
        position absolute
        height $ pct 100
        width $ pct 100
        display flex
    "#sidebar" ? do
        position fixed
        width $ pct 15
        height $ pct 100
        borderRight solid (px 2) black
    "#main" ? do
        position absolute
        width $ pct 84
        height $ pct 100
        right $ px 0
    ".runningTimeList" ? do
        position absolute
        width $ pct 100
        height $ pct 100
    ".runningTimeList label" ? width (pct 20)
    ".runningTimeList input" ? width (pct 20)
    "#maximumPerformance" ? visibility visible
    "#fivePercentCoasting" ? visibility hidden
    "#eightPercentCoasting" ? visibility hidden
    "#energySaving" ? visibility hidden
    "#fullCoasting" ? visibility hidden
    ".button" # hover ? cursor pointer
    "#saveButton" ? do
        position fixed
        bottom $ px 10
        left $ px 10
    "#cumulativeError" ? do
        position fixed
        bottom $ px 50
        left $ px 10
        width $ pct 10
        visibility hidden

dwellTimeSetsCss :: T.Text
dwellTimeSetsCss = render $ do
    validation
    dialogCss
    zeroPM
    ".row" ? do
        width $ pct 100
        display flex
    ".rowElem" ? do
        background $ grayish 220
        padding (px 10) (px 10) (px 10) (px 10)
        margin (px 3) (px 3) (px 3) (px 3)
    "#container" ? do
        position absolute
        height $ pct 100
        width $ pct 100
        display flex
    "#sidebar" ? do
        position fixed
        width $ pct 15
        height $ pct 100
        borderRight solid (px 2) black
    "#main" ? do
        position absolute
        width $ pct 84
        height $ pct 100
        right $ px 0
    ".dwellTimeSet" ? do
        position absolute
        width $ pct 100
        height $ pct 100
    ".dwellTimeSet label" ? width (pct 20)
    ".dwellTimeSet input" ? width (pct 20)
    "#dwellTimeSet1" ? visibility visible
    "#dwellTimeSet2" ? visibility hidden
    "#dwellTimeSet3" ? visibility hidden
    ".button" # hover ? cursor pointer
    "#saveButton" ? do
        position fixed
        bottom $ px 10
        left $ px 10
    "#cumulativeError" ? do
        position fixed
        bottom $ px 50
        left $ px 10
        width $ pct 10
        visibility hidden

alarmLevelsCss :: T.Text
alarmLevelsCss = render $ do
    dialogCss
    zeroPM
    ".row" ? do
        width $ pct 100
        display flex
    ".rowElem" ? do
        background $ grayish 220
        padding (px 10) (px 10) (px 10) (px 10)
        margin (px 3) (px 3) (px 3) (px 3)
    ".row label" ? width (pct 20)
    ".row select" ? width (pct 10)
