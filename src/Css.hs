{-# LANGUAGE OverloadedStrings #-}

module Css(
    homeCss,
    accountDetailsCss,
    runningTimeListsCss,
    dwellTimeSetsCss,
    alarmLevelsCss
) where

import Clay as C

import qualified Data.Text.Lazy as T

validation :: Css
validation = ".error" ? do
    color red
    padding (px 10) (px 10) (px 10) (px 10)
    margin (px 3) (px 3) (px 3) (px 3)

-- TODO Try a little refactoring
homeCss :: T.Text
homeCss = render $ do
    validation
    star ? do
        margin (px 0) (px 0) (px 0) (px 0)
        padding (px 0) (px 0) (px 0) (px 0)
    "#container" ? do
        position absolute
        height $ pct 100
        width $ pct 100
        display flex
    "#sidebar" ? do
        width $ pct 15
        borderRight solid (px 2) black
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
    ".button" # hover ? cursor pointer
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
    star ? do
        margin (px 0) (px 0) (px 0) (px 0)
        padding (px 0) (px 0) (px 0) (px 0)
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
    star ? do
        margin (px 0) (px 0) (px 0) (px 0)
        padding (px 0) (px 0) (px 0) (px 0)
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
    star ? do
        margin (px 0) (px 0) (px 0) (px 0)
        padding (px 0) (px 0) (px 0) (px 0)
    ".row" ? do
        width $ pct 100
        display flex
    ".rowElem" ? do
        background $ grayish 220
        padding (px 10) (px 10) (px 10) (px 10)
        margin (px 3) (px 3) (px 3) (px 3)

alarmLevelsCss :: T.Text
alarmLevelsCss = render $ do
    star ? do
        margin (px 0) (px 0) (px 0) (px 0)
        padding (px 0) (px 0) (px 0) (px 0)
    ".row" ? do
        width $ pct 100
        display flex
    ".rowElem" ? do
        background $ grayish 220
        padding (px 10) (px 10) (px 10) (px 10)
        margin (px 3) (px 3) (px 3) (px 3)
