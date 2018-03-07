{-# LANGUAGE OverloadedStrings #-}

module Css where

import Clay as C

import qualified Data.Text.Lazy as T
-- import qualified Data.Text as T

homeCss :: T.Text
homeCss = render $ do
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
    "#main" ?  width (pct 85)
    ".row" ? do
        width $ pct 100
        display flex
    ".header" ? fontWeight bold
    ".row div" ? do
        background $ grayish 220
        padding (px 10) (px 10) (px 10) (px 10)
        margin (px 3) (px 3) (px 3) (px 3)
    ".uid" ? width (pct 33)
    ".accountName" ? width (pct 67)

accountDetailsCss :: T.Text
accountDetailsCss = render $ do
    star ? do
        margin (px 0) (px 0) (px 0) (px 0)
        padding (px 0) (px 0) (px 0) (px 0)
    ".row" ? do
        width $ pct 100
        display flex
    ".form" ? do
        display flex
        flexDirection column
