#!/usr/bin/env stack
-- stack --resolver lts-8.5 script
 
{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Data.Text as T

main = shell "git pull" empty .&&. shell "stack build" empty
