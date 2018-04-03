#!/usr/bin/env stack
-- stack --resolver lts-8.5 script
 
{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Data.Text as T

status ExitSuccess = putStrLn "SUCCESS"
status (ExitFailure _) = putStrLn "FAILED"

main = do
    putStr "Getting new code ... "
    status $ shell "git pull" empty

    putStr "Starting Build ... "
    status $ shell "stack build" empty

    putStrLn "Exiting ..."
