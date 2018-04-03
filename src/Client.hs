#!/usr/bin/env stack
-- stack --resolver lts-8.5 script
 
{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Data.Text as T

host = "dmrc@172.22.107.1"
buildDirectory = "~/ASPMS/.stack-work/install/x86_64-linux/lts-8.5/8.0.2/bin"
buildFile = "ASPMS"

main = do
    -- Trigger Build
    shell (T.unwords ["ssh", host, "./ASPMS/Build.hs"]) empty
    
    -- Fetch Binary
    shell (T.unwords ["scp", "~", mconcat [host, ":", buildDirectory, buildFile]]) empty
    putStrLn "Done"
