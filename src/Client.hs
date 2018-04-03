#!/usr/bin/env stack
-- stack --resolver lts-8.5 script
 
{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Data.Text as T
import Data.Maybe(mapMaybe)

host = "dmrc@172.22.107.1"
buildDirectory = "~/ASPMS/.stack-work/install/x86_64-linux/lts-8.5/8.0.2/bin"
buildFile = "ASPMS"

commands = -- mapMaybe textToLine ["ls -l"]
    map unsafeTextToLine ["echo Hello World!", "cd ASPMS ", "pwd ", "ls . "]

main =
    -- -- Trigger Build
    -- shell (T.unwords ["ssh", host, "chmod 755 ~/ASPMS/Build.hs"]) empty .&&.
    --     shell (T.unwords ["ssh", host, "cd ASPMS"]) empty .&&.
    --     shell (T.unwords ["ssh", host, "./Build.hs"]) empty .&&.
        
    --     -- Fetch Binary
    --     shell (T.unwords ["scp", "~/", mconcat [host, ":", buildDirectory, buildFile]]) empty
    -- putStrLn "Done"
    shell (T.unwords ["ssh", host]) $ select commands
