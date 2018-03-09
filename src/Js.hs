{-# LANGUAGE QuasiQuotes #-}

module Js where

import Language.Javascript.JMacro

import qualified Data.Text.Lazy as T

account :: String
account = show $ renderJs [jmacro|
        window.onload = \ {
            var checkbox = document.getElementById('lineOverviewConfig'),
                child1 = document.getElementById('enableGlobalCommand'),
                child2 = document.getElementById('enableRegulation');
            checkbox.onchange = \ {
                if (checkbox.checked) {
                    child1.removeAttribute('disabled');
                    child2.removeAttribute('disabled');
                }
                else {
                    child1.checked = false;
                    child2.checked = false;
                    child1.setAttribute('disabled', 'disabled');
                    child2.setAttribute('disabled', 'disabled');
                }
            };
        };
    |]
