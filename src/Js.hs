{-# LANGUAGE QuasiQuotes #-}

module Js(
    home,
    account,
    runningTimeLists,
    dwellTimeSets,
    alarmLevels
) where

import Types
import Data.Monoid((<>))
import Language.Javascript.JMacro

import qualified Data.Text.Lazy as T

sendXHRExp :: JStat
sendXHRExp = [jmacro|
        fun sendXHR url postData callback {
            var xhttp = new XMLHttpRequest();
            xhttp.onload = function() {
                if (this.status == 200) {
                   if (this.responseText === "1") {
                       callback.success();
                   }
                   else {
                       // Error Handling
                       callback.failure();
                   }
                }
            };
            xhttp.open("POST", url, true);
            xhttp.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
            xhttp.send(postData);
        };
    |]

home :: String
home = show $ renderJs $ sendXHRExp <> [jmacro|
        var VIEW = {
                ACCOUNTS: 1,
                SYSTEM_PARAMS: 2
            },
            currentView = VIEW.ACCOUNTS;
        fun toggleTabView view {
            var accountsView = document.getElementById('accountsView'),
                systemParamsView = document.getElementById('systemParamsView'),
                toHide, toShow;
            if (currentView !== view) {
                if (view === VIEW.SYSTEM_PARAMS) {
                    toHide = accountsView;
                    toShow = systemParamsView;
                    currentView = VIEW.SYSTEM_PARAMS;
                }
                else {
                    toHide = systemParamsView;
                    toShow = accountsView;
                    currentView = VIEW.ACCOUNTS;
                }
                toHide.style.visibility = 'hidden';
                toShow.style.visibility = 'visible';
            }
        };
        window.onload = \ {
            var accountsViewButton = document.getElementById('accountsViewButton'),
                systemParamsViewButton = document.getElementById('systemParamsViewButton');
            accountsViewButton.onclick = \ {
                toggleTabView(VIEW.ACCOUNTS);
            };
            systemParamsViewButton.onclick = \ {
                toggleTabView(VIEW.SYSTEM_PARAMS);
            };

            var saveButton = document.getElementById('saveButton'),
                formDiv =  document.getElementById('form');
            saveButton.onclick = \ {
                var i, str = [],
                    inputs = formDiv.querySelectorAll('input');
                for(i=0; i<inputs.length; ++i) {
                    str.push(inputs[i].getAttribute('id') + "=" + inputs[i].value);
                }
                sendXHR("/systemParams", str.join('&'), {
                    success: \ {
                        location.reload();
                    },
                    failure: \ {
                        console.log("Encountered an error");
                    }
                });
            };
        };
    |]

account :: AccountMode -> String
account mode = show $ renderJs $ sendXHRExp <> [jmacro|
        window.onload = \ {
            var checkbox = document.getElementById('aocLineOverview'),
                child1 = document.getElementById('enableGlobalCommand'),
                child2 = document.getElementById('enableEnableRegulation');
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
            var form = document.getElementById("accountForm");
            form.addEventListener('submit', function(e) {
                e.preventDefault();
            });
            var submitButton = document.getElementById('submit');
            submitButton.onclick = \ {
                var obj = {};
                var i, elem,
                    password = document.getElementById('accountPassword'),
                    name = document.getElementById('accountName'),
                    acrList = document.getElementById('accountACR').querySelectorAll('input'),
                    aocList = document.getElementById('accountAOC').querySelectorAll('input'),
                    aocLineOverviewList = document.getElementById('aocLineOverviewDiv').querySelectorAll('input'),
                    userID = `(userIDExp)`;

                obj.accountPassword = password.value;
                obj.accountName = name.value;

                obj.accountACR = [];
                for(i=0; i<acrList.length; ++i) {
                    elem = acrList[i];
                    obj.accountACR.push([elem.getAttribute('id'), elem.checked]);
                }

                obj.accountAOC = {};

                // Line Overview Config Input
                if (aocLineOverviewList[0].checked) {
                    obj.accountAOC.aocLineOverview = {};
                    obj.accountAOC.aocLineOverview.enableGlobalCommand = aocLineOverviewList[1].checked;
                    obj.accountAOC.aocLineOverview.enableEnableRegulation = aocLineOverviewList[2].checked;
                }
                else{
                    obj.accountAOC.aocLineOverview = null;
                }

                // AOC Inputs, other than Line Overview Config Inputs
                for(i=3; i<aocList.length; ++i) {
                    elem = aocList[i];
                    obj.accountAOC[elem.getAttribute('id')] = elem.checked;
                }

                sendXHR("/account", "userID=" + userID + "&data=" + JSON.stringify(obj), {
                    success: \ {
                        `(redirect)`;
                    },
                    failure: \ {
                        console.log("Encountered an error");
                    }
                });
            };
        };
    |]
    where
        -- userID :: JExp
        userIDExp = if mode == EDIT
            then [jmacroE|document.getElementById('userID').innerHTML|]
            else [jmacroE|document.getElementById('userID').value|]
        redirect = if mode == EDIT
            then [jmacro|location.reload()|]
            else [jmacro|window.location.replace(window.location.href.replace("addAccount", "home"))|]

runningTimeLists :: String
runningTimeLists = show $ renderJs $ sendXHRExp <> [jmacro|
        window.onload = \ {
            var saveButton = document.getElementById('saveButton');
            saveButton.onclick = \ {
                var i, j, inputs, obj = {}, arr,
                    lists = document.querySelectorAll('.runningTimeList');
                for (i=0; i<lists.length; ++i) {
                    inputs = lists[i].querySelectorAll('input');
                    arr = [];
                    for (j=0; j<inputs.length; ++j) {
                        arr.push([inputs[j].getAttribute('id').split(','), parseFloat(inputs[j].value)]);
                    }
                    obj[lists[i].getAttribute('id')] = arr;
                }

                sendXHR("/runningTimeLists", "data=" + JSON.stringify(obj), {
                    success: \ {
                        location.reload();
                    },
                    failure: \ {
                        console.log("Encountered an error");
                    }
                });
            };
        };
    |]

dwellTimeSets :: String
dwellTimeSets = show $ renderJs $ sendXHRExp <> [jmacro|
        window.onload = \ {
            var saveButton = document.getElementById('saveButton');
            saveButton.onclick = \ {
                var i, j, inputs, obj = {}, arr,
                    lists = document.querySelectorAll('.dwellTimeSet');
                for (i=0; i<lists.length; ++i) {
                    inputs = lists[i].querySelectorAll('input');
                    arr = [];
                    for (j=0; j<inputs.length; ++j) {
                        arr.push([inputs[j].getAttribute('id'), parseFloat(inputs[j].value)]);
                    }
                    obj[lists[i].getAttribute('id')] = arr;
                }

                sendXHR("/dwellTimeSets", "data=" + JSON.stringify(obj), {
                    success: \ {
                        location.reload();
                    },
                    failure: \ {
                        console.log("Encountered an error");
                    }
                })
            };
        };
    |]

alarmLevels :: String
alarmLevels = show $ renderJs $ sendXHRExp <> [jmacro|
        window.onload = \ {
            var saveButton = document.getElementById('saveButton');
            saveButton.onclick = \ {
                var i, obj = [],
                    selects = document.querySelectorAll('select');
                for (i=0; i<selects.length; ++i) {
                    obj.push([[], selects[i].value]);
                }

                sendXHR("/alarmLevels", "data=" + JSON.stringify(obj), {
                    success: \ {
                        location.reload();
                    },
                    failure: \ {
                        console.log("Encountered an error");
                    }
                })
            };
        };
    |]
