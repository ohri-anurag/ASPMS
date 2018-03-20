{-# LANGUAGE QuasiQuotes #-}

module Js(
    home,
    account,
    runningTimeLists,
    dwellTimeSets,
    alarmLevels,
    changePassword
) where

import Types
import Data.Monoid((<>))
import Language.Javascript.JMacro

import qualified Data.Text.Lazy as T

validation :: JStat
validation = [jmacro|
        fun displayError parent errorMsg {
            if(parent.querySelectorAll('.error').length > 0)
                return;
            var errorDiv = document.createElement('div');
            errorDiv.appendChild(document.createTextNode(errorMsg));
            errorDiv.setAttribute('class', 'error');
            parent.appendChild(errorDiv);
            var label = parent.querySelector('label'),
                input = parent.querySelector('input');
            label.style.width = '33.33%';
            input.style.width = '33.33%';
            errorDiv.style.width = '33.33%';
            parent.style.width = '60%';
        }
        fun removeError parent {
            if(parent.querySelectorAll('.error').length === 0)
                return;
            var errorDiv = parent.querySelector('.error');
            parent.removeChild(errorDiv);
            var label = parent.querySelector('label'),
                input = parent.querySelector('input');
            label.style = {};
            input.style = {};
            errorDiv.style = {};
            parent.style = {};
        }
        fun validate id predicate errorMsg {
            var elem = document.getElementById(id);
            removeError elem.parentElement
            if(predicate(elem.value)) {
                return true;
            }
            displayError elem.parentElement errorMsg
            return false;
        }
        fun notEmpty id {
            return validate id (\ val -> val !== "") "Field cannot be left empty."
        }
        fun positiveInt id {
            return validate id (\ val {
                var regex = /^[1-9]\d*$/;
                return regex.test(val)
            }) "Field must have a non-zero positive integer."
        }
        fun noMoreThan10 id {
            return validate id (\ val -> val.length <= 10) "Field cannot have more than 10 characters."
        }
        fun positiveFloat id {
            return validate id (\ val {
                var regex = /^([1-9]\d*(\.\d*[1-9])?|0\.\d*[1-9])$/;
                return regex.test(val)
            }) "Field must have a non-zero positive number."
        }
    |]



sendXHRExp :: JStat
sendXHRExp = [jmacro|
        var firstCall = true;
        fun toggleDialog bool text callback {
            var dialog = document.getElementById('dialog'),
                dialogText = document.getElementById('dialogText'),
                dialogButton = document.getElementById('dialogButton'),
                screen = document.getElementById('screen');
            if (firstCall) {
                dialogButton.onclick = \ {
                    toggleDialog false null null
                    callback();
                };
                firstCall = false;
            }
            if (bool) {
                screen.style.opacity = 0.7;
                screen.style.zIndex = 1;
                dialog.style.opacity = 1;
                dialog.style.zIndex = 2;

                while (dialogText.hasChildNodes()) {
                    dialogText.removeChild(dialogText.firstChild);
                }
                var textNode = document.createTextNode(text);
                dialogText.appendChild(textNode);
            }
            else {
                screen.style.opacity = 0;
                screen.style.zIndex = -1;
                dialog.style.opacity = 0;
                dialog.style.zIndex = -1;
            }
        }
        fun sendXHR url postData callback {
            var xhttp = new XMLHttpRequest();
            xhttp.onload = function() {
                if (this.status == 200) {
                   if (this.responseText === "1") {
                       toggleDialog(true, "Changes saved successfully.", callback.success);
                   }
                   else {
                       // Error Handling
                       toggleDialog(true, "Could not save changes.", callback.failure);
                   }
                }
            };
            xhttp.open("POST", url, true);
            xhttp.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
            xhttp.send(postData);
        };
    |]

home :: String
home = show $ renderJs $ sendXHRExp <>
    validation <>
    [jmacro|
        var VIEW = {
                ACCOUNTS: 1,
                SYSTEM_PARAMS: 2
            },
            currentView = VIEW.ACCOUNTS;
        fun toggleTabView view {
            var accountsView = document.getElementById('accountsView'),
                systemParamsView = document.getElementById('systemParamsView'),
                runningTimeListsLink = document.getElementById('runningTimeLists'),
                dwellTimeSetsLink = document.getElementById('dwellTimeSets'),
                alarmLevelsLink = document.getElementById('alarmLevels'),
                addAccountLink = document.getElementById('addAccount'),
                toHide, toShow;
            if (currentView !== view) {
                if (view === VIEW.SYSTEM_PARAMS) {
                    toHide = accountsView;
                    toShow = systemParamsView;
                    currentView = VIEW.SYSTEM_PARAMS;
                    runningTimeListsLink.style.visibility = 'visible';
                    dwellTimeSetsLink.style.visibility = 'visible';
                    alarmLevelsLink.style.visibility = 'visible';
                    addAccountLink.style.visibility = 'hidden';
                }
                else {
                    toHide = systemParamsView;
                    toShow = accountsView;
                    currentView = VIEW.ACCOUNTS;
                    runningTimeListsLink.style.visibility = 'hidden';
                    dwellTimeSetsLink.style.visibility = 'hidden';
                    alarmLevelsLink.style.visibility = 'hidden';
                    addAccountLink.style.visibility = 'visible';
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

            var apply = document.getElementById('apply');
            apply.onclick = \ {
                sendXHR("/applyChanges", "",{
                    success: \ {
                        console.log("Success!");
                    },
                    failure: \ {
                        console.log("Encountered an error");
                    }
                });
            };

            var i, deleteButtons = document.querySelectorAll('.deleteButton');
            for(i=0; i<deleteButtons.length; ++i) {
                deleteButtons[i].onclick = \ {
                    var id = this.getAttribute('id');
                    sendXHR("/deleteAccount", "userID=" + id,{
                        success: \ {
                            location.reload();
                        },
                        failure: \ {
                            console.log("Encountered an error");
                        }
                    });
                }
            }

            var saveButton = document.getElementById('saveButton'),
                formDiv =  document.getElementById('form');
            saveButton.onclick = \ {
                var i, str = [], check = true,
                    inputs = formDiv.querySelectorAll('input');
                for(i=0; i<inputs.length; ++i) {
                    check = check && notEmpty(inputs[i].getAttribute('id')) && positiveInt(inputs[i].getAttribute('id'));
                    str.push(inputs[i].getAttribute('id') + "=" + inputs[i].value);
                }

                if (!check)
                    return;

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
account mode = show $ renderJs $ sendXHRExp <>
    validation <>
    [jmacro|
        window.onload = \ {
            var checkbox = document.getElementById('aocLineOverview'),
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

                var check = `(userIDCheck)` && notEmpty('accountName') && noMoreThan10('accountName') && notEmpty('accountPassword') & noMoreThan10('accountPassword');
                if(!check)
                    return;

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
                    obj.accountAOC.aocLineOverview.enableRegulation = aocLineOverviewList[2].checked;
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
        userIDCheck = if mode == EDIT
            then [jmacroE|true|]
            else [jmacroE|notEmpty('userID') && noMoreThan10('userID')|]
        redirect = if mode == EDIT
            then [jmacro|location.reload()|]
            else [jmacro|window.location.replace(window.location.href.replace("addAccount", "home"))|]

runningTimeLists :: String
runningTimeLists = show $ renderJs $ sendXHRExp <>
    validation <>
    [jmacro|
        var currentView;
        fun display id {
            var elem = document.getElementById(id);
            if (currentView === elem)
                return;
            currentView.style.visibility = 'hidden';
            elem.style.visibility = 'visible';
            currentView = elem;
        };
        window.onload = \ {
            currentView = document.getElementById('maximumPerformance');
            document.getElementById('maximumPerformanceButton').onclick = \ {
                display 'maximumPerformance'
            };
            document.getElementById('fivePercentCoastingButton').onclick = \ {
                display 'fivePercentCoasting'
            };
            document.getElementById('eightPercentCoastingButton').onclick = \ {
                display 'eightPercentCoasting'
            };
            document.getElementById('energySavingButton').onclick = \ {
                display 'energySaving'
            };
            document.getElementById('fullCoastingButton').onclick = \ {
                display 'fullCoasting'
            };
            var saveButton = document.getElementById('saveButton');
            saveButton.onclick = \ {
                var i, j, inputs, obj = {}, arr, check = true,
                    lists = document.querySelectorAll('.runningTimeList');
                for (i=0; i<lists.length; ++i) {
                    inputs = lists[i].querySelectorAll('input');
                    arr = [];
                    for (j=0; j<inputs.length; ++j) {
                        check = check && notEmpty(inputs[j].getAttribute('id')) && positiveFloat(inputs[j].getAttribute('id'));
                        arr.push([inputs[j].getAttribute('id').substr(3).split(','), parseFloat(inputs[j].value)]);
                    }
                    obj[lists[i].getAttribute('id')] = arr;
                }

                if (!check) {
                    document.getElementById('cumulativeError').style.visibility = 'visible';
                    return;
                }
                document.getElementById('cumulativeError').style.visibility = 'hidden';

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
dwellTimeSets = show $ renderJs $ sendXHRExp <>
    validation <>
    [jmacro|
        var currentView;
        fun display id {
            var elem = document.getElementById(id);
            if (currentView === elem)
                return;
            currentView.style.visibility = 'hidden';
            elem.style.visibility = 'visible';
            currentView = elem;
        };
        window.onload = \ {
            currentView = document.getElementById('dwellTimeSet1');
            document.getElementById('dwellTimeSet1Button').onclick = \ {
                display 'dwellTimeSet1'
            };
            document.getElementById('dwellTimeSet2Button').onclick = \ {
                display 'dwellTimeSet2'
            };
            document.getElementById('dwellTimeSet3Button').onclick = \ {
                display 'dwellTimeSet3'
            };
            var saveButton = document.getElementById('saveButton');
            saveButton.onclick = \ {
                var i, j, inputs, obj = {}, arr, check = true,
                    lists = document.querySelectorAll('.dwellTimeSet');
                for (i=0; i<lists.length; ++i) {
                    inputs = lists[i].querySelectorAll('input');
                    arr = [];
                    for (j=0; j<inputs.length; ++j) {
                        check = check && notEmpty(inputs[j].getAttribute('id')) && positiveInt(inputs[j].getAttribute('id'));
                        arr.push([inputs[j].getAttribute('id').substr(2), parseFloat(inputs[j].value)]);
                    }
                    obj[lists[i].getAttribute('id')] = arr;
                }

                if (!check) {
                    document.getElementById('cumulativeError').style.visibility = 'visible';
                    return;
                }
                document.getElementById('cumulativeError').style.visibility = 'hidden';

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
                    obj.push([selects[i].getAttribute('id'), selects[i].value]);
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

changePassword :: String
changePassword = show $ renderJs $ sendXHRExp <> 
    validation <>
    [jmacro|
        window.onload = \ {
            var saveButton = document.getElementById('saveButton');
            saveButton.onclick = \ {
                var input = document.getElementById('password').value, check = true;
                check = notEmpty('password') && noMoreThan10('password');

                if (!check)
                    return;

                sendXHR("/changePassword", "password=" + input, {
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
