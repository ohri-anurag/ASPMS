{-# LANGUAGE QuasiQuotes #-}

module Js(
    home,
    account,
    runningTimeLists,
    dwellTimeSets,
    alarmLevels,
    changePassword
) where
import Prelude(String, show, ($), (==))
import Types
import Data.Monoid((<>))
import Language.Javascript.JMacro

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
        fun noLongerThan num {
            return \ id -> validate id (\ val -> val.length <= num) ("Field cannot have more than " + num + " characters.")
        }
        fun noMoreThan5 id {
            return validate id (\ val -> parseFloat(val) <= 5) "Field cannot have number greater than 5."
        }
        fun noMoreThan999 id {
            return validate id (\ val -> parseFloat(val) <= 999) "Field cannot have number greater than 999."
        }
        fun positiveInt id {
            return validate id (\ val {
                var regex = /^[1-9]\d*$/;
                return regex.test(val)
            }) "Field must have a non-zero positive integer."
        }
        fun positiveFloat id {
            return validate id (\ val {
                var regex = /^([1-9]\d*(\.\d*[1-9])?|0\.\d*[1-9])$/;
                return regex.test(val)
            }) "Field must have a non-zero positive number."
        }
        fun alphaNumAndSpaces id {
            return validate id (\ val {
                var regex = /^[\dA-Za-z]+(\s[\dA-Za-z]+)*$/;
                return regex.test(val)
            }) "Field must consist of alphabets, numbers and space(only 1 allowed between words)."
        }
        fun noSpaces id {
            return validate id (\ val {
                var regex = /^\S+$/;
                return regex.test(val)
            }) "Field must not contain spaces."
        }
        fun validator id checkList {
            var i, check = true;
            for(i=0; i<checkList.length; ++i) {
                 check = check && checkList[i] id;
            }
            return check;
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

heartBeatExp :: JStat
heartBeatExp = [jmacro|
        setInterval(\ {
            var xhttp = new XMLHttpRequest();
            xhttp.open("GET", "/heartbeat", true);
            xhttp.send();
        }, 1000);
    |]

home :: String
home = show $ renderJs $ sendXHRExp <>
    validation <>
    heartBeatExp <>
    [jmacro|
        var VIEW = {
                ACCOUNTS: 1,
                SYSTEM_PARAMS: 2
            },
            currentView = VIEW.ACCOUNTS;
        fun toggleTabView view {
            var accountsView = document.getElementById('accountsView'),
                systemParamsView = document.getElementById('systemParamsView'),
                systemParamsRemaining = document.getElementById('systemParamsRemaining'),
                addAccountDiv = document.getElementById('addAccountDiv'),
                toHide, toShow;
            if (currentView !== view) {
                if (view === VIEW.SYSTEM_PARAMS) {
                    toHide = accountsView;
                    toShow = systemParamsView;
                    currentView = VIEW.SYSTEM_PARAMS;
                    systemParamsRemaining.style.visibility = 'visible';
                    addAccountDiv.style.visibility = 'hidden';
                }
                else {
                    toHide = systemParamsView;
                    toShow = accountsView;
                    currentView = VIEW.ACCOUNTS;
                    systemParamsRemaining.style.visibility = 'hidden';
                    addAccountDiv.style.visibility = 'visible';
                }
                toHide.style.visibility = 'hidden';
                toShow.style.visibility = 'visible';
            }
        };
        fun showConfirmationDialog text {
            var screen = document.getElementById('screen'),
                confirm = document.getElementById('confirm'),
                confirmText = document.getElementById('confirmText');
            
            confirmText.innerHTML = text;
            screen.style.opacity = 0.7;
            screen.style.zIndex = 1;
            confirm.style.opacity = 1;
            confirm.style.zIndex = 2;
        }
        fun hideConfirmationDialog {
            var screen = document.getElementById('screen'),
                confirm = document.getElementById('confirm');

            screen.style.opacity = 0;
            screen.style.zIndex = -1;
            confirm.style.opacity = 0;
            confirm.style.zIndex = -1;
        }
        window.onload = \ {
            var accountsViewButton = document.getElementById('accountsViewButton'),
                systemParamsViewButton = document.getElementById('systemParamsViewButton');
            accountsViewButton.onclick = \ {
                toggleTabView(VIEW.ACCOUNTS);
            };
            systemParamsViewButton.onclick = \ {
                toggleTabView(VIEW.SYSTEM_PARAMS);
            };

            var apply = document.getElementById('apply'),
                yes = document.getElementById('yes'),
                no = document.getElementById('no');
            no.onclick = hideConfirmationDialog;
            apply.onclick = \{
                yes.onclick = \{
                    sendXHR("/applyChanges", "",{
                        success: \ {
                            console.log("Success!");
                        },
                        failure: \ {
                            console.log("Encountered an error");
                        }
                    });
                    hideConfirmationDialog();
                };
                showConfirmationDialog("This will send the latest information to all servers and workstations. Proceed?");
            };

            var i, deleteButtons = document.querySelectorAll('.deleteButton');
            for(i=0; i<deleteButtons.length; ++i) {
                deleteButtons[i].onclick = \ {
                    var self = this;
                    yes.onclick = \{
                        var id = self.getAttribute('id');
                        sendXHR("/deleteAccount", "userID=" + id,{
                            success: \ {
                                location.reload();
                            },
                            failure: \ {
                                console.log("Encountered an error");
                            }
                        });
                        hideConfirmationDialog();
                    };
                    showConfirmationDialog("Delete Account?");
                };
            }

            var saveButton = document.getElementById('saveButton'),
                formDiv =  document.getElementById('form');
            saveButton.onclick = \ {
                var str = [], flag, check = true,
                    inputs = formDiv.querySelectorAll('input');
                for(i=0; i<inputs.length; ++i) {
                    var id = inputs[i].getAttribute('id');
                    if (id === "tunnelLimit") {
                        flag = validator id [notEmpty, noMoreThan5, positiveInt];
                    } else {
                        flag = validator id [notEmpty, noMoreThan999, positiveInt];
                    }
                    check = check && flag;
                    str.push(id + "=" + inputs[i].value);
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
    heartBeatExp <>
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

                var flag, check = true; 
                flag = `(userIDCheck)`;
                check = check && flag;
                flag = validator 'accountName' [notEmpty, noLongerThan 35, alphaNumAndSpaces];
                check = check && flag;
                flag = validator 'accountPassword' [notEmpty, noLongerThan 25, noSpaces];
                check = check && flag;
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
            else [jmacroE|validator 'userID' [notEmpty, noLongerThan 25, alphaNumAndSpaces]|]
        redirect = if mode == EDIT
            then [jmacro|location.reload()|]
            else [jmacro|window.location.replace(window.location.href.replace("addAccount", "home"))|]

runningTimeLists :: String
runningTimeLists = show $ renderJs $ sendXHRExp <>
    validation <>
    heartBeatExp <>
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
                var i, j, inputs, obj = {}, arr, flag, check = true,
                    lists = document.querySelectorAll('.runningTimeList');
                for (i=0; i<lists.length; ++i) {
                    inputs = lists[i].querySelectorAll('input');
                    arr = [];
                    for (j=0; j<inputs.length; ++j) {
                        var id = inputs[j].getAttribute('id');
                        // Check if the value is greater than 9990, if so, skip validation
                        if (parseFloat(inputs[j].value) <= 9990) {
                            flag = validator id [notEmpty, noMoreThan999, positiveFloat];
                            check = check && flag;
                        }
                        arr.push([id.substr(3).split(','), parseFloat(inputs[j].value)]);
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
    heartBeatExp <>
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
                var i, j, inputs, obj = {}, arr, flag, check = true,
                    lists = document.querySelectorAll('.dwellTimeSet');
                for (i=0; i<lists.length; ++i) {
                    inputs = lists[i].querySelectorAll('input');
                    arr = [];
                    for (j=0; j<inputs.length; ++j) {
                        var id = inputs[j].getAttribute('id');
                        flag = validator id [notEmpty, noMoreThan999, positiveInt];
                        check = check && flag;
                        arr.push([id.substr(2), parseFloat(inputs[j].value)]);
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
alarmLevels = show $ renderJs $ sendXHRExp <>
    heartBeatExp <>
    [jmacro|
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
    heartBeatExp <>
    [jmacro|
        window.onload = \ {
            var saveButton = document.getElementById('saveButton');
            saveButton.onclick = \ {
                var input = document.getElementById('password').value, check = true;
                check = validator 'password' [notEmpty, noLongerThan 25, noSpaces];

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
