- [ASPMS](#aspms)
  - [Pre Installation Instructions](#pre-installation-instructions)
  - [How to Install](#how-to-install)
    - [Build from source](#build-from-source)
    - [Run Precompiled binary (ASPMS)](#run-precompiled-binary-aspms)
  - [How it works](#how-it-works)
    - [Saving Data](#saving-data)
    - [Updating Data](#updating-data)
  - [Users](#users)
- [Chief Controller View](#chief-controller-view)
  - [Login](#login)
    - [How to Login](#how-to-login)
  - [Home](#home)
    - [Sidebar](#sidebar)
    - [Views](#views)
      - [Accounts View](#accounts-view)
      - [System Parameters View](#system-parameters-view)
  - [Add Account](#add-account)
  - [Edit Account](#edit-account)
  - [Running Time Lists](#running-time-lists)
    - [Sidebar](#sidebar-1)
    - [Views](#views-1)
      - [Maximum Performance](#maximum-performance)
      - [Five Percent Coasting](#five-percent-coasting)
      - [Eight Percent Coasting](#eight-percent-coasting)
      - [Energy Saving](#energy-saving)
      - [Full Coasting](#full-coasting)
    - [How to Edit](#how-to-edit)
    - [Accepted Values](#accepted-values)
  - [Dwell Time Sets](#dwell-time-sets)
    - [Sidebar](#sidebar-2)
    - [Views](#views-2)
      - [Dwell Time Set 1](#dwell-time-set-1)
      - [Dwell Time Set 2](#dwell-time-set-2)
      - [Dwell Time Set 3](#dwell-time-set-3)
    - [How to Edit](#how-to-edit-1)
    - [Accepted Values](#accepted-values-1)
  - [Alarm Levels](#alarm-levels)
    - [How to Edit](#how-to-edit-2)
    - [How to Save](#how-to-save)
  - [Change Password](#change-password)
    - [How to Edit](#how-to-edit-3)
    - [How to Save](#how-to-save-1)
    - [Accepted Values](#accepted-values-2)
- [Rolling Stock Controller View](#rolling-stock-controller-view)
  - [Login](#login-1)
    - [How to Login](#how-to-login-1)
  - [Rolling Stock Roster](#rolling-stock-roster)
    - [How to Add Rolling Stock](#how-to-add-rolling-stock)
    - [Accepted Values](#accepted-values-3)
    - [How to Delete Rolling Stock](#how-to-delete-rolling-stock)
    - [How to Save](#how-to-save-2)
  - [Change Password](#change-password-1)
    - [How to Edit](#how-to-edit-4)
    - [How to Save](#how-to-save-3)
    - [Accepted Values](#accepted-values-4)
- [Crew Controller View](#crew-controller-view)
  - [Login](#login-2)
    - [How to Login](#how-to-login-2)
  - [Crew Roster](#crew-roster)
    - [How to Add Crew](#how-to-add-crew)
    - [Accepted Values](#accepted-values-5)
    - [How to Delete Crew](#how-to-delete-crew)
    - [How to Save](#how-to-save-4)
  - [Change Password](#change-password-2)
    - [How to Edit](#how-to-edit-5)
    - [How to Save](#how-to-save-5)
    - [Accepted Values](#accepted-values-6)

# ASPMS
Welcome to Accounts and System Parameters Management System.
This document tries to capture all the use cases of ASPMS.
## Pre Installation Instructions
ATS assumes that logger server runs on 172.21.107.1(Network 1) and 172.22.107.1(Network 2).
So, the machine on which ASPMS is to be tested must be configured to have IP as 172.21.107.1 or 172.22.107.1.
## How to Install
### Build from source
1. Install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
2. Create a SSH key pair and add the key to your github account by following the instructions at:
    - [Generate SSH Key](https://help.github.com/enterprise/2.12/user/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent/)
    - [Add SSH key to Github Account](https://help.github.com/enterprise/2.12/user/articles/adding-a-new-ssh-key-to-your-github-account/)
3. Clone code from github:
    > git clone https://github.com/ohri-anurag/ASPMS.git
4. Change the current directory:
    > cd ASPMS
5. Run following command to compile:
    > stack build
6. Run following command to execute:
    > stack exec ASPMS
### Run Precompiled binary (ASPMS)
1. Run following command to create a directory:
    > mkdir ASPMS
2. Run following directory to change the current working directory:
    > cd ASPMS
4. Run following command to execute:
    > ./ASPMS

## How it works
ASPMS can be used to:
1. Edit Account data
2. Edit System Parameters
3. Save the data
4. Update the data to all workstations and servers.

### Saving Data
Data can be saved by clicking on Save buttons whenever changes are made. This does **NOT** update the data to all workstations and servers.

### Updating Data
Data can be updated by clicking on Apply Changes button. This updates already saved data to all workstations and servers. This does **NOT** save current changes.

The rest of the document goes through all the pages in ASPMS and explains them one by one.

## Users
There are three users permitted to use ASPMS:
1. Chief Controller
2. Rolling Stock Controller
3. Crew Controller

Each user has a different password for ASPMS, and can login using that password. All three users have different permissions and different views.

| User                     | View                                             | Default Password |
| ------------------------ | ------------------------------------------------ | ---------------- |
| Chief Controller         | Everything except Rolling Stock and Crew Details | password         |
| Crew Controller          | Crew Details                                     | passwordcrew     |
| Rolling Stock Controller | Rolling Stock Details                            | passwordrsc      |

# Chief Controller View
## Login
![Login screen](img/Login.png)

The login screen is the first screen encountered when opening ASPMS.

### How to Login
The default password is "password" (without the quotes). This can be changed to some other value once the user has logged in.

In order to login, enter the password in the text box, and press enter or click Login.

## Home
The home page is displayed after logging in.

It consists of 2 regions,
1. Sidebar (Left, minor column)
2. View region (Right, occupies most of the screen)

### Sidebar
*Accounts*

This button opens the Accounts view in the view region. Accounts view is the default view.

*System Parameters*

This button opens the System Parameters view in the view region.

*Apply Changes*

![Apply Changes](img/ApplyChanges.png)
This button applies whatever changes have been saved, and sends them to all the workstations and servers.

Click on **Apply Changes**, and then click **Yes** in the confirmation dialog. Then click **OK** in the success dialog.

*Add Account*

This link can be clicked to add a new account via the Add Account page.

*Version*

This section displays the current version of ASPMS.

*Load Data*

This button can be used to import the data that was previously exported from ASPMS. The imported data is automatically saved and ASPMS is updated to use it.
Clicking on this button opens up a window, from which the user can browse to the file to be loaded. After selecting the file in window, user can click on Open button to load it into ASPMS.

*Save Data*

This button can be used to export the data that ASPMS holds currently. A new file will be created on the server, and a dialog will display the name of the file.

Internet Explorer 11 will display a dialog asking you if you want to download the saved file to the machine. Click on Save Button to download it.

*Change Password*

This link can be clicked to change the existing password via the Change Password page.

*Logout*

Click the logout button to log out of ASPMS.

### Views

The home page has 2 views
1. Accounts View (Default)
2. System Parameters View

#### Accounts View
![Accounts View](img/Home1.png)
Upon opening home page, this view is shown by default.

It displays all the accounts in the system, along with the following details:
- User ID
- Name

*Add Account*

Click on Add Account link in the sidebar to open the Add Account Page.

*Edit Account*

The user ID is clickable, and can be clicked to open the edit account page.

*Delete Account*

![Delete Account](img/DeleteAccount.png)
Click on the **Delete Account** button corresponding to that account's user ID. Then click **Yes** in the confirmation dialog. Then click **OK** in the success dialog.

#### System Parameters View
![System Parameters View](img/Home2.png)
This view lists the following systems parameters:
1. Departure Offset
2. Route Trigger Offset
3. Minimum Dwell Time
4. Delay Detection Threshold
5. Interstation Stop Detection Time
6. Tunnel Limit
7. WakeUp Command Offset

*How to Edit*

Each of the above system parameters has a text box next to them. Just click in the text box to edit these values.

*How to Save*
![Save System Parameters](img/SaveSP.png)
Click on the **Save Changes** button below to save any changes. Then click **OK** in the success dialog.

*Accepted Values*

The values acceptable for each of the above parameters are documented in the table below:

| System Parameter                 | Accepted Value                                                                                                 |
| -------------------------------- | -------------------------------------------------------------------------------------------------------------- |
| Departure Offset                 | Value must be a number **greater than 0** and **lesser than or equal to 999**, decimals are **NOT** allowed.   |
| Route Trigger Offset             | Value must be a number **greater than 0** and **lesser than or equal to 999**, decimals are **NOT** allowed.   |
| Minimum Dwell Time               | Value must be a number **greater than 0** and **lesser than or equal to 999**, decimals are **NOT** allowed.   |
| Delay Detection Threshold        | Value must be a number **greater than 0** and **lesser than or equal to 999**, decimals are **NOT** allowed.   |
| Interstation Stop Detection Time | Value must be a number **greater than 0** and **lesser than or equal to 999**, decimals are **NOT** allowed.   |
| Tunnel Limit                     | Value must be a number **greater than 0** and **lesser than or equal to 5**, decimals are **NOT** allowed.     |
| WakeUp Command Offset            | Value must be a number **greater than 5** and **lesser than or equal to 18000**, decimals are **NOT** allowed. |


>**NOTE :** New Account cannot be added if invalid information is provided. Appropriate error message will be displayed.
![Error](img/ValidationSystemParameter.png)

When viewing the System Parameters view, there are 3 more links available in the Sidebar.

*Running Time Lists*

This link can be clicked to open the Running Time Lists page.

*Dwell Time Sets*

This link can be clicked to open the Dwell Time Sets page.

*Alarm Levels*

This link can be clicked to open the Alarm Levels page.


## Add Account
![Add Account](img/AddAccount.png)
This page allows the user to add a new account to ASPMS.

The following table explains each part of the page:

| Label            | Value                                                             |
| ---------------- | ----------------------------------------------------------------- |
| User Id          | Enter the user ID for the new account.                            |
| Account Name     | Enter the account name for the new account.                       |
| Account Password | Enter the account password for the new account.                   |
| Account ACR      | Choose the regions for which the new account will have authority. |
| Account AOC      | Choose the Area of Control permissions for the new account.       |

*Accepted Values*

The following table lists the accepted values for the text boxes on the Add Account page

| Field Name       | Accepted Value                                                                                                                                                                   |
| ---------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| User Id          | Alphabets(A-Z,a-z), Numbers(0-9), spaces and underscores are allowed. Only one space/underscore allowed between words. Should **NOT** be empty. Maximum length is 25 characters. |
| Account Name     | Alphabets(A-Z,a-z), Numbers(0-9) and spaces are allowed. Only one space allowed between words. Should **NOT** be empty. Maximum length is 25 characters.                         |
| Account Password | Should **NOT** contain spaces, all other characters are acceptable . Should **NOT** be empty. Maximum length is 25 characters.                                                   |

*Line Overview Config*

This Area Of Control permission is further divided into 2 permissions:
1. Enable Global Command
2. Enable Regulation

Both these permissions are automaticaly disabled when Line Overview Config is disabled. They can only be enabled when Line Overview Config is enabled.

*Add Account*

After entering acceptable values, click **Add Account** and then click **OK** in the success dialog.

>**NOTE :** New Account cannot be added if invalid information is provided. Appropriate error message will be displayed.
![Error](img/ValidationAddAccount.png)

*Home*

Clicking on this link takes the user back to the home page. 

## Edit Account
![Edit Account](img/EditAccount.png)
This page allows the user to edit an account in ASPMS.

The following table explains each part of the page:

| Label            | Value                                                       |
| ---------------- | ----------------------------------------------------------- |
| Account Name     | Enter the new account name for the account.                 |
| Account Password | Enter the new account password for the account.             |
| Account ACR      | Edit the regions for which the account will have authority. |
| Account AOC      | Edit the Area of Control permissions for the account.       |

*Accepted Values*

The following table lists the accepted values for the text boxes on the Add Account page

| Field Name       | Accepted Value                                                                                                                                           |
| ---------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Account Name     | Alphabets(A-Z,a-z), Numbers(0-9) and spaces are allowed. Only one space allowed between words. Should **NOT** be empty. Maximum length is 25 characters. |
| Account Password | Should **NOT** contain spaces, all other characters are acceptable . Should **NOT** be empty. Maximum length is 25 characters.                           |

*Line Overview Config*

This Area Of Control permission is further divided into 2 permissions:
1. Enable Global Command
2. Enable Regulation

Both these permissions are automaticaly disabled when Line Overview Config is disabled. They can only be enabled when Line Overview Config is enabled.

*Save Changes*

After entering acceptable values, click **Save Changes** and then click **OK** in the success dialog.

>**NOTE :** Account cannot be changed if invalid information is provided. Appropriate error message will be displayed.
![Error](img/ValidationEditAccount.png)

*Home*

Clicking on this link takes the user back to the home page. 

## Running Time Lists
This page consists of 2 regions,
1. Sidebar (Left, minor column)
2. View region (Right, occupies most of the screen)

### Sidebar
*Maximum Performance*

This button opens the Maximum Performance view in the view region.

*Five Percent Coasting*

This button opens the Five Percent Coasting view in the view region.

*Eight Percent Coasting*

This button opens the Eight Percent Coasting view in the view region.

*Energy Saving*

This button opens the Energy Saving view in the view region.

*Full Coasting*

This button opens the Full Coasting view in the view region.

*Save Changes*

Click on **Save Changes** and then click **OK** in the success dialog.

*Import Running Times*

Click on **Choose File** to select a file that was exported from VDU, to import all of the running time data into ASPMS.

![Import Running Times](img/import.png)

There are 5 profiles for running times. Data will be imported for the profile that is currently open.

>**NOTE :** If wrong file is uploaded, appropriate message will be displayed.

![Error](img/ValidationImportRTL.png)

*Home*

Click on **Home** to go back to the home page.

### Views
#### Maximum Performance
![Maximum Performance](img/RTL_MP.png)
Displays the Maximum Performance Running Time Lists.

#### Five Percent Coasting
![Five Percent Coasting](img/RTL_FPC.png)
Displays the Five Percent Coasting Running Time Lists.

#### Eight Percent Coasting
![Eight Percent Coasting](img/RTL_EPC.png)
Displays the Eight Percent Coasting Running Time Lists.

#### Energy Saving
![Energy Saving](img/RTL_ES.png)
Displays the Energy Saving Running Time Lists.

#### Full Coasting
![Full Coasting](img/RTL_FC.png)
Displays the Full Coasting Running Time Lists.

### How to Edit
In each of the above 5 views, follow these steps to edit:
1. Click on the text box associated with the stop points that you want to edit.
2. Replace the existing value with the new value.

### Accepted Values
All running times must be positive numbers. Maximum value is 999. Fields can **NOT** be left blank.


>**NOTE :** Running Time Lists cannot be changed if invalid information is provided. Appropriate error message will be displayed.
![Error](img/ValidationRTL.png)

## Dwell Time Sets
This page consists of 2 regions,
1. Sidebar (Left, minor column)
2. View region (Right, occupies most of the screen)

### Sidebar
*Dwell Time Set 1*

This button opens the Dwell Time Set 1 view in the view region.

*Dwell Time Set 2*

This button opens the Dwell Time Set 2 view in the view region.

*Dwell Time Set 3*

This button opens the Dwell Time Set 3 view in the view region.

*Save Changes*

Click on **Save Changes** and then click **OK** in the success dialog.

*Home*

Click on **Home** to go back to the home page.

### Views
#### Dwell Time Set 1
![Maximum Performance](img/DTS_1.png)
Displays the dwell times for Dwell Time Set 1.

#### Dwell Time Set 2
![Five Percent Coasting](img/DTS_2.png)
Displays the dwell times for Dwell Time Set 2.

#### Dwell Time Set 3
![Eight Percent Coasting](img/DTS_3.png)
Displays the dwell times for Dwell Time Set 3.

### How to Edit
In each of the above 3 views, follow these steps to edit:
1. Click on the text box associated with the stop point that you want to edit.
2. Replace the existing value with the new value.

### Accepted Values
All dwell times must be positive numbers. Maximum value is 999. Decimal values are **NOT** accepted. Fields can **NOT** be left blank.


>**NOTE :** Dwell Time Sets cannot be changed if invalid information is provided. Appropriate error message will be displayed.
![Error](img/ValidationDTS.png)

## Alarm Levels
![Alarm Levels](img/AlarmLevels.png)
This page displays the alarm level corresponding to each event.

### How to Edit
To edit the alarm level for an event
1. Click the drop down list corresponding to the event.
2. Select the new alarm level from the drop down list.

### How to Save
Click on **Save Changes** and then click **OK** to save the changes.

*Home*

Click on **Home** to go back to the home page.

## Change Password
![Change Password](img/ChangePassword.png)
This page allows you to edit the user password.

### How to Edit
Just enter the new password in the text box.

### How to Save
Click on **Save Changes** and then click **OK** in the success dialog.

### Accepted Values
Spaces are not acceptable, all other characters are accepted. Password must **NOT** be left blank. Maximum length is 25 characters.

>**NOTE :** Password cannot be changed if invalid information is provided. Appropriate error message will be displayed.
![Error](img/ValidationCP.png)

*Home*

Click on **Home** to go back to the Home Page.

# Rolling Stock Controller View
## Login
![Login screen](img/LoginRSC.png)

The login screen is the first screen encountered when opening ASPMS.

### How to Login
The default password is "passwordrsc" (without the quotes). This can be changed to some other value once the user has logged in.

In order to login, enter the password in the text box, and press enter or click Login.

## Rolling Stock Roster
![Rolling Stock Roster](img/RollingStock.png)
This page displays a list of the Rolling Stock, showing each Rake ID along with its description.

*Change Password*

This link can be clicked to change the existing password via the Change Password page.

*Logout*

Click the logout button to log out of ASPMS.

### How to Add Rolling Stock
![Rolling Stock Roster](img/AddRollingStock.png)
Click on **Add Rolling Stock** button at the end of the page. It will add an empty row to the table. Enter the new Rake ID and the associated description.

### Accepted Values
All Rake IDs must be of the form 801, 802, 803 .. 899, 8A0 ... 8F9. Maximum value is 8F9. Fields can **NOT** be left blank.

The description has a maximum length of 25 characters.

### How to Delete Rolling Stock

Select the Rolling Stock to be deleted by clicking on the checkboxes next to them. Then click on **Delete Rolling Stock** button to delete the selected Rolling Stock.

### How to Save
Click on **Save Changes** and then **OK** to save the changes.

>**NOTE :** Rolling Stock cannot be added if invalid information is provided. Appropriate error message will be displayed.
![Error](img/ValidationRS.png)

## Change Password
![Change Password](img/ChangePasswordRSC.png)
This page allows you to edit the user password.

### How to Edit
Just enter the new password in the text box.

### How to Save
Click on **Save Changes** and then click **OK** in the success dialog.

### Accepted Values
Spaces are not acceptable, all other characters are accepted. Password must **NOT** be left blank. Maximum length is 25 characters.

>**NOTE :** Password cannot be changed if invalid information is provided. Appropriate error message will be displayed.
![Error](img/ValidationCPRSC.png)

*Home*

Click on **Home** to go back to the Home Page.

# Crew Controller View
## Login
![Login screen](img/LoginCrew.png)

The login screen is the first screen encountered when opening ASPMS.

### How to Login
The default password is "passwordcrew" (without the quotes). This can be changed to some other value once the user has logged in.

In order to login, enter the password in the text box, and press enter or click Login.

## Crew Roster
![Crew Roster](img/Crew.png)
This page displays a list of the Crew, showing each Crew ID along with its description.

*Change Password*

This link can be clicked to change the existing password via the Change Password page.

*Logout*

Click the logout button to log out of ASPMS.

### How to Add Crew
![Crew Roster](img/AddCrew.png)
Click on **Add Crew** button at the end of the page. It will add an empty row to the table. Enter the new Crew ID and the associated description.

### Accepted Values
All Crew IDs must be a number. Maximum value is 9999. Fields can **NOT** be left blank.

The description has a maximum length of 25 characters.

### How to Delete Crew

Select the Crew to be deleted by clicking on the checkboxes next to them. Then click on **Delete Crew** button to delete the selected Crew.

### How to Save
Click on **Save Changes** and then **OK** to save the changes.

>**NOTE :** Crew cannot be added if invalid information is provided. Appropriate error message will be displayed.
![Error](img/ValidationCrew.png)

## Change Password
![Change Password](img/ChangePasswordCrew.png)
This page allows you to edit the user password.

### How to Edit
Just enter the new password in the text box.

### How to Save
Click on **Save Changes** and then click **OK** in the success dialog.

### Accepted Values
Spaces are not acceptable, all other characters are accepted. Password must **NOT** be left blank. Maximum length is 25 characters.

>**NOTE :** Password cannot be changed if invalid information is provided. Appropriate error message will be displayed.
![Error](img/ValidationCPCrew.png)

*Home*

Click on **Home** to go back to the Home Page.
