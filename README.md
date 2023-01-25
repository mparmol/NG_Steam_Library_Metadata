<!-- README.md is generated from README.Rmd. Please edit that file -->

# **NextGame project - Steam Library Metadata**

The purpose of this project is the creation of the metadata table for a Steam specefic user.

The information is scrapped from different databases:
· [Steam](https://store.steampowered.com/) 
· [HowLongToBeat](https://howlongtobeat.com/): Database with potential completion time for many games. It is maintained by users, as records are inserted manually. Showed time data is a computed measure from all collaborators.
· [SteamSpy](https://steamspy.com/): Steam data database maintained by their own API. They show certain metadata in an easier way compared to Steam database. 
· [Steam-tracker](https://steam-tracker.com/): Steam-tracker aims to show different data from certain Steam users, where the most interesting would be the information about removed games.

For this purpose different APIs are used.
· [Steam](https://store.steampowered.com/api/appdetails/)
· [HowLongToBeat](https://github.com/ckatzorke/howlongtobeat)
· [SteamSpy](https://steamspy.com/api.php)

### **Steam_Metadata.R**

### **Installation**

### Install miniconda (available for Windows and Linux):

https://docs.conda.io/en/latest/miniconda.html

### Create conda environment with R:

> conda create -n r_env r-essentials r-base

### Install Howlongtobeat API

> https://github.com/ckatzorke/howlongtobeat

Install the dependency

> npm install howlongtobeat --save

### **R dependencies**

**Packages**

· [`data.table`](https://cran.r-project.org/web/packages/data.table/index.html) v`r packageVersion("data.table")`. 
· [`stringr`](https://cran.r-project.org/web/packages/stringr/index.html) v`r packageVersion("stringr")`.
· [`stringi`](https://cran.r-project.org/web/packages/stringi/index.html) v`r packageVersion("stringi")`. 
· [`rvest`](https://cran.r-project.org/web/packages/rvest/index.html) v`r packageVersion("rvest")`. 
· [`RCurl`](https://cran.r-project.org/web/packages/RCurl/index.html) v`r packageVersion("RCurl")`. 
· [`readr`](https://cran.r-project.org/web/packages/readr/index.html) v`r packageVersion("readr")`. 
· [`lubridate`](https://cran.r-project.org/web/packages/lubridate/index.html) v`r packageVersion("lubridate")`. 
· [`optparse`](https://cran.r-project.org/web/packages/optparse/index.html) v`r packageVersion("optparse")`. 
· [`progress`](https://cran.r-project.org/web/packages/progress/index.html) v`r packageVersion("progress")`. 

### **Usage**




1. Load conda environment. 
    > conda activate r_env
2. Run script indicating
    > Rscript NextGame.R 

### **Troubleshooting**

Sometimes you will get the error:


> (node:3834371) UnhandledPromiseRejectionWarning: Unhandled promise rejection. This error originated either by throwing inside of an async function without a catch block, or by rejecting a promise which was not handled with .catch(). To terminate the node process on unhandled promise rejection, use the CLI flag `--unhandled-rejections=strict` (see https://nodejs.org/api/cli.html#cli_unhandled_rejections_mode). (rejection id: 2)
(node:3834371) [DEP0018] DeprecationWarning: Unhandled promise rejections are deprecated. In the future, promise rejections that are not handled will terminate the Node.js process with a non-zero exit code.

Don't worry! It's the HowLongToBeat API trying to download games information

