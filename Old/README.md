# **NextGame - Database creation**
Utility to create personal account Steam metadata table.

## What do you need:

### Install miniconda (available for Windows and Linux):

https://docs.conda.io/en/latest/miniconda.html

### Create conda environment with R:

> conda create -n r_env r-essentials r-base

### Install Howlongtobeat API

> https://github.com/ckatzorke/howlongtobeat

Install the dependency

> npm install howlongtobeat --save

### Usage:

1. Open NextGame.R file and modifify line 16 with your own Steam game link
    > system("wget https://steamcommunity.com/id/marko_pakete/games/?tab=all")
2. Load conda environment. 
    > conda activate r_env
3. Run script
    > Rscript NextGame.R 

Troubleshooting:

Sometimes you will get the error:


> (node:3834371) UnhandledPromiseRejectionWarning: Unhandled promise rejection. This error originated either by throwing inside of an async function without a catch block, or by rejecting a promise which was not handled with .catch(). To terminate the node process on unhandled promise rejection, use the CLI flag `--unhandled-rejections=strict` (see https://nodejs.org/api/cli.html#cli_unhandled_rejections_mode). (rejection id: 2)
(node:3834371) [DEP0018] DeprecationWarning: Unhandled promise rejections are deprecated. In the future, promise rejections that are not handled will terminate the Node.js process with a non-zero exit code.

Don't worry! It's the HowLongToBeat API trying to download games information

![NewGame](Cap.png "NewGame")

