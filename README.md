# **NextGame project - Steam Library Metadata** ![Stars](https://img.shields.io/github/stars/mparmol/NG_Steam_Library_Metadata)


Steam is the largest video game distribution service nowadays. As the number of videogames found in their database increase, it does for its users' libraries too. This could end in the "problem" of not knowing what games you own, or more concretely, their characteristics. If you want to start a new game it could depend on many factors, like genre, user rating, how much time will it take to finish it or the pc requirements. The purpose of this project is the creation of the metadata table for a Steam library-specific user, with all the possible information related to the games it owns, in a structured easy-readable way.

The information is scrapped from four different databases:

· [Steam](https://store.steampowered.com/)\
· [HowLongToBeat](https://howlongtobeat.com/): Database with potential completion time for many games. It is maintained by users, as records are inserted manually. Showed time data is a computed measure from all collaborators.\
· [SteamIdFinder](https://www.steamidfinder.com/): Check any SteamID or user pseudonym for any user profile information.\
· [SteamSpy](https://steamspy.com/): Steam data database maintained by their own API. They show certain metadata more readily compared to Steam's database.\
· [Steam-tracker](https://steam-tracker.com/): Steam-tracker aims to show different data from certain Steam users, where the most interesting would be the information about removed games.

For this purpose different APIs are used.

· [Steam](https://store.steampowered.com/api/appdetails/)\
· [HowLongToBeat](https://github.com/ckatzorke/howlongtobeat)\
· [SteamSpy](https://steamspy.com/api.php)

## **Steam_Metadata.R**

#### [**1- Installation**](#installation)
#### [**2- Usage**](#usage)
#### [**3- Output**](#output)
#### [**4- Troubleshooting**](#troubleshooting)

### **Installation**

· The tool could be used in any OS (Windows/Linux/Mac).

Steam_Metadata.R is an R-scripted tool that will require R locally installed to run. If you already have R installed in a Conda environment you can skip this step and move forward to [step 3](#3--install-howlongtobeat-api). 

#### 1- Download and install miniconda (available for Windows and Linux)

Anaconda (or miniconda) is an environment creator application to install packages independencies safely. Download your OS version from their webpage. Install and configure it. 

> https://docs.conda.io/en/latest/miniconda.html

#### 2- Create conda environment with R (R \>= 4.1.0)

Execute miniconda and create a new environment with R. It will install the latest R version, but check it is at least version \>=4.1.0.

```bash 
> conda create -n steam_metadata r-essentials r-base
```

#### 3- Install HowLongToBeat API

HowLongToBeat API is based on javascript coding: https://github.com/ckatzorke/howlongtobeat. Follow their tutorial to install needed dependencies and support them for their wrapper (thanks!).

#### 4- Install R dependencies

Some R packages are needed to fetch data and process it. 

   - First, you will have to load your conda environment
    
```bash
> conda activate steam_metadata
```    
   - Load R
 
```bash
> R
```
   - Install dependecies
    
**Packages (recommended version)**

· [`data.table`](https://cran.r-project.org/web/packages/data.table/index.html) v1.14.6\
· [`stringr`](https://cran.r-project.org/web/packages/stringr/index.html) v1.4.1\
· [`stringi`](https://cran.r-project.org/web/packages/stringi/index.html) v1.7.8\
· [`rvest`](https://cran.r-project.org/web/packages/rvest/index.html) v1.0.3\
· [`RCurl`](https://cran.r-project.org/web/packages/RCurl/index.html) v1.98-1.9\
· [`readr`](https://cran.r-project.org/web/packages/readr/index.html) v2.1.3\
· [`lubridate`](https://cran.r-project.org/web/packages/lubridate/index.html) v1.8.0\
· [`optparse`](https://cran.r-project.org/web/packages/optparse/index.html) v1.7.3\
· [`progress`](https://cran.r-project.org/web/packages/progress/index.html) v1.2.2

You can install them manually with the command `ìnstall.packages()` or as a bulk:

```R
install.packages(c("data.table", "stringr", "stringi", "rvest", "RCurl", "readr", "lubridate", "optparse", "progress"))
```

   - Exit R

### **Usage**

First, the user profile from which you want to fetch information must be set to public. You can follow [Team17 tutorial](https://support.team17.com/hc/en-gb/articles/360003517458-Steam-Privacy-Settings) to change it to public in case you don't have it yet. You can get the user ID by accesing it profile on Steam and looking at the URL. For example, my account would be:
> https://steamcommunity.com/id/marko_pakete/ (marko_pakete)

```bash
Usage: Steam_Metadata.R [options]


Options:
-i CHARACTER, --input=CHARACTER
	Steam user name or ID

-h, --help
	Show this help message and exit
```

1. Load conda environment. 

```bash
> conda activate steam_metadata
```    

2. Run script with the parameter *-i* followed by user pseudonym or account id.

```bash
> Rscript Steam_Metadata.R -i *user_name*
```    
3. Relax and chill.[^1]

**Example**

```
Rscript Steam_Metadata.R -i marko_pakete
```

### **Output**

The script generates two tables as output: 

- `Steam_Metadata_Full_*user_id*`: Here we can find all columns generated from the tool. Some columns could be interesting for analysts, like the similarity one, which shows how similar is the game name compared to the name found in HowLongToBeat database.
- `Steam_Library_Metadata_*user_id*`: Processed and cleaned metadata table. It has 16 columns with the following information:

| Name | AppID | Genre | Tags | Votes_total | Positive_rating | Played_time (h) | Time_to_finish (h) | Time_to_complete (h) | 100%_Completed | Developer | Publisher | Release_date | Removed_game | Minimum requirements | Recommended requirements |
| ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- |
| Game name | AppID | A game could be assigned to more than one genre | How game is categorized | Total votes to date for a game | Percentage of positive votes (positive/positive+negative) | Total played time for a game | Time to finish the main campaign | Time to obtain all achivements or finish the game witll al possible extra | Games with all achievements unlocked | Developer | Publisher (could change with time) | Final version release date | Not available for purchase games | PC minimun requierements | PC recommended requirements |   

### **Troubleshooting**

Sometimes you will get a similar error while processing HowLongToBeat:


> (node:3834371) UnhandledPromiseRejectionWarning: Unhandled promise rejection. This error originated either by throwing inside of an async function without a catch block, or by rejecting a promise which was not handled with .catch(). To terminate the node process on unhandled promise rejection, use the CLI flag `--unhandled-rejections=strict` (see https://nodejs.org/api/cli.html#cli_unhandled_rejections_mode). (rejection id: 2)
(node:3834371) [DEP0018] DeprecationWarning: Unhandled promise rejections are deprecated. In the future, promise rejections that are not handled will terminate the Node.js process with a non-zero exit code.

Don't worry! It's the HowLongToBeat API trying to download games information. It will eventually continue the analysis.

On the other hand, you can get the following error when using Steam API:

> Error in function (type, msg, asError = TRUE)  :
  Failed to connect to store.steampowered.com port 443 after 21036 ms: Couldn't connect to server

In this case, run the script again. It will automatically continue from the last checkpoint file.

Anothe possible error could come while using SteamSpy API. Just re-run the script.

> Error in function (type, msg, asError = TRUE)  :
  getaddrinfo() thread failed to start

[^1]: Excution time will depend on the library size, but time range from: **100 Games~** -> 8 min, **3000 Games~** -> 4 h, **15k+ Games~** -> 25h (depending on the API's). Working to improve performance!




