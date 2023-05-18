# **NextGame project - Steam Library Metadata** ![Stars](https://img.shields.io/github/stars/mparmol/NG_Steam_Library_Metadata)


Steam is currently the biggest video game distribution platform. As the number of games in their database increases, so does the number of games in the libraries of its users. This can result in the problem of not being aware of the games in one's library and their characteristics. When deciding on which game to start, several factors come into play, such as genre, user rating, completion time, and PC requirements. The aim of this project is to create a metadata table for a specific Steam library user, containing all relevant information about the games in a structured and easily readable format. The examination of the metadata can be conducted using the [Metadata Analysis tool](https://github.com/mparmol/NG_Metadata_Analysis) as a guide.

The information is scrapped from five different databases:

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

Anaconda or Miniconda is a software application used to create isolated environments for package installations. To use it, simply download the appropriate version for your operating system from the Anaconda website and install it.

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
· [`progress`](https://cran.r-project.org/web/packages/progress/index.html) v1.2.2\
· [`httr`](https://cran.r-project.org/web/packages/httr/index.html) v1.4.4\
· [`jsonlite`](https://cran.r-project.org/web/packages/jsonlite/index.html) v1.8.3

You can install them manually with the command `ìnstall.packages()` or as a bulk:

```R
install.packages(c("data.table", "stringr", "stringi", "rvest", "RCurl", "readr", "lubridate", "optparse", "progress", "httr", "jsonlite"))
```

   - Exit R

### **Usage**

· User profile from which you want to fetch information must be set to public. You can follow [Team17 tutorial](https://support.team17.com/hc/en-gb/articles/360003517458-Steam-Privacy-Settings) to change it to public in case you don't have it yet. You can get the user ID by accesing it profile on Steam and looking at the URL. For example, my account would be:
> https://steamcommunity.com/id/marko_pakete/ (marko_pakete)

· Steam Web API Token is needed. You can follow [this link](https://steamcommunity.com/dev/apikey) to get your personal token. 

```bash
Usage: Steam_Metadata.R [options]


Options:
-i CHARACTER, --input=CHARACTER
	Steam user name or ID

-a CHARACTER, --appid=CHARACTER
   Steam Web API Token

-h, --help
	Show this help message and exit
```

1. Load conda environment. 

```bash
> conda activate steam_metadata
```    

2. Run script with the parameter *-i* followed by user pseudonym or account id.

```bash
> Rscript Steam_Metadata.R -i *user_name* -a *API_token*
```    
3. Relax and chill.[^1]

**Example**

```
Rscript Steam_Metadata.R -i marko_pakete -a API_TOKEN
```

### **Output**

The script generates two tables as output: 

- `Steam_Metadata_Full_*user_id*`: Here we can find all columns generated from the tool. Some columns could be interesting for analysts, like the similarity one, which shows how similar is the game name compared to the name found in HowLongToBeat database.
- `Steam_Library_Metadata_*user_id*`: Processed and cleaned metadata table. It has 16 columns with the following information:

| Name | AppID | Genre | Tags | Votes_total | Positive_rating | Played_time (h) | Time_to_finish (h) | Time_to_complete (h) | Achievements | First_achievement | Last_achievement | 100%_Completed | Developer | Publisher | Release_date | Removed_game | Minimum requirements | Recommended requirements |
| ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- |
| Game name | AppID | A game could be assigned to more than one genre | How game is categorized | Total votes to date for a game | Percentage of positive votes (positive/positive+negative) | Total played time for a game | Time to finish the main campaign | Time to obtain all achivements or finish the game witll al possible extra | Games with achievements | Games with all achievements unlocked | Developer | Publisher (could change with time) | Final version release date | Not available for purchase games | PC minimun requierements | PC recommended requirements |   

### **Troubleshooting**

Sometimes you will get a similar error while processing HowLongToBeat:


> (node:3834371) UnhandledPromiseRejectionWarning: Unhandled promise rejection. This error originated either by throwing inside of an async function without a catch block, or by rejecting a promise which was not handled with .catch(). To terminate the node process on unhandled promise rejection, use the CLI flag `--unhandled-rejections=strict` (see https://nodejs.org/api/cli.html#cli_unhandled_rejections_mode). (rejection id: 2)
(node:3834371) [DEP0018] DeprecationWarning: Unhandled promise rejections are deprecated. In the future, promise rejections that are not handled will terminate the Node.js process with a non-zero exit code.

Don't worry! It's the HowLongToBeat API trying to download games information. It will eventually continue the analysis.


