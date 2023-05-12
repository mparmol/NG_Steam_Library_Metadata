##Steam user Library Metadata

#Useful script to create a table with all (or almost all) the information from your Steam library from different Steam databases

# Loading needed packages

suppressPackageStartupMessages(require("data.table"))
suppressPackageStartupMessages(require("stringr"))
suppressPackageStartupMessages(require("stringi"))
suppressPackageStartupMessages(require("rvest"))
suppressPackageStartupMessages(require("RCurl"))
suppressPackageStartupMessages(require("readr"))
suppressPackageStartupMessages(require("lubridate"))
suppressPackageStartupMessages(require("optparse"))
suppressPackageStartupMessages(require("progress"))
suppressPackageStartupMessages(require("httr"))
suppressPackageStartupMessages(require("jsonlite"))

######## Input control block

option_list = list(
  make_option(c("-i", "--input"), type="character", default=NULL, 
              help="Steam user name or ID", metavar="character"),
  make_option(c("-a", "--appid"), type="character", default=NULL, 
              help="Steam user APP_ID", metavar="character")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

if (is.null(opt$input) | is.null(opt$appid)){
  print_help(opt_parser)
  stop("Both argument must be supplied (Steam user name or ID and APP_ID).", call.=FALSE)
}else {
   id_search=opt$input
}

########

Sys.setlocale("LC_TIME", "C") # Setting database language to English

isEmpty <- function(x) { #This function checks if a data frame is empty or not
  return(length(x)==0)
}

word2num <- function(word){ #Useful function to convert between ordinal and roman numerals
    wsplit <- strsplit(tolower(word)," ")[[1]]
    one_digits <- list(zero=0, one=1, two=2, three=3, four=4, five=5,
                       six=6, seven=7, eight=8, nine=9)
    teens <- list(eleven=11, twelve=12, thirteen=13, fourteen=14, fifteen=15,
                  sixteen=16, seventeen=17, eighteen=18, nineteen=19)
    ten_digits <- list(ten=10, twenty=20, thirty=30, forty=40, fifty=50,
                       sixty=60, seventy=70, eighty=80, ninety=90)
    doubles <- c(teens,ten_digits)
    out <- 0
    i <- 1
    while(i <= length(wsplit)){
        j <- 1
        if(i==1 && wsplit[i]=="hundred")
            temp <- 100
        else if(i==1 && wsplit[i]=="thousand")
            temp <- 1000
        else if(wsplit[i] %in% names(one_digits))
            temp <- as.numeric(one_digits[wsplit[i]])
        else if(wsplit[i] %in% names(teens))
            temp <- as.numeric(teens[wsplit[i]])
        else if(wsplit[i] %in% names(ten_digits))
            temp <- (as.numeric(ten_digits[wsplit[i]]))
        if(i < length(wsplit) && wsplit[i+1]=="hundred"){
            if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
                out <- out + 100*temp
            else
                out <- 100*(out + temp)
            j <- 2
        }
        else if(i < length(wsplit) && wsplit[i+1]=="thousand"){
            if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
                out <- out + 1000*temp
            else
                out <- 1000*(out + temp)
            j <- 2
        }
        else if(i < length(wsplit) && wsplit[i+1] %in% names(doubles)){
            temp <- temp*100
            out <- out + temp
        }
        else{
            out <- out + temp
        }
        i <- i + j
    }
    return(list(word,out))
}

cleanFun <- function(htmlString) { #Clean html coding from string, replacing for a blank
  return(gsub("<.*?>", "", htmlString))
}

cleanFun2 <- function(htmlString) { #Clean html coding from string, replacing for a space
  return(gsub("<.*?>", ";?;", htmlString))
}

##### Get full user name data. Get both user ID and pseudonym if available

user_info<-getURL(paste("https://www.steamidfinder.com/lookup/",id_search,sep=""),.opts=curlOptions(followlocation=TRUE)) #Get user information from steamfinder


steam_link=paste("https://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=",opt$appid,"&steamid=",strsplit(strsplit(strsplit(user_info,"profile<")[[1]][2],"\" rel=\"noopener")[[1]][1],"/")[[1]][length(strsplit(strsplit(strsplit(user_info,"profile<")[[1]][2],"\" rel=\"noopener")[[1]][1],"/")[[1]])],"&include_appinfo=1&format=json",sep="")


#https://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=APP_ID&steamid=", steam_id, "&include_appinfo=1&format=json
#https://api.steampowered.com/ISteamUserStats/GetPlayerAchievements/v1/?key=APP_ID&appid=7850&steamid=76561198118578417&include_appinfo=1&format=json


#steam_link=paste("https://steamcommunity.com/profiles/",strsplit(strsplit(strsplit(user_info,"profile<")[[1]][2],"\" rel=\"noopener")[[1]][1],"/")[[1]][length(strsplit(strsplit(strsplit(user_info,"profile<")[[1]][2],"\" rel=\"noopener")[[1]][1],"/")[[1]])],"/games/?tab=all",sep="")
steam_link_achiv=paste("https://steamcommunity.com/profiles/",strsplit(strsplit(strsplit(user_info,"profile<")[[1]][2],"\" rel=\"noopener")[[1]][1],"/")[[1]][length(strsplit(strsplit(strsplit(user_info,"profile<")[[1]][2],"\" rel=\"noopener")[[1]][1],"/")[[1]])],"/games/?tab=perfect",sep="")

info_Steam_removed<-paste("https://steam-tracker.com/scan/",strsplit(strsplit(strsplit(user_info,"profile<")[[1]][2],"\" rel=\"noopener")[[1]][1],"/")[[1]][length(strsplit(strsplit(strsplit(user_info,"profile<")[[1]][2],"\" rel=\"noopener")[[1]][1],"/")[[1]])],sep="")

#### Metadata scraping from HowLongToBeat

options(warn=-1) # We exclude warnings to show on the screen

############################################################################################Updates SteamSpy

if(!file.exists(paste("Steam_Metadata_SSpySteam_",id_search,".txt",sep=""))) # Useful when stoping the analysis at this point. Checkpoint.
{
  print("Scraping more data: votes, developer, publisher, release date, pc requirements...")

  game_list<-read.delim(paste("Game_HowLong_",id_search,".txt",sep=""),header=F)

  #### Más metadato

  binded_table<-NULL

  system("rm -rf index.html")
  system("wget -q https://api.steampowered.com/ISteamApps/GetAppList/v2/") # Using the Steam API to get game data related.
      
  AppID_List <- read_file("index.html")

  AppID_List<-gsub("\\\"\\},\\{\\\"appid\\\":","``",AppID_List)
  AppID_List<-gsub(",\"name\":\"","``",AppID_List)
  AppID_List<-gsub("\\{\\\"applist\\\":\\{\\\"apps\\\":\\[\\{\\\"appid\\\":","",AppID_List)
  AppID_List<-gsub("\\\"\\}\\]\\}\\}","",AppID_List)

  res_games<-data.frame(matrix(strsplit(AppID_List,"\\`\\`")[[1]],ncol=2,byrow=T))
  binded_table<-rbind(binded_table,res_games)
  
  print(paste("Total entries in Steam database: ",dim(res_games)[1])) # This is the total number Steam AppID we are getting information for

  res_games<-binded_table[!duplicated(binded_table),]

  game_list[,9]<-res_games[match(game_list[,7],res_games[,2]),1]
  game_list[,10]<-res_games[match(game_list[,7],res_games[,2]),2]

  pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]", # We iniated another progress bar
                    total = dim(game_list)[1],
                    complete = "=",   # Completion bar character
                    incomplete = "-", # Incomplete bar character
                    current = ">",    # Current bar character
                    clear = FALSE,    # If TRUE, clears the bar when finish
                    width = 100)      # Width of the progress bar

  for(i in 1:dim(game_list)[1])
  {
    pb$tick()

    meta_game=NA

    if(!is.na(game_list[i,8]))
    {
      steamSpy <- FALSE

      while(!grepl("name",meta_game) | !steamSpy)
      {
        tmp<-tryCatch({
          meta_game<-getURL(paste("https://steamspy.com/api.php?request=appdetails&appid=",game_list[i,8],sep="")) # We complete the information using steamspy API. Here we save the information for genre, votes, developer...
          steamSpy<-TRUE
        },
        error=function(e){
        print("ERROR: SteamSpy API Timeout, waiting 1 min")
        Sys.sleep(60)
        #getURL(paste("https://steamspy.com/api.php?request=appdetails&appid=",game_list[i,8],sep=""))
        })
      }

      game_list[i,10]<-strsplit(strsplit(meta_game,"name\\\":\\\"")[[1]][2],"\\\",\\\"")[[1]][1]
      game_list[i,11]<-strsplit(strsplit(meta_game,"genre\\\":\\\"")[[1]][2],"\\\",\\\"")[[1]][1]
      game_list[i,12]<-strsplit(strsplit(meta_game,"positive\\\":")[[1]][2],",\\\"")[[1]][1]
      game_list[i,13]<-strsplit(strsplit(meta_game,"negative\\\":")[[1]][2],",\\\"")[[1]][1]
      game_list[i,14]<-strsplit(strsplit(meta_game,"developer\\\":\\\"")[[1]][2],"\\\",\\\"")[[1]][1]
      game_list[i,15]<-strsplit(strsplit(meta_game,"publisher\\\":\\\"")[[1]][2],"\\\",\\\"")[[1]][1]
      if(!grepl('tags\\\":\\[\\]',meta_game))
      {
        game_list[i,16]<-paste(sapply(strsplit(strsplit(strsplit(meta_game,"tags\\\":\\{")[[1]][2], ',')[[1]],'\\\"'),"[[",2),collapse=", ")
      }

      steamS <- FALSE

      while(!steamS)
      {
        tmp<-tryCatch({
          meta_game<-getURL(paste("https://store.steampowered.com/api/appdetails/?cc=EU&appids=",game_list[i,8],sep="")) # Using the Steam API we get the release date and pc requirements information
          steamS<-TRUE
        },
        error=function(e){
        print("ERROR: Steam API Timeout, waiting 1 min")
        Sys.sleep(60)
        #getURL(paste("https://store.steampowered.com/api/appdetails/?cc=EU&appids=",game_list[i,8],sep=""))
        })
      }

      if(grepl("\"success\"\\:true",meta_game))
      {
        app_id_gen<-strsplit(strsplit(meta_game,"\":")[[1]][1],"\"")[[1]][2]
        game_list[match(app_id_gen,game_list[,8]),19]<-format(ymd(paste(strsplit(strsplit(strsplit(strsplit(meta_game,"release_date")[[1]][2],"\\\"}")[[1]][1],"\\:\\\"")[[1]][2]," ")[[1]][3],strsplit(strsplit(strsplit(strsplit(meta_game,"release_date")[[1]][2],"\\\"}")[[1]][1],"\\:\\\"")[[1]][2]," ")[[1]][2],strsplit(strsplit(strsplit(strsplit(meta_game,"release_date")[[1]][2],"\\\"}")[[1]][1],"\\:\\\"")[[1]][2]," ")[[1]][1],sep=" ")), "%d-%b-%Y")
        game_list[match(app_id_gen,game_list[,8]),20]<-gsub("\t"," ",paste(unlist(strsplit(gsub("Minimum:","",strsplit(gsub("\",\"recommended\":\""," ",cleanFun(strsplit(strsplit(meta_game,"pc_requirements\\\"\\:\\{\\\"minimum\\\":\\\"")[[1]][2],"\"},\\\"mac_requirements")[[1]][1])),"Recommended:")[[1]][1]),"\\\\r|\\\\n|\\\\t"))[unlist(strsplit(gsub("Minimum:","",strsplit(gsub("\",\"recommended\":\""," ",cleanFun(strsplit(strsplit(meta_game,"pc_requirements\\\"\\:\\{\\\"minimum\\\":\\\"")[[1]][2],"\"},\\\"mac_requirements")[[1]][1])),"Recommended:")[[1]][1]),"\\\\r|\\\\n|\\\\t")) != ""],collapse = " "))
        game_list[match(app_id_gen,game_list[,8]),21]<-gsub("\t"," ",paste(unlist(strsplit(strsplit(gsub("\",\"recommended\":\""," ",cleanFun(strsplit(strsplit(meta_game,"pc_requirements\\\"\\:\\{\\\"minimum\\\":\\\"")[[1]][2],"\"},\\\"mac_requirements")[[1]][1])),"Recommended:")[[1]][2],"\\\\r|\\\\n|\\\\t"))[unlist(strsplit(strsplit(gsub("\",\"recommended\":\""," ",cleanFun(strsplit(strsplit(meta_game,"pc_requirements\\\"\\:\\{\\\"minimum\\\":\\\"")[[1]][2],"\"},\\\"mac_requirements")[[1]][1])),"Recommended:")[[1]][2],"\\\\r|\\\\n|\\\\t")) != ""],collapse = " "))
      }

      Sys.sleep(1)
    }


  }

  write.table(game_list,paste("Steam_Metadata_SSpySteam_",id_search,".txt",sep=""),quote = F,row.names = F,col.names = F,sep = "\t")
  #system(paste("rm -rf ",paste("Game_HowLong_",id_search,".txt",sep=""),sep=""))
  system("rm -rf index.html")
}
