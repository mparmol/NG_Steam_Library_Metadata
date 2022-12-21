##Updates

### Más info: https://nik-davis.github.io/posts/2019/steam-data-collection/

#API references:
#https://partner.steamgames.com/doc/webapi
#https://wiki.teamfortress.com/wiki/User:RJackson/StorefrontAPI
#https://steamapi.xpaw.me/#
#https://steamspy.com/api.php


### Extract game list, from windows

require("data.table")
require("stringr")
require("rvest")
library(RCurl) # ESte
library(XML) #O este, sobra


isEmpty <- function(x) { #This function checks if a data frame is empty or not
  return(length(x)==0)
}

game_list<-read.delim("Games_HowLong_v9.txt")

#### Más metadato

### Pillar AppID

### Más info: https://partner.steamgames.com/doc/webapi/ISteamApps#GetAppList


if(!file.exists("TODO.txt"))
{
  AppID_List <- html_text(html_node(read_html("https://api.steampowered.com/ISteamApps/GetAppList/v2/"),"p"))

  res_games<-data.frame(matrix(ncol=2,nrow=str_count(AppID_List,'"name"')[1]))

  for(i in 2:(str_count(AppID_List,'"name"')[1]+1))
  {
    res_games[i,1]<-strsplit(strsplit(strsplit(sapply(strsplit(AppID_List, '\\{'), "[[", i),"\\}")[[1]][1],":")[[1]][2],",")[[1]][1]
    res_games[i,2]<-strsplit(strsplit(strsplit(sapply(strsplit(AppID_List, '\\{'), "[[", i),"\\}")[[1]][1],":")[[1]][3],'\\\"')[[1]][2]
  }

  write.table(res_games,"TODO.txt",quote = F,row.names = F,col.names = F)
}


game_list$AppID<-res_games[match(game_list[,1],res_games[,2]),1]
game_list$AppID_name<-res_games[match(game_list[,1],res_games[,2]),2]
game_list$AppID_2<-res_games[match(game_list[,2],res_games[,2]),1]
game_list$AppID_name_2<-res_games[match(game_list[,2],res_games[,2]),2]

write.table(game_list,"Games_HowLong_AppID.txt",quote = F,row.names = F,col.names = F,sep = "\t")

#library(rvest)
#my_df <- as.data.frame(read_html(index.html) %>% html_table(fill=TRUE))

### Datos del juego desde AppID

### Más info: https://steamspy.com/api.php

for(i in 1:dim(game_list)[1])
{
  if(!is.na(game_list[i,7]))
  {
    meta_juego<-getURL(paste("https://steamspy.com/api.php?request=appdetails&appid=",game_list[i,7],sep=""))
    game_list[i,11]<-strsplit(strsplit(meta_juego,"genre\\\":\\\"")[[1]][2],"\\\",\\\"")[[1]][1]
    game_list[i,12]<-strsplit(strsplit(meta_juego,"positive\\\":")[[1]][2],",\\\"")[[1]][1]
    game_list[i,13]<-strsplit(strsplit(meta_juego,"negative\\\":")[[1]][2],",\\\"")[[1]][1]
    game_list[i,14]<-strsplit(strsplit(meta_juego,"developer\\\":\\\"")[[1]][2],"\\\",\\\"")[[1]][1]
  }
}

write.table(game_list,"Games_HowLong_AppID_metadato.txt",quote = F,row.names = F,col.names = F,sep = "\t")

