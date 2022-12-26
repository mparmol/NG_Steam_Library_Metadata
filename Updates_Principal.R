##Updates

### Más info: https://nik-davis.github.io/posts/2019/steam-data-collection/
# https://github.com/nik-davis/steam-data-science-project

#API references:
#https://partner.steamgames.com/doc/webapi
#https://wiki.teamfortress.com/wiki/User:RJackson/StorefrontAPI
#https://steamapi.xpaw.me/#
#https://steamspy.com/api.php

#DataScraping with R: https://remiller1450.github.io/s230s19/Intro_to_Web_Scraping.html

### Extract game list, from windows

require("data.table")
require("stringr")
require("rvest")
require("RCurl")
require("readr")

isEmpty <- function(x) { #This function checks if a data frame is empty or not
  return(length(x)==0)
}

game_list<-read.delim("Games_HowLong.txt")

#### Más metadato

### Pillar AppID

### Más info: https://partner.steamgames.com/doc/webapi/ISteamApps#GetAppList

jejeje<-NULL

#if(!file.exists("TODO.txt"))
#{
  #AppID_List <- html_text(html_node(read_html("https://api.steampowered.com/ISteamApps/GetAppList/v2/"),"p"))
  #for(i in 1:10)
  #{
    #AppID_List <- getURL("https://api.steampowered.com/ISteamApps/GetAppList/v2/")

    system("rm -rf index.html")
    system("wget https://api.steampowered.com/ISteamApps/GetAppList/v2/")
    
    AppID_List <- read_file("index.html")

    hola<-gsub("\\\"\\},\\{\\\"appid\\\":","``",AppID_List)
    hola2<-gsub(",\"name\":\"","``",hola)
    hola3<-gsub("\\{\\\"applist\\\":\\{\\\"apps\\\":\\[\\{\\\"appid\\\":","",hola2)
    hola4<-gsub("\\\"\\}\\]\\}\\}","",hola3)


    uooo<-data.frame(matrix(strsplit(hola4,"\\`\\`")[[1]],ncol=2,byrow=T))
    res_games<-uooo
    jejeje<-rbind(jejeje,res_games)
    print(dim(res_games))
  #}

  res_games<-jejeje[!duplicated(jejeje),]


  #res_games<-data.frame(matrix(ncol=2,nrow=dim(uooo)[1]))

  #for(i in 2:3500)
  ##for(i in 2:(str_count(AppID_List,'"name"')[1]+1))
  ##{
    ##res_games[i,1]<-strsplit(strsplit(strsplit(sapply(strsplit(AppID_List, '\\{'), "[[", i),"\\}")[[1]][1],":")[[1]][2],",")[[1]][1]
    ##res_games[i,2]<-strsplit(strsplit(strsplit(sapply(strsplit(AppID_List, '\\{'), "[[", i),"\\}")[[1]][1],":\\\"")[[1]][2],'\\\"')[[1]][1]
  ##}

  write.table(res_games,"TODO.txt",quote = F,row.names = F,col.names = F,sep="\t")
#}else 
#{
  #res_games<-read.delim("TODO.txt",sep="\t",header=F)
#}


game_list$AppID<-res_games[match(game_list[,7],res_games[,2]),1]
game_list$AppID_name<-res_games[match(game_list[,7],res_games[,2]),2]
#game_list$AppID_2<-res_games[match(game_list[,2],res_games[,2]),1]
#game_list$AppID_name_2<-res_games[match(game_list[,2],res_games[,2]),2]

write.table(game_list,"Games_HowLong_AppID.txt",quote = F,row.names = F,col.names = F,sep = "\t")

#library(rvest)
#my_df <- as.data.frame(read_html(index.html) %>% html_table(fill=TRUE))

### Datos del juego desde AppID

### Más info: https://steamspy.com/api.php

### Probar el paquete https://pypi.org/project/steamspypi/


for(i in 1:dim(game_list)[1])
{
  if(!is.na(game_list[i,8]))
  {
    meta_juego<-getURL(paste("https://steamspy.com/api.php?request=appdetails&appid=",game_list[i,8],sep=""))
    game_list[i,10]<-strsplit(strsplit(meta_juego,"genre\\\":\\\"")[[1]][2],"\\\",\\\"")[[1]][1]
    game_list[i,11]<-strsplit(strsplit(meta_juego,"positive\\\":")[[1]][2],",\\\"")[[1]][1]
    game_list[i,12]<-strsplit(strsplit(meta_juego,"negative\\\":")[[1]][2],",\\\"")[[1]][1]
    game_list[i,13]<-strsplit(strsplit(meta_juego,"developer\\\":\\\"")[[1]][2],"\\\",\\\"")[[1]][1]
    game_list[i,14]<-strsplit(strsplit(meta_juego,"publisher\\\":\\\"")[[1]][2],"\\\",\\\"")[[1]][1]
  }

  write.table(game_list,"Games_HowLong_AppID_metadato.txt",quote = F,row.names = F,col.names = F,sep = "\t")

}


