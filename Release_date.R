##NextGame

### Extract game list, from windows

require("data.table")
require("stringr")
require("stringi")
require("rvest")
require("RCurl")
require("readr")

isEmpty <- function(x) { #This function checks if a data frame is empty or not
  return(length(x)==0)
}


cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

game_list<-read.delim("Games_HowLong_AppID_metadato.txt",header=F)

for(i in 1:dim(game_list)[1])
{
  if(!is.na(game_list[i,8]))
  {
    meta_juego<-getURL(paste("https://store.steampowered.com/api/appdetails/?cc=EU&appids=",game_list[i,8],sep=""))
    
    if(grepl("\"success\"\\:true",meta_juego))
    {
      app_id_gen<-strsplit(strsplit(meta_juego,"steam_appid\\\"\\:")[[1]][2],"\\,")[[1]][1]

      game_list[match(app_id_gen,game_list[,8]),17]<-strsplit(strsplit(strsplit(meta_juego,"release_date")[[1]][2],"\\\"}")[[1]][1],"\\:\\\"")[[1]][2]
      game_list[match(app_id_gen,game_list[,8]),18]<-gsub("Minimum:","",strsplit(gsub("\",\"recommended\":\""," ",cleanFun(strsplit(strsplit(meta_juego,"pc_requirements\\\"\\:\\{\\\"minimum\\\":\\\"")[[1]][2],"\"},\\\"mac_requirements")[[1]][1])),"Recommended:")[[1]][1])
      game_list[match(app_id_gen,game_list[,8]),19]<-strsplit(gsub("\",\"recommended\":\""," ",cleanFun(strsplit(strsplit(meta_juego,"pc_requirements\\\"\\:\\{\\\"minimum\\\":\\\"")[[1]][2],"\"},\\\"mac_requirements")[[1]][1])),"Recommended:")[[1]][2]
    }

    Sys.sleep(3)
  }

  write.table(game_list,"Games_HowLong_AppID_metadato_DATAA.txt",quote = F,row.names = F,col.names = F,sep = "\t")

}