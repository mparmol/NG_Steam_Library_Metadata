##NextGame

### Extract game list, from windows

library(data.table)
library(stringr)

system("wget https://steamcommunity.com/id/marko_pakete/games/?tab=all")

file_process<-as.data.frame(fread("index.html?tab=all",fill = T))
h<-file_process[grep("rgGames",file_process[,1]),]

res_games<-data.frame(matrix(nrow=str_count(h,"name")[1]))

for(i in 2:(str_count(h,"name")[1]+1))
{
  res_games[i,1]<-substr(strsplit(sapply(strsplit(h[1], "name"), "[[", i),",")[[1]][1],4,nchar(strsplit(sapply(strsplit(h[1], "name"), "[[", i),",")[[1]][1])-1)
}

write.table(res_games,"Games.txt",quote = F,row.names = F,col.names = F)

#https://github.com/Depressurizer/Depressurizer/releases




### Extract gameplay time 

### NO COGE EL QUE DEBE, MIRAR BIEN QUE COJA EL NOMBRE PERFECTO

game_list<-read.delim("Games.txt")



for(i in 1:dim(game_list)[1])
{
  if(is.na(game_list[i,2]))
  {
    pasted_value=paste(strsplit(game_list[i,1],split = " ")[[1]],collapse =  "_")
    
    if(grepl("&",pasted_value))
    {
      pasted_value=strsplit(pasted_value,"&_")[[1]][2]
    }
    
    if(grepl("\\(",pasted_value))
    {
      pasted_value=strsplit(pasted_value,"_\\(")[[1]][1]
    }
    
    if(grepl("\\'",pasted_value))
    {
      pasted_value=gsub("\\'","_",pasted_value)
    }
    
    system(paste("node New.js ",pasted_value," > aux_time.txt", sep=""))
    
    data_time<-read.delim("aux_time.txt")
    
    if(dim(data_time)[1]>0)
    {
      if(gsub(",","",strsplit(grep("gameplayMain:",data_time[,1],value=T), "Main: ")[[1]][2])>0)
      {
        print(paste(game_list[i,1],": ",gsub(",","",strsplit(grep("gameplayMain:",data_time[,1],value=T), "Main: ")[[1]][2]),"h",sep = ""))
        game_list[i,2]<-gsub(",","",strsplit(grep("gameplayMain:",data_time[,1],value=T), "Main: ")[[1]][2])
      }else
      {
        print(paste(game_list[i,1],": ","Sin registro de tiempo",sep = ""))
        game_list[i,2]<-"Sin registro de tiempo"
      }
    }else
    {
      print(paste(game_list[i,1],": NA",sep = ""))
      game_list[i,2]<-"NA"
    }
  }
}

write.table(game_list,"Games_HowLong.txt",quote = F,row.names = F,col.names = F,sep = "\t")