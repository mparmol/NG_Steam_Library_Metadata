##NextGame

### Extract game list, from windows

https://github.com/Depressurizer/Depressurizer/releases

### Extract gameplay time

game_list<-read.delim("Games.txt")

for(i in 1:dim(game_list)[1])
{
  pasted_value=paste(strsplit(game_list[i,],split = " ")[[1]],collapse =  "_")
  
  system(paste("node New.js ",pasted_value," > aux_time.txt", sep=""))
  
  data_time<-read.delim("aux_time.txt")
  
  if(dim(data_time)[1]>0)
  {
    if(gsub(",","",strsplit(grep("gameplayMain:",data_time[,1],value=T), "Main: ")[[1]][2])>0)
    {
      print(paste(game_list[i,],": ",gsub(",","",strsplit(grep("gameplayMain:",data_time[,1],value=T), "Main: ")[[1]][2]),"h",sep = ""))
      
    }else
    {
      print(paste(game_list[i,],": ","Sin registro de tiempo",sep = ""))
      
    }
  }else
  {
    print(paste(game_list[i,],": NA",sep = "")) 
  }
}

