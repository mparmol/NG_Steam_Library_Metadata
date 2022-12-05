##NextGame

### Extract game list, from windows

library(data.table)
library(stringr)

#########system("rm -rf index.html?tab=all")
#########system("wget https://steamcommunity.com/id/marko_pakete/games/?tab=all")

#########file_process<-as.data.frame(fread("index.html?tab=all",fill = T))
#########h<-file_process[grep("rgGames",file_process[,1]),]

#########res_games<-data.frame(matrix(nrow=str_count(h,"name")[1]))

#########for(i in 2:(str_count(h,"name")[1]+1))
#########{
  #########res_games[i,1]<-substr(strsplit(sapply(strsplit(h[1], "name"), "[[", i),",")[[1]][1],4,nchar(strsplit(sapply(strsplit(h[1], "name"), "[[", i),",")[[1]][1])-1)
#########}

#########write.table(res_games,"Games.txt",quote = F,row.names = F,col.names = F)

#https://github.com/Depressurizer/Depressurizer/releases
#https://github.com/Twombs/Steam-Games-List


### Extract gameplay time 

### NO COGE EL QUE DEBE, MIRAR BIEN QUE COJA EL NOMBRE PERFECTO

game_list<-read.delim("Games.txt")

###Limpiar nombre, chequeo de estado.

for(i in 1:dim(game_list)[1])
{
    if(grepl("\\u2122",game_list[i,1],fixed=TRUE))
    {
      game_list[i,1]=gsub("\\u2122","",game_list[i,1],fixed=TRUE)
    }
    
    if(grepl("\\u00fc",game_list[i,1],fixed=TRUE))
    {
      game_list[i,1]=gsub("\\u00fc","ü",game_list[i,1],fixed=TRUE)
    }

    if(grepl("\\u00ae",game_list[i,1],fixed=TRUE))
    {
      game_list[i,1]=gsub("\\u00ae","ü",game_list[i,1],fixed=TRUE)
    }
}




###

game_list[,2]<-NA
game_list[,3]<-NA

cont=0

for(i in 1:dim(game_list)[1])
{
  if(is.na(game_list[i,2]))
  {
    pasted_value=paste(strsplit(game_list[i,1],split = " ")[[1]],collapse =  "_")
    
    if(grepl("&",pasted_value)) #Este bloque de modificaciones sirven para encontrar ya que con el nombre exacto no salen resultados..problemas con símbolos
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
    
    if(file.info("aux_time.txt")$size>0)
    {
      data_time<-read.delim("aux_time.txt")
      
      if(dim(data_time)[1]>0 & (grepl(paste("'",game_list[i,1],"'",sep=""),data_time,fixed=TRUE) | grepl(str_to_title(paste("'",game_list[i,1],"'",sep="")),data_time,fixed=TRUE))) # Busca el nombre exacto, si no se sale de la búsqueda. Si no encuentra el nombre exacto en la lista, convierte todo a minúscula menos la primera letra
      {
        if(gsub(",","",strsplit(grep("gameplayMain:",data_time[,1],value=T), "Main: ")[[1]][2])>0)
        {
          print(paste(game_list[i,1]," ",pasted_value,": ",gsub(",","",strsplit(grep("gameplayMain:",data_time[,1],value=T), "Main: ")[[1]][2]),"h",sep = ""))
          print(paste(game_list[i,1]," ",pasted_value,": ",gsub(",","",strsplit(grep("gameplayCompletionist:",data_time[,1],value=T), "Completionist: ")[[1]][2]),"h",sep = ""))
          game_list[i,2]<-gsub(",","",strsplit(grep("gameplayMain:",data_time[,1],value=T), "Main: ")[[1]][2])
          game_list[i,3]<-gsub(",","",strsplit(grep("gameplayCompletionist:",data_time[,1],value=T), "Completionist: ")[[1]][2])
        }else
        {
          print(paste(game_list[i,1]," ",pasted_value,": ","Sin registro de tiempo",sep = ""))
          game_list[i,2]<-"Sin registro de tiempo"
          game_list[i,3]<-"Sin registro de tiempo"
        }
      }else
      {
        print(paste(game_list[i,1]," ",pasted_value,": NA",sep = ""))
        game_list[i,2]<-"NA"
        game_list[i,3]<-"NA"
      }
      
      #cont=cont+1
      
      #if(cont==20)
      #{
      #  print("Sleep")
      #  Sys.sleep(30)
      #  cont=0
      #}
    }else
    {
      Sys.sleep(120)
      print("Descansando buffer 2 min")
      i=i-2
    }
  }
}

write.table(game_list,"Games_HowLong.txt",quote = F,row.names = F,col.names = F,sep = "\t")
