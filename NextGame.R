##NextGame

### Extract game list, from windows

library(data.table)
library(stringr)

isEmpty <- function(x) { #This function checks if a data frame is empty or not
  return(length(x)==0)
}

if(!file.exists("Games.txt"))
{
  system("rm -rf index.html?tab=all")
  system("wget https://steamcommunity.com/id/marko_pakete/games/?tab=all")

  file_process<-as.data.frame(fread("index.html?tab=all",fill = T))
  h<-file_process[grep("rgGames",file_process[,1]),]

  res_games<-data.frame(matrix(nrow=str_count(h,"name")[1]))

  for(i in 2:(str_count(h,"name")[1]+1))
  {
    res_games[i,1]<-substr(strsplit(sapply(strsplit(h[1], "name"), "[[", i),",")[[1]][1],4,nchar(strsplit(sapply(strsplit(h[1], "name"), "[[", i),",")[[1]][1])-1)
  }

  write.table(res_games,"Games.txt",quote = F,row.names = F,col.names = F)
}

#https://github.com/Depressurizer/Depressurizer/releases
#https://github.com/Twombs/Steam-Games-List
#https://www.kaggle.com/datasets/ca06934a676693b069bc319eb7c76647afbcf8019dc4828cabb50678a064f3ff BAJAR METADATO
#https://github.com/Duerkos/steam_analysis BAJAR METADATA

### Extract gameplay time 

### NO COGE EL QUE DEBE, MIRAR BIEN QUE COJA EL NOMBRE PERFECTO

game_list<-read.delim("Games.txt")

###Limpiar nombre, chequeo de estado.

for(i in 1:dim(game_list)[1])
{
  aux_game_name<-gsub(" ","",game_list[i,1])

  if(!isEmpty(which(game_list[,1]==aux_game_name)) & length(strsplit(game_list[i,1], " ")[[1]])>1)
  {
    aa<-which(game_list[,1]==aux_game_name)
    game_list<-as.data.frame(game_list[-aa,])
  }else if(!isEmpty(which(game_list[,1]==aux_game_name)) & length(which(game_list[,1]==aux_game_name))>1)
  {
    aa<-which(game_list[,1]==aux_game_name)
    game_list<-as.data.frame(game_list[-aa[2:length(aa)],])
  }

  if(grepl("\\\\u[a-zA-Z0-9]{4}",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("\\\\u[a-zA-Z0-9]{4}","",game_list[i,1])
  }
}




###

game_list[,2]<-NA
game_list[,3]<-NA
game_list[,4]<-NA

cont=0
i=1

#for(i in 1:dim(game_list)[1])
while(i<dim(game_list)[1])
{
  if(is.na(game_list[i,2]))
  {
    pasted_value=paste(strsplit(game_list[i,1],split = " ")[[1]],collapse =  "_")
    
    if(grepl("&",pasted_value)) #Este bloque de modificaciones sirven para encontrar ya que con el nombre exacto no salen resultados..problemas con símbolos ## NO VA BIEN!!!   HACER: & $ . -
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

    if(grepl("\\|",pasted_value))
    {
      pasted_value=gsub("\\|","_",pasted_value)
    }

    system(paste("node New.js ",pasted_value," > aux_time.txt", sep=""))
    
    if(file.info("aux_time.txt")$size>0)
    {
      data_time<-read.delim("aux_time.txt")

       #### cambios fuertes


      if(dim(data_time)[1]>0)
      {
        data_time<-paste(data_time)

        name_list_j<-NULL
        name_list_j_gpm<-NULL
        name_list_j_gpc<-NULL
        name_list_j_simil<-NULL

        for(u in 2:(str_count(data_time,"  name: ")[1]+1))
        {

          if(grepl(pattern = "\\',",sapply(strsplit(sapply(strsplit(data_time, "  name: "), "[[", u),"\\\""), "[[",1)))
          {
            name_list_j<-c(name_list_j, sapply(strsplit(sapply(strsplit(sapply(strsplit(data_time, "  name: "), "[[", u),"\\,\\\""), "[[",1),"\\'"), "[[",2))
          }else
          {
            name_list_j<-c(name_list_j, sapply(strsplit(sapply(strsplit(data_time, "  name: "), "[[", u),"\\,\\\""), "[[",1))

          }
          
          name_list_j_gpm<-c(name_list_j_gpm, sapply(strsplit(sapply(strsplit(data_time, "gameplayMain: "), "[[", u),","), "[[",1))
          name_list_j_gpc<-c(name_list_j_gpc, sapply(strsplit(sapply(strsplit(data_time, "gameplayCompletionist: "), "[[", u),","), "[[",1))
          name_list_j_simil<-c(name_list_j_simil, sapply(strsplit(sapply(strsplit(data_time, "similarity: "), "[[", u),","), "[[",1))
        }

        name_list_j_simil<-as.numeric(name_list_j_simil)
      
        name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]
        name_list_j_gpm[which(max(name_list_j_simil)==name_list_j_simil)][1]
        name_list_j_gpc[which(max(name_list_j_simil)==name_list_j_simil)][1]

        #### Por aquí seguía
      
        #if(dim(data_time)[1]>0 & (grepl(paste("'",game_list[i,1],"'",sep=""),data_time,fixed=TRUE) | grepl(str_to_title(paste("'",game_list[i,1],"'",sep="")),data_time,fixed=TRUE))) # Busca el nombre exacto, si no se sale de la búsqueda. Si no encuentra el nombre exacto en la lista, convierte todo a minúscula menos la primera letra
        if((grepl(paste("'",game_list[i,1],"'",sep=""),data_time,fixed=TRUE) | grepl(str_to_title(paste("'",game_list[i,1],"'",sep="")),data_time,fixed=TRUE))) # Busca el nombre exacto, si no se sale de la búsqueda. Si no encuentra el nombre exacto en la lista, convierte todo a minúscula menos la primera letra
        {
          #if(gsub(",","",strsplit(grep("gameplayMain:",data_time[,1],value=T), "Main: ")[[1]][2])>0)
          if(name_list_j_gpm[which(max(name_list_j_simil)==name_list_j_simil)][1]>0)
          {
            #print(paste(game_list[i,1],": ",gsub(",","",strsplit(grep("gameplayMain:",data_time[,1],value=T), "Main: ")[[1]][2]),"h"," ",gsub(",","",strsplit(grep("gameplayCompletionist:",data_time[,1],value=T), "Completionist: ")[[1]][2]),"h"," ",gsub("\\'","",gsub(",","",strsplit(grep("name:",data_time[,1],value=T), "name: ")[[1]][2])),sep = ""))
            #game_list[i,2]<-gsub(",","",strsplit(grep("gameplayMain:",data_time[,1],value=T), "Main: ")[[1]][2])
            #game_list[i,3]<-gsub(",","",strsplit(grep("gameplayCompletionist:",data_time[,1],value=T), "Completionist: ")[[1]][2])
            #game_list[i,4]<-gsub("\\'","",gsub(",","",strsplit(grep("name:",data_time[,1],value=T), "name: ")[[1]][2]))
            print(paste(game_list[i,1],": ",name_list_j_gpm[which(max(name_list_j_simil)==name_list_j_simil)][1],"h"," ",name_list_j_gpc[which(max(name_list_j_simil)==name_list_j_simil)][1],"h"," ",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1],sep = ""))
            game_list[i,2]<-name_list_j_gpm[which(max(name_list_j_simil)==name_list_j_simil)][1]
            game_list[i,3]<-name_list_j_gpc[which(max(name_list_j_simil)==name_list_j_simil)][1]
            game_list[i,4]<-name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]
            game_list[i,5]<-"Exact"
          #}else if(gsub(",","",strsplit(grep("gameplayCompletionist:",data_time[,1],value=T), "Completionist: ")[[1]][2])>0)
          }else if(name_list_j_gpc[which(max(name_list_j_simil)==name_list_j_simil)][1]>0)
          {
            #print(paste(game_list[i,1],": ",gsub(",","",strsplit(grep("gameplayMain:",data_time[,1],value=T), "Main: ")[[1]][2]),"h"," ",gsub(",","",strsplit(grep("gameplayCompletionist:",data_time[,1],value=T), "Completionist: ")[[1]][2]),"h"," ",gsub("\\'","",gsub(",","",strsplit(grep("name:",data_time[,1],value=T), "name: ")[[1]][2])),sep = ""))
            #game_list[i,2]<-gsub(",","",strsplit(grep("gameplayMain:",data_time[,1],value=T), "Main: ")[[1]][2])
            #game_list[i,3]<-gsub(",","",strsplit(grep("gameplayCompletionist:",data_time[,1],value=T), "Completionist: ")[[1]][2])
            #game_list[i,4]<-gsub("\\'","",gsub(",","",strsplit(grep("name:",data_time[,1],value=T), "name: ")[[1]][2]))
            print(paste(game_list[i,1],": ",name_list_j_gpm[which(max(name_list_j_simil)==name_list_j_simil)][1],"h"," ",name_list_j_gpc[which(max(name_list_j_simil)==name_list_j_simil)][1],"h"," ",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1],sep = ""))
            game_list[i,2]<-name_list_j_gpm[which(max(name_list_j_simil)==name_list_j_simil)][1]
            game_list[i,3]<-name_list_j_gpc[which(max(name_list_j_simil)==name_list_j_simil)][1]
            game_list[i,4]<-name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]
            game_list[i,5]<-"Exact"
          }else
          {
            print(paste(game_list[i,1],": Sin registro de tiempo",sep = ""))
            game_list[i,2]<-"Sin registro de tiempo"
            game_list[i,3]<-"Sin registro de tiempo"
          }
        #}else if(dim(data_time)[1]>0)
        }else
        {
            #print(paste(game_list[i,1],": ",gsub(",","",strsplit(grep("gameplayMain:",data_time[,1],value=T), "Main: ")[[1]][2]),"h"," ",gsub(",","",strsplit(grep("gameplayCompletionist:",data_time[,1],value=T), "Completionist: ")[[1]][2]),"h"," ",gsub("\\'","",gsub(",","",strsplit(grep("name:",data_time[,1],value=T), "name: ")[[1]][2])),sep = ""))
            #game_list[i,2]<-gsub(",","",strsplit(grep("gameplayMain:",data_time[,1],value=T), "Main: ")[[1]][2])
            #game_list[i,3]<-gsub(",","",strsplit(grep("gameplayCompletionist:",data_time[,1],value=T), "Completionist: ")[[1]][2])
            #game_list[i,4]<-gsub("\\'","",gsub(",","",strsplit(grep("name:",data_time[,1],value=T), "name: ")[[1]][2]))
            print(paste(game_list[i,1],": ",name_list_j_gpm[which(max(name_list_j_simil)==name_list_j_simil)][1],"h"," ",name_list_j_gpc[which(max(name_list_j_simil)==name_list_j_simil)][1],"h"," ",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1],sep = ""))
            game_list[i,2]<-name_list_j_gpm[which(max(name_list_j_simil)==name_list_j_simil)][1]
            game_list[i,3]<-name_list_j_gpc[which(max(name_list_j_simil)==name_list_j_simil)][1]
            game_list[i,4]<-name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]
            game_list[i,5]<-"Non Exact"
        }
      }else
      {

        #for(o in 1:length())
        #{

        #  system(paste("node New.js ",pasted_value," > aux_time.txt", sep=""))
        #}

        print(paste(game_list[i,1],": NA",sep = ""))
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
      print("Descansando buffer 2 min")
      Sys.sleep(120)
      i=i-1
    }

    i=i+1
  }
}

write.table(game_list,"Games_HowLong.txt",quote = F,row.names = F,col.names = F,sep = "\t")
