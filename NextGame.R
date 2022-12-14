##NextGame

### Extract game list, from windows

require("data.table")
require("stringr")
#library(data.table)
#library(stringr)

isEmpty <- function(x) { #This function checks if a data frame is empty or not
  return(length(x)==0)
}

if(!file.exists("Games.txt"))
{
  system("rm -rf index.html?tab=all")
  system("wget https://steamcommunity.com/id/marko_pakete/games/?tab=all")
  #system("wget https://steamcommunity.com/id/guayabazo/games/?tab=all")

  # André: https://steamcommunity.com/profiles/76561198012006378/games/?tab=all
  # Jolas: https://steamcommunity.com/id/guayabazo/games/?tab=all
  # Álvaro: https://steamcommunity.com/profiles/76561197992225029/games/?tab=all
  # Jimmy: https://steamcommunity.com/profiles/76561198124010932/games/?tab=all
  # Álvaro: https://steamcommunity.com/id/DuckSaucer77/games/?tab=all

  file_process<-as.data.frame(fread("index.html?tab=all",fill = T))
  h<-file_process[grep("rgGames",file_process[,1]),]

  res_games<-data.frame(matrix(nrow=str_count(h,'"name"')[1]))

  for(i in 2:(str_count(h,'"name"')[1]+1))
  {
    res_games[i,1]<-substr(strsplit(sapply(strsplit(h[1], '"name"'), "[[", i),",\\\"")[[1]][1],3,nchar(strsplit(sapply(strsplit(h[1], '"name"'), "[[", i),",\\\"")[[1]][1])-1)
  }

  write.table(res_games,"Games.txt",quote = F,row.names = F,col.names = F)
}

#https://github.com/Depressurizer/Depressurizer/releases
#https://github.com/Twombs/Steam-Games-List
#https://www.kaggle.com/datasets/ca06934a676693b069bc319eb7c76647afbcf8019dc4828cabb50678a064f3ff BAJAR METADATO
#https://github.com/Duerkos/steam_analysis BAJAR METADATA
#https://github.com/scjustice/steam_webscraper EL SCRAPPER PRO

### Extract gameplay time

game_list<-read.delim("Games.txt")
game_list_aux<-game_list
###Limpiar nombre, chequeo de estado.

for(i in 1:dim(game_list)[1]) ## Muy bien todo esto, pero mantener el nombre original para llevarlo a tabla final...
{
  aux_game_name<-gsub(" ","",game_list[i,1])

  if(!isEmpty(which(game_list[,1]==aux_game_name)) & length(strsplit(game_list[i,1], " ")[[1]])>1)
  {
    aa<-which(game_list[,1]==aux_game_name)
    game_list<-as.data.frame(game_list[-aa,])
    game_list_aux<-as.data.frame(game_list_aux[-aa,])
  }else if(!isEmpty(which(game_list[,1]==aux_game_name)) & length(which(game_list[,1]==aux_game_name))>1)
  {
    aa<-which(game_list[,1]==aux_game_name)
    game_list<-as.data.frame(game_list[-aa[2:length(aa)],])
    game_list_aux<-as.data.frame(game_list_aux[-aa[2:length(aa)],])
  }

  if(grepl("\\\\u00fc",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("\\\\u00fc","ü",game_list[i,1])
    game_list_aux[i,1]=gsub("\\\\u00fc","ü",game_list_aux[i,1])
  }

  if(grepl("\\\\u[a-zA-Z0-9]{4}",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("\\\\u[a-zA-Z0-9]{4}","",game_list[i,1])
    game_list_aux[i,1]=gsub("\\\\u[a-zA-Z0-9]{4}","",game_list_aux[i,1])
  }

  if(grepl("Transformed Collection$",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("Collection","",game_list[i,1])
  }

  if(grepl("Collector's Edition$",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("Collector's Edition","",game_list[i,1])
  }

  if(grepl("Ultimate Edition$",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("Ultimate Edition","",game_list[i,1])
  }

  if(grepl("Extended Edition$",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("Extended Edition","",game_list[i,1])
  }

  if(grepl("Gold Edition$",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("Gold Edition","",game_list[i,1])
  }

  if(grepl("Premium Edition$",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("Premium Edition","",game_list[i,1])
  }

  if(grepl("Deluxe Edition$",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("Deluxe Edition","",game_list[i,1])
  }

  if(grepl("Special Editions$",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("Special Editions","",game_list[i,1])
  }

  if(grepl("Maximum Edition$",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("Maximum Edition","",game_list[i,1])
  }

  #if(grepl("Reloaded$",game_list[i,1])) #######################
  #{
  #  game_list[i,1]=gsub("Reloaded","",game_list[i,1])
  #}

  if(grepl("Remastered$",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("Remastered","",game_list[i,1])
  }

  if(grepl("Remastered Edition$",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("Remastered Edition","",game_list[i,1])
  }

  if(grepl("The Visual Novel$",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("The Visual Novel","",game_list[i,1])
  }

  if(grepl("Mega Drive",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("Mega Drive","",game_list[i,1])
  }

  #if(grepl("Enhanced Edition$",game_list[i,1])) #######################
  #{
  #  game_list[i,1]=gsub("Enhanced Edition","",game_list[i,1])
  #}

  #if(grepl("Definitive Edition$",game_list[i,1])) #######################
  #{
  #  game_list[i,1]=gsub("Definitive Edition","",game_list[i,1])
  #}

  if(grepl("Complete$",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub(" Complete","",game_list[i,1])
  }

  if(grepl("Steam Edition$",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("Steam Edition","",game_list[i,1])
  }

  if(grepl("- Anniversary Edition$",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("- Anniversary Edition","",game_list[i,1])
  }

  if(grepl("ARCADE GAME SERIES: ",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("ARCADE GAME SERIES: ","",game_list[i,1])
  }

  if(grepl(" and ",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub(" and "," ",game_list[i,1])
  }

  #if(grepl(":",game_list[i,1])) #######################
  #{
  #  game_list[i,1]=gsub(":","",game_list[i,1])
  #}

  if(grepl(" & ",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub("&","",game_list[i,1])
  }
  
  if(grepl(" - ",game_list[i,1])) #######################
  {
    game_list[i,1]=gsub(" - "," ",game_list[i,1])
  }

  if(grepl("\\'",game_list[i,1]))
  {
    game_list[i,1]=gsub("\\'","_",game_list[i,1])
  }

  if(grepl("\\|",game_list[i,1]))
  {
    game_list[i,1]=gsub("\\|","_",game_list[i,1])
  }

  if(grepl("\\.",game_list[i,1]))
  {
    game_list[i,1]=gsub("\\.","_",game_list[i,1])
  }

  if(grepl("\\)$",game_list[i,1]))
  {
    game_list[i,1]=strsplit(game_list[i,1],"\\(")[[1]][1]
  }

  if(grepl("\\]$",game_list[i,1]))
  {
    game_list[i,1]=strsplit(game_list[i,1],"\\[")[[1]][1]
  }

  if(grepl("^\\[",game_list[i,1]))
  {
    game_list[i,1]=strsplit(game_list[i,1],"\\]")[[1]][2]
  }

}

game_list[i,1]<-NA
game_list_aux[i,1]<-NA

###

game_list[,2]<-NA
game_list[,3]<-NA
game_list[,4]<-NA

cont=0
i=1

while(i<dim(game_list)[1])
{
  if(is.na(game_list[i,2]))
  {
    pasted_value=paste("'",game_list[i,1],"'",sep="")

    band_f=0
    cont_long_string=0
    valor_unico=0
    
    while(band_f==0 & cont_long_string<length(strsplit(game_list[i,1], " ")[[1]])+7)
    {
      cont_long_string=cont_long_string+1

      system(paste("node New.js ",pasted_value," > aux_time.txt", sep=""))
    
      if(file.info("aux_time.txt")$size>0)
      {
        data_time<-read.delim("aux_time.txt")

        if(valor_unico==1 & length(data_time[grepl(" name:",data_time[,1]),])==1)
        {
          valor_unico=0
        }

        if(dim(data_time)[1]>0 & valor_unico==0)
        {
          band_f=1

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
        
          #if((grepl(paste("'",game_list[i,1],"'",sep=""),data_time,fixed=TRUE) | grepl(str_to_title(paste("'",game_list[i,1],"'",sep="")),data_time,fixed=TRUE))) # Busca el nombre exacto, si no se sale de la búsqueda. Si no encuentra el nombre exacto en la lista, convierte todo a minúscula menos la primera letra
          if(max(name_list_j_simil)>0.925)
          {
            if(name_list_j_gpm[which(max(name_list_j_simil)==name_list_j_simil)][1]>0)
            {
              print(paste(game_list_aux[i,1],": ",name_list_j_gpm[which(max(name_list_j_simil)==name_list_j_simil)][1],"h"," ",name_list_j_gpc[which(max(name_list_j_simil)==name_list_j_simil)][1],"h"," ",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1],sep = ""))
              game_list[i,2]<-name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]
              game_list[i,3]<-name_list_j_gpm[which(max(name_list_j_simil)==name_list_j_simil)][1]
              game_list[i,4]<-name_list_j_gpc[which(max(name_list_j_simil)==name_list_j_simil)][1]
              game_list[i,5]<-max(name_list_j_simil)
              game_list[i,6]<-"Exact"
            }else if(name_list_j_gpc[which(max(name_list_j_simil)==name_list_j_simil)][1]>0)
            {
              print(paste(game_list_aux[i,1],": ",name_list_j_gpm[which(max(name_list_j_simil)==name_list_j_simil)][1],"h"," ",name_list_j_gpc[which(max(name_list_j_simil)==name_list_j_simil)][1],"h"," ",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1],sep = ""))
              game_list[i,2]<-name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]
              game_list[i,3]<-name_list_j_gpm[which(max(name_list_j_simil)==name_list_j_simil)][1]
              game_list[i,4]<-name_list_j_gpc[which(max(name_list_j_simil)==name_list_j_simil)][1]
              game_list[i,5]<-max(name_list_j_simil)
              game_list[i,6]<-"Exact"
            }else
            {
              print(paste(game_list_aux[i,1],": Sin registro de tiempo"," ",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1],sep = ""))
              game_list[i,2]<-name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]
              game_list[i,3]<-"Sin registro de tiempo"
              game_list[i,4]<-"Sin registro de tiempo"
              game_list[i,5]<-max(name_list_j_simil)
              game_list[i,6]<-"Exact"
            }
          }else
          {
            if(name_list_j_gpm[which(max(name_list_j_simil)==name_list_j_simil)][1]>0)
            {
              print(paste(game_list_aux[i,1],": ",name_list_j_gpm[which(max(name_list_j_simil)==name_list_j_simil)][1],"h"," ",name_list_j_gpc[which(max(name_list_j_simil)==name_list_j_simil)][1],"h"," ",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1],sep = ""))
              game_list[i,2]<-name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]
              game_list[i,3]<-name_list_j_gpm[which(max(name_list_j_simil)==name_list_j_simil)][1]
              game_list[i,4]<-name_list_j_gpc[which(max(name_list_j_simil)==name_list_j_simil)][1]
              game_list[i,5]<-max(name_list_j_simil)
              game_list[i,6]<-"Non Exact"
            }else if(name_list_j_gpc[which(max(name_list_j_simil)==name_list_j_simil)][1]>0)
            {
              print(paste(game_list_aux[i,1],": ",name_list_j_gpm[which(max(name_list_j_simil)==name_list_j_simil)][1],"h"," ",name_list_j_gpc[which(max(name_list_j_simil)==name_list_j_simil)][1],"h"," ",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1],sep = ""))
              game_list[i,2]<-name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]
              game_list[i,3]<-name_list_j_gpm[which(max(name_list_j_simil)==name_list_j_simil)][1]
              game_list[i,4]<-name_list_j_gpc[which(max(name_list_j_simil)==name_list_j_simil)][1]
              game_list[i,5]<-max(name_list_j_simil)
              game_list[i,6]<-"Non Exact"
            }else
            {
              print(paste(game_list_aux[i,1],": Sin registro de tiempo"," ",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1],sep = ""))
              game_list[i,2]<-name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]
              game_list[i,3]<-"Sin registro de tiempo"
              game_list[i,4]<-"Sin registro de tiempo"
              game_list[i,5]<-max(name_list_j_simil)
              game_list[i,6]<-"Non Exact"
            }
          }
        }else
        {
          if(cont_long_string<length(strsplit(game_list[i,1], " ")[[1]]))
          {
            pasted_value<-NULL

            for(y in 1:length(strsplit(game_list[i,1], " ")[[1]]))
            {
              if(y==1)
              {
                pasted_value<-paste("'",strsplit(game_list[i,1], " ")[[1]][1],sep="")
              }else if(cont_long_string==y)
              {
                pasted_value<-paste(pasted_value,": ",strsplit(game_list[i,1], " ")[[1]][y],sep="")
              }else
              {
                pasted_value<-paste(pasted_value," ",strsplit(game_list[i,1], " ")[[1]][y],sep="")
              }
            }

            pasted_value=paste(pasted_value,"'",sep="")
          }else if(grepl("^Disney ",game_list[i,1])) #######################
          {
            pasted_value=paste("'",gsub("Disney ","",game_list[i,1]),"'",sep="")
          }else if(grepl("Enhanced Edition$",game_list[i,1])) #######################
          {
            pasted_value=paste("'",gsub("Enhanced Edition","",game_list[i,1]),"'",sep="")
          }else if(grepl("Definitive Edition$",game_list[i,1])) #######################
          {
            pasted_value=paste("'",gsub("Definitive Edition","",game_list[i,1]),"'",sep="")
          }else if(grepl("Edition$",game_list[i,1])) #######################
          {
            pasted_value=paste("'",gsub("Edition","",game_list[i,1]),"'",sep="")
          }else if(grepl("Game of the Year Edition$",game_list[i,1])) #######################
          {
            pasted_value=paste("'",gsub("Game of the Year Edition","",game_list[i,1]),"'",sep="")
          }else if(grepl(":",game_list[i,1])) #######################
          {
            pasted_value=paste("'",gsub(":","",game_list[i,1]),"'",sep="")
          }else
          {
            pasted_value<-strsplit(game_list[i,1], " ")[[1]][1]
            valor_unico<-1
          } 
        }
      }else
      {
        print("Descansando buffer 1 min")
        Sys.sleep(60)
        cont_long_string=cont_long_string-1  ###MIRAR
      }
    }

    if(band_f==0)
    {
      print(paste(game_list[i,1],": NA",sep = ""))
      #game_list[i,2]<-"NA"
      #game_list[i,3]<-"NA"
    }

    i=i+1
  }
}

game_list[,1]<-game_list_aux[,1]
write.table(game_list,"Games_HowLong.txt",quote = F,row.names = F,col.names = F,sep = "\t")
