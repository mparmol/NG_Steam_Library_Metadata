##Steam user Metadata

#This script is useful to create a table with all (or almost) the information from your Steam library using different Steam databases

### Extract game list, from windows

suppressPackageStartupMessages(require("data.table"))
suppressPackageStartupMessages(require("stringr"))
suppressPackageStartupMessages(require("stringi"))
suppressPackageStartupMessages(require("rvest"))
suppressPackageStartupMessages(require("RCurl"))
suppressPackageStartupMessages(require("readr"))
suppressPackageStartupMessages(require("lubridate"))
suppressPackageStartupMessages(require("optparse"))

#options(warn=-1)

option_list = list(
  make_option(c("-i", "--input"), type="character", default=NULL, 
              help="Steam user name or ID", metavar="character")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

if (is.null(opt$input)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (Steam user name or ID).", call.=FALSE)
}

id_search=opt

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

##### Get full user data

user_info<-getURL(paste("https://www.steamidfinder.com/lookup/",id_search,sep=""),.opts=curlOptions(followlocation=TRUE)) #Get user information from steamfinder, get both user name if available and ID

if(grepl("customURL",user_info)) #If we have steam user tag we get the information from it. Otherwise we work from steam id information
{
  steam_link=paste("https://steamcommunity.com/id/",strsplit(strsplit(strsplit(user_info,"customURL")[[1]][2],"\" rel=")[[1]][1],"/")[[1]][length(strsplit(strsplit(strsplit(user_info,"customURL")[[1]][2],"\" rel=")[[1]][1],"/")[[1]])],"/games/?tab=all",sep="")
  steam_link_achiv=paste("https://steamcommunity.com/id/",strsplit(strsplit(strsplit(user_info,"customURL")[[1]][2],"\" rel=")[[1]][1],"/")[[1]][length(strsplit(strsplit(strsplit(user_info,"customURL")[[1]][2],"\" rel=")[[1]][1],"/")[[1]])],"/games/?tab=perfect",sep="")
}else
{
  steam_link=paste("https://steamcommunity.com/profiles/",strsplit(strsplit(strsplit(user_info,"profile<")[[1]][2],"\" rel=\"noopener")[[1]][1],"/")[[1]][length(strsplit(strsplit(strsplit(user_info,"profile<")[[1]][2],"\" rel=\"noopener")[[1]][1],"/")[[1]])],"/games/?tab=all",sep="")
  steam_link_achiv=paste("https://steamcommunity.com/profiles/",strsplit(strsplit(strsplit(user_info,"profile<")[[1]][2],"\" rel=\"noopener")[[1]][1],"/")[[1]][length(strsplit(strsplit(strsplit(user_info,"profile<")[[1]][2],"\" rel=\"noopener")[[1]][1],"/")[[1]])],"/games/?tab=perfect",sep="")
}

info_Steam_removed<-paste("https://steam-tracker.com/scan/",strsplit(strsplit(strsplit(user_info,"profile<")[[1]][2],"\" rel=\"noopener")[[1]][1],"/")[[1]][length(strsplit(strsplit(strsplit(user_info,"profile<")[[1]][2],"\" rel=\"noopener")[[1]][1],"/")[[1]])],sep="")

system(paste("rm -rf ",id_search,sep=""))

#### Metadata retrieving from HowLongToBeat

if(!file.exists("Games_HowLong.txt")) #If output already exists (and later APIs fail later) we skip this step
{
  if(!file.exists("Games_buscar.txt")) 
  {
    ####### Nombre de la tabla, todos los caracteres

    info_Steam<-getURL(steam_link)
    file_process<-as.data.frame(info_Steam)

    h<-file_process[grep("rgGames",file_process[,1]),]

    res_games<-data.frame(matrix(nrow=str_count(h,'"name"')[1]))

    for(i in 2:(str_count(h,'"name"')[1]+1))
    {
      res_games[i,1]<-substr(strsplit(sapply(strsplit(h[1], '"name"'), "[[", i),",\\\"")[[1]][1],3,nchar(strsplit(sapply(strsplit(h[1], '"name"'), "[[", i),",\\\"")[[1]][1])-1)
    }

    game_list_orig<-as.data.frame(res_games)
    
    ####### Para buscar en howlong to beat
    
    system("rm -rf index.html?tab=all")
    system(paste("wget ",steam_link,sep=""))

    file_process<-as.data.frame(fread("index.html?tab=all",fill = T))
    
    h<-file_process[grep("rgGames",file_process[,1]),]

    res_games<-data.frame(matrix(ncol=8,nrow=str_count(h,'"name"')[1]))

    for(i in 2:(str_count(h,'"name"')[1]+1))
    {
      res_games[i,1]<-substr(strsplit(sapply(strsplit(h[1], '"name"'), "[[", i),",\\\"")[[1]][1],3,nchar(strsplit(sapply(strsplit(h[1], '"name"'), "[[", i),",\\\"")[[1]][1])-1)
      res_games[i,7]<-game_list_orig[i,1]
      res_games[i,8]<-strsplit(strsplit(sapply(strsplit(h[1], '\\,\\{'), "[[", i-1),"appid\\\"\\:")[[1]][2],",")[[1]][1]
    }

    game_list<-as.data.frame(res_games[-1,])
    game_list_orig<-game_list_orig[-1,]
    write.table(game_list,"Games_buscar.txt",quote = F,row.names = F,col.names = F,sep="\t")
  }else
  {
    game_list<-read.delim("Games_buscar.txt",header=F,sep="\t")
  }

  ### Extract gameplay time

  game_list_aux<-game_list

  ###Limpiar nombre, chequeo de estado.

  for(i in 1:dim(game_list)[1])
  {
    aux_game_name<-gsub(" ","",game_list[i,1])

    if(grepl("\\\\u00fc",game_list[i,1])) #######################
    {
      game_list[i,1]=gsub("\\\\u00fc","ü",game_list[i,1])
      game_list_aux[i,1]=gsub("\\\\u00fc","ü",game_list_aux[i,1])
    }

    if(grepl("\\\\u00f6",game_list[i,1])) #######################
    {
      game_list[i,1]=gsub("\\\\u00f6","ö",game_list[i,1])
      game_list_aux[i,1]=gsub("\\\\u00f6","ö",game_list_aux[i,1])
    }

    if(grepl("\\\\u00db",game_list[i,1])) #######################
    {
      game_list[i,1]=gsub("\\\\u00db","Û",game_list[i,1])
      game_list_aux[i,1]=gsub("\\\\u00db","Û",game_list_aux[i,1])
    }

    if(grepl("\\\\u00e3",game_list[i,1])) #######################
    {
      game_list[i,1]=gsub("\\\\u00e3","ã",game_list[i,1])
      game_list_aux[i,1]=gsub("\\\\u00e3","ã",game_list_aux[i,1])
    }

    if(grepl("\\\\u[a-zA-Z0-9]{4}",game_list[i,1])) #######################
    {
      game_list[i,1]=gsub("\\\\u[a-zA-Z0-9]{4}","",game_list[i,1])
      game_list_aux[i,1]=gsub("\\\\u[a-zA-Z0-9]{4}","",game_list_aux[i,1])
      
      if(nchar(gsub(" ","",game_list[i,1]))==0)
      {
        game_list[i,1]=game_list[i,7]
      }
    }

    if(grepl("ARCADE GAME SERIES: ",game_list[i,1])) #######################
    {
      game_list[i,1]=gsub("ARCADE GAME SERIES: ","",game_list[i,1])
    }

    if(grepl(" & ",game_list[i,1])) #######################
    {
      game_list[i,1]=gsub("&","",game_list[i,1])
    }

    if(grepl("\\'",game_list[i,1]))
    {
      game_list[i,1]=gsub("\\'","_",game_list[i,1])
    }

    if(grepl("\\|",game_list[i,1]))
    {
      game_list[i,1]=gsub("\\|","_",game_list[i,1])
    }

    if(grepl("\\)$",game_list[i,1]))
    {
      game_list[i,1]=strsplit(game_list[i,1]," \\(")[[1]][1]
      
      if(nchar(gsub(" ","",game_list[i,1]))==0)
      {
        game_list[i,1]=game_list[i,7]
        game_list[i,1]=strsplit(game_list[i,1],"\\(")[[1]][1]
      }
    }

    if(grepl("\\]$",game_list[i,1]) & length(strsplit(game_list[i,1]," ")[[1]])>1)
    {
      game_list[i,1]=strsplit(game_list[i,1],"\\[")[[1]][1]
    }

    if(grepl("^\\[",game_list[i,1]) & length(strsplit(game_list[i,1]," ")[[1]])>1)
    {
      game_list[i,1]=strsplit(game_list[i,1],"\\]")[[1]][2]
    }

    if(!is.na(game_list[i,1]) & length(strsplit(game_list[i,1]," ")[[1]])*2==nchar(game_list[i,1])+1)
    {
      game_list[i,1]=gsub(" ","",game_list[i,1])
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
      pasted_value_tunning=NULL

      if(length(strsplit(game_list[i,1]," ")[[1]])==1)
      {
        game_list[i,1]<-gsub(" ","",game_list[i,1])
        
        if(grepl("\\\\/",game_list[i,1]))
        {
          game_list[i,1]=gsub("\\\\/"," / ",game_list[i,1])
        }

        pasted_value_tunning=paste("'",game_list[i,1],":'",sep="")
        pasted_value=paste("'",game_list[i,1],"'",sep="")
      }else if(!is.na(as.numeric(as.roman(strsplit(game_list[i,1], " ")[[1]][length(strsplit(game_list[i,1], " ")[[1]])]))) & length(strsplit(game_list[i,1]," ")[[1]])>1)
      {
        pasted_value_tunning=paste("'",game_list[i,1],":'",sep="")
        pasted_value=paste("'",game_list[i,1],"'",sep="")
      }else if(!is.na(as.numeric(strsplit(game_list[i,1], " ")[[1]][length(strsplit(game_list[i,1], " ")[[1]])])) & length(strsplit(game_list[i,1]," ")[[1]])>1)
      {
        pasted_value_tunning=paste("'",game_list[i,1],":'",sep="")
        pasted_value=paste("'",game_list[i,1],"'",sep="")
      }else
      {
        pasted_value=paste("'",game_list[i,1],"'",sep="")
      }

      band_f=0
      cont_long_string=0
      valor_unico=0
      cont_slash=0
      scape_key=0
      scape_roman_key=0

      while(band_f==0 & scape_key<1)
      {
        cont_long_string=cont_long_string+1
      
        system(paste("node New.js ",pasted_value," > aux_time.txt", sep=""))
      
        if(!is.null(pasted_value_tunning))
        {
          system("rm -rf aux_time_tunning.txt")
          system("touch aux_time_tunning.txt")
          while(file.info("aux_time_tunning.txt")$size==0)
          {
            system(paste("node New.js ",pasted_value_tunning," > aux_time_tunning.txt", sep=""))
            Sys.sleep(1)
          }
        }

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
          
            if(!is.null(pasted_value_tunning))
            {
              data_time_t<-read.delim("aux_time_tunning.txt")

              if(dim(data_time_t)[1]>0)
              {
                data_time_t<-paste(data_time_t)
                name_list_j_t<-NULL
                name_list_j_gpm_t<-NULL
                name_list_j_gpc_t<-NULL
                name_list_j_simil_t<-NULL

                for(u in 2:(str_count(data_time_t,"  name: ")[1]+1))
                {

                  if(grepl(pattern = "\\',",sapply(strsplit(sapply(strsplit(data_time_t, "  name: "), "[[", u),"\\\""), "[[",1)))
                  {
                    name_list_j_t<-c(name_list_j_t, sapply(strsplit(sapply(strsplit(sapply(strsplit(data_time_t, "  name: "), "[[", u),"\\,\\\""), "[[",1),"\\'"), "[[",2))
                  }else
                  {
                    name_list_j_t<-c(name_list_j_t, sapply(strsplit(sapply(strsplit(data_time_t, "  name: "), "[[", u),"\\,\\\""), "[[",1))

                  }
                  
                  name_list_j_gpm_t<-c(name_list_j_gpm_t, sapply(strsplit(sapply(strsplit(data_time_t, "gameplayMain: "), "[[", u),","), "[[",1))
                  name_list_j_gpc_t<-c(name_list_j_gpc_t, sapply(strsplit(sapply(strsplit(data_time_t, "gameplayCompletionist: "), "[[", u),","), "[[",1))
                  name_list_j_simil_t<-c(name_list_j_simil_t, sapply(strsplit(sapply(strsplit(data_time_t, "similarity: "), "[[", u),","), "[[",1))
                }

                name_list_j_simil_t<-as.numeric(name_list_j_simil_t)
              
                name_list_j_t[which(max(name_list_j_simil_t)==name_list_j_simil_t)][1]
                name_list_j_gpm_t[which(max(name_list_j_simil_t)==name_list_j_simil_t)][1]
                name_list_j_gpc_t[which(max(name_list_j_simil_t)==name_list_j_simil_t)][1]

                if(nchar(tolower(stri_trans_general(game_list[i,1], "latin-ascii")))==nchar(tolower(gsub(":","",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]))) & !is.na(match(tolower(stri_trans_general(game_list[i,1], "latin-ascii")),tolower(strsplit(gsub("\\'","_",gsub(":","",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]))," ")[[1]]))) & length(strsplit(game_list[i,1]," ")[[1]])==1)
                {
                  
                }else if(nchar(tolower(stri_trans_general(game_list[i,1], "latin-ascii")))==nchar(tolower(gsub(":","",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]))) & !is.na(match(tolower(stri_trans_general(game_list[i,1], "latin-ascii")),tolower(strsplit(gsub("\\'","_",gsub(" ","_",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]))," ")[[1]]))) & length(strsplit(game_list[i,1]," ")[[1]])==1)
                {
                  
                }else if(nchar(tolower(stri_trans_general(game_list[i,1], "latin-ascii")))==nchar(tolower(gsub(":","",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]))) & !is.na(match(tolower(stri_trans_general(game_list[i,1], "latin-ascii")),tolower(gsub("\\'","_",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1])))))
                {
                  
                }else if(nchar(tolower(gsub("\\!","",stri_trans_general(game_list[i,1], "latin-ascii"))))==nchar(tolower(gsub(":","",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]))) & !is.na(match(tolower(gsub("\\!","",stri_trans_general(game_list[i,1], "latin-ascii"))),tolower(gsub("\\'","_",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1])))))
                {
                                  
                }else if(!is.na(as.numeric(as.roman(strsplit(game_list[i,1], " ")[[1]][length(strsplit(game_list[i,1], " ")[[1]])]))) & !is.na(as.numeric(as.roman(strsplit(name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1], " ")[[1]][length(strsplit(name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1], " ")[[1]])]))) & (as.numeric(as.roman(strsplit(game_list[i,1], " ")[[1]][length(strsplit(game_list[i,1], " ")[[1]])]))==as.numeric(as.roman(strsplit(name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1], " ")[[1]][length(strsplit(name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1], " ")[[1]])]))) & length(strsplit(game_list[i,1], " ")[[1]])==length(strsplit(name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1], " ")[[1]]))
                {

                }else
                {
               
                  name_list_j_gpm<-name_list_j_gpm_t
                  name_list_j_gpc<-name_list_j_gpc_t
                  name_list_j_simil<-name_list_j_simil_t
                  name_list_j<-name_list_j_t
                  
                }
              }
            }

            if(length(strsplit(game_list[i,1]," ")[[1]])==1 & (is.na(match(gsub(":","",tolower(stri_trans_general(game_list[i,1], "latin-ascii"))),stri_trans_general(tolower(strsplit(gsub(" ","_",gsub(":","",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]))," ")[[1]]), "latin-ascii"))) & is.na(match(gsub(":","",tolower(stri_trans_general(game_list[i,1], "latin-ascii"))),stri_trans_general(tolower(strsplit(gsub("'","_",gsub(":","",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]))," ")[[1]]), "latin-ascii"))) & is.na(match(gsub(":","",tolower(stri_trans_general(game_list[i,1], "latin-ascii"))),stri_trans_general(tolower(strsplit(gsub(" ","",gsub(":","",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1]))," ")[[1]]), "latin-ascii"))) & is.na(match(gsub(":","",tolower(stri_trans_general(game_list[i,1], "latin-ascii"))),stri_trans_general(tolower(strsplit(gsub(":","",name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1])," ")[[1]]), "latin-ascii")))))
            {print("hola1")
            print(scape_key)
            print(pasted_value)
            print(name_list_j[which(max(name_list_j_simil)==name_list_j_simil)][1])
              band_f=1
              print(paste(game_list[i,1],": NA",sep = ""))
              
            }else if(max(name_list_j_simil)>0.925)
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
            game_list_back_change=pasted_value

            if(grepl("^'",game_list[i,1])) #######################
            {
              game_list[i,1]=gsub("'","",game_list[i,1])
            }

            if(length(strsplit(game_list[i,1]," ")[[1]])==1)
            {
              if(grepl("\\.",game_list[i,1]))
              {
                game_list[i,1]=paste("'",gsub("\\.","_",game_list[i,1]),"'",sep="")
                pasted_value=game_list[i,1]
              }else if(grepl(":",game_list[i,1]))
              {
                game_list[i,1]=paste("'",gsub(":","",game_list[i,1]),"'",sep="")
                pasted_value=game_list[i,1]
              }else if(!is.na(as.numeric(strsplit(game_list[i,1], "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE)[[1]][2])))
              {
                game_list[i,1]=paste("'",paste(strsplit(game_list[i,1], "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE)[[1]],collapse = " "),"'",sep="")
                pasted_value=game_list[i,1]
              }else if(length(strsplit(str_trim(gsub('([[:upper:]])', ' \\1', game_list[i,1]),side = "both")," ")[[1]])==2)
              {print("hola2")
                game_list[i,1]=paste("'",str_trim(gsub('([[:upper:]])', ' \\1', game_list[i,1]),side = "both"),"'",sep="")
                pasted_value=game_list[i,1]
              }else
              {
                pasted_value=paste("'",game_list[i,1],"'",sep="")
              }

            }else if(grepl("Transformed Collection$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Collection","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Collector_s Edition$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Collector_s Edition","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Collectors Edition$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Collectors Edition","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Ultimate Edition$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Ultimate Edition","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Extended Edition$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Extended Edition","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Gold Edition$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Gold Edition","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Premium Edition$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Premium Edition","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Deluxe Edition$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Deluxe Edition","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Special Editions$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Special Editions","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Maximum Edition$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Maximum Edition","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Remastered Edition$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Remastered Edition","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Remastered$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Remastered","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("The Visual Novel$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("The Visual Novel","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Mega Drive",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Mega Drive","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Complete$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub(" Complete","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Steam Edition$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Steam Edition","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("- Anniversary Edition$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("- Anniversary Edition","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl(" and ",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub(" and "," ",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("\\\\/",game_list[i,1]) & cont_slash==0)
            {        
              game_list[i,1]=paste("'",gsub("\\\\","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
              cont_slash=1
            }else if(grepl("/",game_list[i,1]) & cont_slash==1)
            {        
              game_list[i,1]=paste("'",strsplit(game_list[i,1],"/")[[1]][1],"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("\\\\",game_list[i,1]))
            {        
              game_list[i,1]=paste("'",gsub("\\\\","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(cont_long_string<length(strsplit(game_list[i,1], " ")[[1]]))
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

              if(pasted_value==game_list_back_change)
              {
                scape_key=scape_key-1
              }

            }else if(grepl("^Disney ",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Disney ","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Single Player$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Single Player","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Legacy Edition$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Legacy Edition","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Redux$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Redux","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Prelude$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Prelude","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Multiplayer$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Multiplayer","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("SEASON UPDATE$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("SEASON UPDATE","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("FINAL EDITION$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("FINAL EDITION","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Enhanced Edition$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Enhanced Edition","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Definitive Edition$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Definitive Edition","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Game of the Year Edition$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Game of the Year Edition","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Digital Edition$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Digital Edition","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Edition$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Edition","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("Classic$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("Classic","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("The Original$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("The Original","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("World Arena$",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("World Arena","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }
            else if(grepl("[0-9]:",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("[0-9]","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("[0-9] - ",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub("[0-9] - ","",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl(" the ",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub(" the "," ",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl(" The ",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub(" The "," ",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl(":",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub(":"," ",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl(" - ",game_list[i,1])) #######################
            {
              game_list[i,1]=paste("'",gsub(" - "," ",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("\\.",game_list[i,1]))
            {
              game_list[i,1]=paste("'",gsub("\\.","_",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(grepl("\\)",game_list[i,1]))
            {
              game_list[i,1]=paste("'",gsub("[()]"," ",game_list[i,1]),"'",sep="")
              pasted_value=game_list[i,1]
            }else if(!is.na(as.roman(strsplit(game_list[i,1], " ")[[1]][length(strsplit(game_list[i,1], " ")[[1]])]))) #######################
            {
              if(is.numeric(type.convert(strsplit(game_list[i,1], " ")[[1]][length(strsplit(game_list[i,1], " ")[[1]])],as.is=TRUE)))
              {
                game_list[i,1]=paste("'",paste(paste(paste(strsplit(game_list[i,1], " ")[[1]][1:length(strsplit(game_list[i,1], " ")[[1]])-1],sep=" "),collapse=" "),as.roman(strsplit(game_list[i,1], " ")[[1]][length(strsplit(game_list[i,1]," ")[[1]])]),collapse=" "),"'",sep="")
              }else
              {
                game_list[i,1]=paste("'",paste(paste(paste(strsplit(game_list[i,1], " ")[[1]][1:length(strsplit(game_list[i,1], " ")[[1]])-1],sep=" "),collapse=" "),as.numeric(as.roman(strsplit(game_list[i,1], " ")[[1]][length(strsplit(game_list[i,1]," ")[[1]])])),collapse=" "),"'",sep="")
              }
              pasted_value=game_list[i,1]
              scape_roman_key=scape_roman_key+1
            }else if(is.numeric(try(as.numeric(word2num(strsplit(game_list[i,1], " ")[[1]][length(strsplit(game_list[i,1], " ")[[1]])])[[2]]),silent=TRUE)) & length(strsplit(game_list[i,1]," ")[[1]])>1)
            {
              game_list[i,1]=paste("'",paste(paste(paste(strsplit(game_list[i,1], " ")[[1]][1:length(strsplit(game_list[i,1], " ")[[1]])-1],sep=" "),collapse=" "),as.numeric(word2num(strsplit(game_list[i,1], " ")[[1]][length(strsplit(game_list[i,1], " ")[[1]])])[[2]]),collapse=" "),"'",sep="")
              pasted_value=game_list[i,1]
            }else
            {
              pasted_value<-strsplit(game_list[i,1], " ")[[1]][1]
              valor_unico<-1
            } 

            if(pasted_value==game_list_back_change | scape_roman_key==2)
            {
              scape_key=scape_key+1
            }
          }
        }else
        {
          Sys.sleep(1)
          cont_long_string=cont_long_string-1  ###MIRAR
        }
      }

      if(band_f==0)
      {
        print(paste(game_list[i,1],": NA",sep = ""))
      }

      i=i+1
    }
  }

  game_list[,1]<-game_list_aux[,1]
  write.table(game_list,"Games_HowLong.txt",quote = F,row.names = F,col.names = F,sep = "\t")
}
############################################################################################Updates SteamSpy

if(!file.exists("Games_HowLong_AppID_metadato.txt"))
{
  game_list<-read.delim("Games_HowLong.txt",header=F)

  #### Más metadato

  binded_table<-NULL

  system("rm -rf index.html")
  system("wget https://api.steampowered.com/ISteamApps/GetAppList/v2/")
      
  AppID_List <- read_file("index.html")

  AppID_List<-gsub("\\\"\\},\\{\\\"appid\\\":","``",AppID_List)
  AppID_List<-gsub(",\"name\":\"","``",AppID_List)
  AppID_List<-gsub("\\{\\\"applist\\\":\\{\\\"apps\\\":\\[\\{\\\"appid\\\":","",AppID_List)
  AppID_List<-gsub("\\\"\\}\\]\\}\\}","",AppID_List)

  res_games<-data.frame(matrix(strsplit(AppID_List,"\\`\\`")[[1]],ncol=2,byrow=T))
  binded_table<-rbind(binded_table,res_games)
  print(dim(res_games))

  res_games<-binded_table[!duplicated(binded_table),]

  game_list$AppID<-res_games[match(game_list[,7],res_games[,2]),1]
  game_list$AppID_name<-res_games[match(game_list[,7],res_games[,2]),2]

  for(i in 1:dim(game_list)[1])
  {
    if(!is.na(game_list[i,8]))
    {
      meta_juego<-getURL(paste("https://steamspy.com/api.php?request=appdetails&appid=",game_list[i,8],sep=""))
      game_list[i,10]<-strsplit(strsplit(meta_juego,"name\\\":\\\"")[[1]][2],"\\\",\\\"")[[1]][1]
      game_list[i,11]<-strsplit(strsplit(meta_juego,"genre\\\":\\\"")[[1]][2],"\\\",\\\"")[[1]][1]
      game_list[i,12]<-strsplit(strsplit(meta_juego,"positive\\\":")[[1]][2],",\\\"")[[1]][1]
      game_list[i,13]<-strsplit(strsplit(meta_juego,"negative\\\":")[[1]][2],",\\\"")[[1]][1]
      game_list[i,14]<-strsplit(strsplit(meta_juego,"developer\\\":\\\"")[[1]][2],"\\\",\\\"")[[1]][1]
      game_list[i,15]<-strsplit(strsplit(meta_juego,"publisher\\\":\\\"")[[1]][2],"\\\",\\\"")[[1]][1]
      if(!grepl('tags\\\":\\[\\]',meta_juego))
      {
        game_list[i,16]<-paste(sapply(strsplit(strsplit(strsplit(meta_juego,"tags\\\":\\{")[[1]][2], ',')[[1]],'\\\"'),"[[",2),collapse=", ")
      }
      Sys.sleep(1)
    }

    write.table(game_list,"Games_HowLong_AppID_metadato.txt",quote = F,row.names = F,col.names = F,sep = "\t")

  }
}

############################################################################################Completed

game_list<-read.delim("Games_HowLong_AppID_metadato.txt",header=F)


info_Steam<-getURL(steam_link_achiv)
file_process<-as.data.frame(info_Steam)

h<-file_process[grep("rgGames",file_process[,1]),]

if(str_count(h,'"name"')[1]>0)
{
  for(i in 2:(str_count(h,'"name"')[1]+1))
  {
    if(!is.na(match(substr(strsplit(sapply(strsplit(h[1], '"appid"'), "[[", i),",\\\"")[[1]][1],2,nchar(strsplit(sapply(strsplit(h[1], '"appid"'), "[[", i),",\\\"")[[1]][1])),game_list[,8])))
    {
      game_list[match(substr(strsplit(sapply(strsplit(h[1], '"appid"'), "[[", i),",\\\"")[[1]][1],2,nchar(strsplit(sapply(strsplit(h[1], '"appid"'), "[[", i),",\\\"")[[1]][1])),game_list[,8]),17]<-"X"
    }
  }
}else 
{
  game_list[,17]<-NA
}

#########################################################################################Played time

info_Steam<-getURL(steam_link)
file_process<-as.data.frame(info_Steam)

h<-file_process[grep("rgGames",file_process[,1]),]

for(i in 2:(str_count(h,'"hours_forever"')[1]+1))
{
  game_list[match(substr(strsplit(sapply(strsplit(h[1], '"appid"'), "[[", i),",\\\"")[[1]][1],2,nchar(strsplit(sapply(strsplit(h[1], '"appid"'), "[[", i),",\\\"")[[1]][1])),game_list[,8]),18]<-substr(strsplit(sapply(strsplit(h[1], '"hours_forever"'), "[[", i),",\\\"")[[1]][1],3,nchar(strsplit(sapply(strsplit(h[1], '"hours_forever"'), "[[", i),",\\\"")[[1]][1])-1)
}

#########################################################################################Released date and pc requirements

for(i in 1:dim(game_list)[1])
{
  if(!is.na(game_list[i,8]))
  {
    meta_juego<-getURL(paste("https://store.steampowered.com/api/appdetails/?cc=EU&appids=",game_list[i,8],sep=""))
    
    if(grepl("\"success\"\\:true",meta_juego))
    {
      app_id_gen<-strsplit(strsplit(meta_juego,"\":")[[1]][1],"\"")[[1]][2]
      game_list[match(app_id_gen,game_list[,8]),19]<-format(ymd(paste(strsplit(strsplit(strsplit(strsplit(meta_juego,"release_date")[[1]][2],"\\\"}")[[1]][1],"\\:\\\"")[[1]][2]," ")[[1]][3],strsplit(strsplit(strsplit(strsplit(meta_juego,"release_date")[[1]][2],"\\\"}")[[1]][1],"\\:\\\"")[[1]][2]," ")[[1]][2],strsplit(strsplit(strsplit(strsplit(meta_juego,"release_date")[[1]][2],"\\\"}")[[1]][1],"\\:\\\"")[[1]][2]," ")[[1]][1],sep=" ")), "%d-%b-%Y")
      game_list[match(app_id_gen,game_list[,8]),20]<-paste(unlist(strsplit(gsub("Minimum:","",strsplit(gsub("\",\"recommended\":\""," ",cleanFun(strsplit(strsplit(meta_juego,"pc_requirements\\\"\\:\\{\\\"minimum\\\":\\\"")[[1]][2],"\"},\\\"mac_requirements")[[1]][1])),"Recommended:")[[1]][1]),"\\\\r|\\\\n|\\\\t"))[unlist(strsplit(gsub("Minimum:","",strsplit(gsub("\",\"recommended\":\""," ",cleanFun(strsplit(strsplit(meta_juego,"pc_requirements\\\"\\:\\{\\\"minimum\\\":\\\"")[[1]][2],"\"},\\\"mac_requirements")[[1]][1])),"Recommended:")[[1]][1]),"\\\\r|\\\\n|\\\\t")) != ""],collapse = " ")
      game_list[match(app_id_gen,game_list[,8]),21]<-paste(unlist(strsplit(strsplit(gsub("\",\"recommended\":\""," ",cleanFun(strsplit(strsplit(meta_juego,"pc_requirements\\\"\\:\\{\\\"minimum\\\":\\\"")[[1]][2],"\"},\\\"mac_requirements")[[1]][1])),"Recommended:")[[1]][2],"\\\\r|\\\\n|\\\\t"))[unlist(strsplit(strsplit(gsub("\",\"recommended\":\""," ",cleanFun(strsplit(strsplit(meta_juego,"pc_requirements\\\"\\:\\{\\\"minimum\\\":\\\"")[[1]][2],"\"},\\\"mac_requirements")[[1]][1])),"Recommended:")[[1]][2],"\\\\r|\\\\n|\\\\t")) != ""],collapse = " ")
    }

    Sys.sleep(1.5)
  }
}


######################################################################################Removed games list

file_process<-gsub("&quot;","\\\\",gsub("&amp;","&",gsub("&#039;","'",cleanFun2(read_file(info_Steam_removed)))))

for(i in 2:(str_count(file_process,'\\;\\?\\;')[1]+1))
{
  if(!is.na(match(strsplit(file_process, '\\;\\?\\;')[[1]][i],game_list[,7])))
  {
    game_list[match(strsplit(file_process, '\\;\\?\\;')[[1]][i],game_list[,7]),22]<-"X"
  }
}

#####

game_list$rating<-round((game_list[,12]/(game_list[,12]+game_list[,13]))*100,digits=1)
game_list$tot_votes<-(game_list[,12]+game_list[,13])

game_list_final_output<-game_list[,c(7,8,11,16,24,23,18,3,4,17,14,15,19,22,20,21)]

colnames(game_list_final_output)<-c("Name","AppID","Genre","Tags","Votes_total","Positive_rating","Played_time (h)","Time_to_finish (h)","Time_to_complete (h)","100% Completed","Developer","Publisher","Release date","Removed game","Minimum requirements","Recommended requirements")

write.table(game_list_final_output,"Steam_Library_Metadata.txt",quote = F,row.names = F,sep = "\t")



