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

isEmpty <- function(x) { #This function checks if a data frame is empty or not
  return(length(x)==0)
}

game_list<-read.delim("Games_HowLong_v9.txt")

#### Más metadato

### Pillar AppID

### Más info: https://partner.steamgames.com/doc/webapi/ISteamApps#GetAppList

library(rvest)

AppID_List <- read_html("https://api.steampowered.com/ISteamApps/GetAppList/v2/")

#library(rvest)
#my_df <- as.data.frame(read_html(index.html) %>% html_table(fill=TRUE))

### Datos del juego desde AppID

### Más info: https://steamspy.com/api.php

https://steamspy.com/api.php?request=appdetails&appid=42640

