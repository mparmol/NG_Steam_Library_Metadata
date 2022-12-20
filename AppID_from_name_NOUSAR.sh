#!/bin/bash    
#Example Usage:  ./playercount.sh "Left 4 Dead 2"    
appid=$(curl -s https://api.steampowered.com/ISteamApps/GetAppList/v2/ | jq ".applist.apps[] | select(.name==\"$1\")" | jq '.appid')    
#curl -s https://api.steampowered.com/ISteamUserStats/GetNumberOfCurrentPlayers/v1/?appid="$appid" | jq '.response.player_count'
echo $appid

steamspy.com/api.php?request=appdetails&appid=$appid