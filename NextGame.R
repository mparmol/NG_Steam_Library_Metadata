##NextGame

### Extract game list, from windows

https://github.com/Depressurizer/Depressurizer/releases

### Extract gameplay time

system("node New.js > aux_time.txt")

data_time<-read.delim("aux_time.txt")

gsub(",","",strsplit(grep("gameplayMain:",data_time[,1],value=T), "Main: ")[[1]][2])
