datos<-read.delim("Games_HowLong.txt",header = F)
datos_lim<-na.omit(datos)
datos_lim<-datos_lim[datos_lim[,3]!="Sin registro de tiempo",]
datos_lim[,3]<-as.numeric(datos_lim[,3])
datos_lim[,4]<-as.numeric(datos_lim[,4])


library(ggplot2)


ggplot(datos_lim, aes(V4,V3,label=V1)) + geom_point() + theme_bw() + geom_text(hjust=0, vjust=0) + ylab("Tiempo pasartelo (h)") + xlab("Tiempo completarlo 100% (h)")

hist(datos_lim$X184)
