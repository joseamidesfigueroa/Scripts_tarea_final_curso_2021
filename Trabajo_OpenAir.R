library(openair)
library(ggplot2)
#Lee los datos desde el archivo previamente generado
datos<-
  read.table("datos_ESA_openair.csv",sep = ";",header = TRUE, na.strings = "NA")
#Da formato de fecha a la columna de fechas según el formato de OpenAir
datos$date<-as.POSIXct(datos$date, format="%d/%m/%Y %H:%M", tz="UTC")
datos$NO2[datos$NO2 < 0]<-NA
datos$SO2[datos$SO2 < 0]<-NA
datos$CO[datos$CO < 0]<-NA
datos$O3[datos$O3 < 0]<-NA
lista_gases<-c("NO2","SO2","CO","O3","PM25","WD","WS","TEMP","RH","PRESS")
titulos<-c("Estadistico","NO2","SO2","CO","O3","PM25","WD","WS","TEMP","RH","PRESS")
#-------------------------------------->
#Maximos
maximos<-cbind.data.frame(max(datos$NO2,na.rm=T),max(datos$SO2,na.rm=T),max(datos$CO,na.rm=T),max(datos$O3,na.rm=T),max(datos$PM25,na.rm=T), max(datos$wd,na.rm=T),max(datos$ws,na.rm=T),max(datos$temp,na.rm=T),max(datos$RH,na.rm=T),max(datos$P,na.rm=T))
names(maximos)<-lista_gases
#-------------------------------------->
#Minimos
minimos<-cbind.data.frame(min(datos$NO2,na.rm=T),min(datos$SO2,na.rm=T),min(datos$CO,na.rm=T),min(datos$O3,na.rm=T),min(datos$PM25,na.rm=T), min(datos$wd,na.rm=T),min(datos$ws,na.rm=T),min(datos$temp,na.rm=T),min(datos$RH,na.rm=T),min(datos$P,na.rm=T))
names(minimos)<-lista_gases
#-------------------------------------->
#-------------------------------------->
#Promedios
medias<-cbind.data.frame(mean(datos$NO2,na.rm=T),mean(datos$SO2,na.rm=T),mean(datos$CO,na.rm=T),mean(datos$O3,na.rm=T),mean(datos$PM25,na.rm=T), mean(datos$wd,na.rm=T),mean(datos$ws,na.rm=T),mean(datos$temp,na.rm=T),mean(datos$RH,na.rm=T),mean(datos$P,na.rm=T))
names(medias)<-lista_gases
#-------------------------------------->
#-------------------------------------->
#medianas
medianas<-cbind.data.frame(median(datos$NO2,na.rm=T),median(datos$SO2,na.rm=T),median(datos$CO,na.rm=T),median(datos$O3,na.rm=T),median(datos$PM25,na.rm=T),median(datos$wd,na.rm=T),median(datos$ws,na.rm=T),median(datos$temp,na.rm=T),median(datos$RH,na.rm=T),median(datos$P,na.rm=T))
names(medianas)<-lista_gases
#-------------------------------------->
#-------------------------------------->
#Defino los percentiles a trabajar
lista_percentiles<-c(0.1,0.33,0.66,0.9,0.95,0.99)
#Calculo de Percentiles
percentiles<-cbind.data.frame(cbind.data.frame(quantile(datos$NO2,probs = lista_percentiles,na.rm = T)),cbind.data.frame(quantile(datos$SO2,probs = lista_percentiles,na.rm = T)),
                              cbind.data.frame(quantile(datos$CO,probs = lista_percentiles,na.rm =  T)),cbind.data.frame(quantile(datos$O3,probs = lista_percentiles,na.rm = T)),
                              cbind.data.frame(quantile(datos$PM25,probs = lista_percentiles,na.rm = T)),cbind.data.frame(quantile(datos$wd,probs = lista_percentiles,na.rm = T)),
                              cbind.data.frame(quantile(datos$ws,probs = lista_percentiles,na.rm =  T)),cbind.data.frame(quantile(datos$temp,probs = lista_percentiles,na.rm = T)),
                              cbind.data.frame(quantile(datos$RH,probs = lista_percentiles,na.rm = T)),cbind.data.frame(quantile(datos$P,probs = lista_percentiles,na.rm = T)))

names(percentiles)<-lista_gases
#-------------------------------------->
#-------------------------------------->
#Genero tabla de resumen con los estadísticos que quiero
lista_estadisticos<-rbind.data.frame("Maximo","Minimo","Promedio","Mediana","Percentil 10","Percentil
33","Percentil 66","Percentil 90","Percentil 95","Percentil 99")
estadisticas_gases<-rbind(maximos,minimos,medias,medianas)
df<-rbind(estadisticas_gases,percentiles)
df<-cbind(lista_estadisticos,df)
names(df)<-titulos
write.table(df,"C:/Users/arw/Dropbox/Curso_Calidad_Aire_2021/trabajo_final/tabla_resumen.csv",sep=";",quote = FALSE, col.names = TRUE, row.names = FALSE)
#-------------------------------------->
#-------------------------------------->
#Histograma
hist<-ggplot(datos,aes(x=PM25))+ 
  geom_histogram(color="black",fill="lightblue")+
  ggtitle("Histograma de frecuencia para PM2.5 para estacion CODEM\n Datos cada hora para todo 2020")+
  labs(y="Frecuencia", x="Concentración PM2.5")+
  theme(legend.position="top")
hist
ggsave("C:/Users/arw/Dropbox/Curso_Calidad_Aire_2021/trabajo_final/hist_pm25.png",width = 800,height
       =1024,units="px",scale=3)
#-------------------------------------->
#-------------------------------------->
#Linea de tiempo
plot<-ggplot(datos,aes(x=as.Date(date),y=PM25))+
  geom_line(colour="darkblue", size=1.25)+
  ggtitle("Concentraciones de PM2.5, estacion CODEM, datos cada hora para todo 2020")+
  labs(y="Concentración PM2.5", x="tiempo")+
  theme(legend.position="center")
plot
ggsave("C:/Users/arw/Dropbox/Curso_Calidad_Aire_2021/trabajo_final/linea_pm25.png",width = 1024,height
       =350,units="px",scale=6)
#-------------------------------------->
#-------------------------------------->
#Cuadro de resumen
summaryPlot(datos)
#-------------------------------------->
#-------------------------------------->

#Calendario
calendarPlot(datos, pollutant = "PM25")
#-------------------------------------->
#-------------------------------------->
#Calendario de rosa de vientos
windRose(datos, type = "month", layout = c(4, 3), pollutant = "PM25")
#-------------------------------------->
#-------------------------------------->
#Cuadro de resumen
pollutionRose(datos,pollutant = "PM25")
#-------------------------------------->
#-------------------------------------->
#Percentil rose de PM25
percentileRose(datos, pollutant = "PM25", smooth = TRUE)
#-------------------------------------->
