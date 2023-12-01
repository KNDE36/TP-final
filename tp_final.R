#########   Trabajo Final   ################
#########  TSM en el Mar Argentino  ###############


rm(list = ls())
setwd()

#cargo las librerias necesarias
require(ncdf4)
require(udunits2)
require(metR)
require(ggplot2)
require(lubridate)



############ ITEM A ###############


archivo<-"/home/clinux01/Documentos/TP FINAL labo_cande/COBE_sst.mon.mean.nc" #COMPU JUEVES
archivo<-"C:/Users/cannm/OneDrive/Documentos/COBE_sst.mon.mean.nc" #CASA
archivo<-"~/Downloads/COBE_sst.mnmean_ERSST.nc"  #COMPU LUNES

GlanceNetCDF(archivo)


#datos completos de sudamerica para graficar zona de trabajo
sst_mar_argentino1<-ReadNetCDF(archivo,vars = "sst",subset = list(lat=c(-60,0),lon=c(280,325),time=c("1990-01-01","2019-12-01")))

#datos para trabajar la region mar argentino en el periodo 1990-2023 (30 anios)
sst_mar_argentino<-ReadNetCDF(archivo,vars = "sst",subset = list(lat=c(-60,-20),lon=c(290,320),time=c("1990-01-01","2019-12-01")))

#arreglo el formato de las longitudes de (0 a 360) a (-180 a 180)
sst_mar_argentino1$lon<-ConvertLongitude(sst_mar_argentino1$lon)
sst_mar_argentino$lon<-ConvertLongitude(sst_mar_argentino$lon)


#grafico un mapa para conocer la region trabajada
mapa<-map_data("world")

mi_mapa<-geom_path(data=mapa,mapping=aes(long,lat,group=group))


ggplot(sst_mar_argentino1,aes(x=lon,y=lat))+
  geom_raster(aes(fill=sst))+
  mi_mapa+
  coord_sf(xlim=c(-80,-31),ylim=c(-65,0))+
  geom_rect(xmin=-70,xmax=-40,ymin=-60,ymax=-20,fill=NA,colour="red")+
  theme(panel.background = element_rect(fill="skyblue"),
        panel.ontop = F)+
  labs(x="Longitud",
       y="Latitud",
       fill="Temp superficial del mar (C°)")




##############  ITEM B ################


sst_mar_argentino$mes<-month(sst_mar_argentino$time)

climatologia<-aggregate(sst_mar_argentino$sst,list(sst_mar_argentino$mes,sst_mar_argentino$lat,sst_mar_argentino$lon),mean,na.rm=T) 
#a los datos de sst,aplico el promedio para cada mes,lat y long

#hay valores Nan pero son esperables porque son coordenadas del continente y no existen datos


colnames(climatologia)<-c("Mes","Latitud","Longitud","Climatologia.Mensual")

climatologia$Longitud<-ConvertLongitude(climatologia$Longitud) 


require(RColorBrewer)
require(scales)


 
campos<-ggplot(climatologia,aes(x=Longitud,y=Latitud))+
    geom_raster(aes(fill=Climatologia.Mensual))+
    mi_mapa+
    coord_sf(xlim=c(-70,-40),ylim=c(-60,-20))+
    facet_wrap(~Mes,ncol=6)+
    labs(x="Longitud",
         y="Latitud",
         fill="Climatologia Mensual")+
    scale_colour_gradientn(name="Temperatura (C)",colours=rev(brewer.pal(9,"RdYlBu")),limits=c(min(climatologia$Climatologia.Mensual),max(climatologia$Climatologia.Mensual)),na.value = "white",breaks=pretty_breaks(n=9),aesthetics = c("fill"))
   
  
campos
#para ver los campos de la climatologia graficados





#############   ITEM C  ###################

#con 40s elijo 60W,51w,45w
#con 30s elijo 48w,46w,40w

#abro nuevamente los datos con las coordenadas seleccionadas

sst_40S<-ReadNetCDF(archivo,vars = "sst",subset = list(lat=-40,lon=c(360-60,360-51,360-45),time=c("1990-01-01","2019-12-01")))
sst_40S$mes<-month(sst_40S$time)
sst_40S$anio<-year(sst_40S$time)

sst_30S<-ReadNetCDF(archivo,vars = "sst",subset = list(lat=-30,lon=c(360-48,360-46,360-40),time=c("1990-01-01","2019-12-01")))
sst_30S$mes<-month(sst_30S$time)
sst_30S$anio<-year(sst_30S$time)


#promedio para 40S y otro para 30S
serie_temp_40s<-aggregate(sst_40S$sst,list(sst_40S$mes,sst_40S$lat,sst_40S$anio),mean)
serie_temp_30s<-aggregate(sst_30S$sst,list(sst_30S$mes,sst_30S$lat,sst_30S$anio),mean)

colnames(serie_temp_40s)<-c("Mes","Latitud","Anio","Promedio")
colnames(serie_temp_30s)<-c("Mes","Latitud","Anio","Promedio")


           #######  30 S ###########


#reescribo fecha para poder graficar
serie_temp_30s$Fecha<-paste(serie_temp_30s$Anio,serie_temp_30s$Mes,sep = "-")

#recorto solo 6 anios para ver mejor
recorte_30<-subset(serie_temp_30s,serie_temp_30s$Anio>=2003 & serie_temp_30s$Anio<=2008)

recorte_30$Numero=1:length(recorte_30$Mes)

ggplot(recorte_30,aes(x=Numero,y=Promedio))+
  geom_col(aes(fill=factor(Anio)),position="dodge")+
  labs(title="Serie Temporal Promedio",
       subtitle = "Latitud: 30 grados sur",
       x="Meses",
       y="Temperatura (C)",
       fill="Anios")+
  scale_x_continuous(labels = month(recorte_30$Mes[seq(1,length(recorte_30$Numero),6)],label = T),breaks=seq(1,length(recorte_30$Numero),6))+
  theme_get()



      ########  40 S #############

#reescribo fecha para poder graficar
serie_temp_40s$Fecha<-paste(serie_temp_40s$Anio,serie_temp_40s$Mes,sep="-")

#recorto y tomo solo 6 anios para ver mejor
recorte_40<-subset(serie_temp_40s,serie_temp_40s$Anio>=2003 & serie_temp_40s$Anio<=2008)

recorte_40$Numero=1:length(recorte_40$Mes)

ggplot(recorte_40,aes(x=Numero,y=Promedio))+
  geom_col(aes(fill=factor(Anio)),position = "dodge")+
  labs(title="Serie Temporal Promedio",
       subtitle = "Latitud: 40 grados sur ",
       x="Meses",
       y="Temperatura (C)",
       fill="Anios")+
   scale_x_continuous(labels = month(recorte_40$Mes[seq(1,length(recorte_40$Numero),6)],label = T),breaks=seq(1,length(recorte_40$Numero),6))+
  theme_get()




#############  ITEM D  #############


#promedio para 40S y otro para 30S
onda_anual_40s<-aggregate(serie_temp_40s$Promedio,list(serie_temp_40s$Mes,serie_temp_40s$Latitud),mean)
onda_anual_30s<-aggregate(serie_temp_30s$Promedio,list(serie_temp_30s$Mes,serie_temp_30s$Latitud),mean)

colnames(onda_anual_40s)<-c("Mes","Latitud","Promedio")
colnames(onda_anual_30s)<-c("Mes","Latitud","Promedio")

#calculo el desvio estandar de todos los E,F,M (meses)etc.. del periodo
desvio_40S<-aggregate(serie_temp_40s$Promedio,list(serie_temp_40s$Mes,serie_temp_40s$Latitud),sd)
desvio_30S<-aggregate(serie_temp_30s$Promedio,list(serie_temp_30s$Mes,serie_temp_30s$Latitud),sd)

#Nombro las columnas
colnames(desvio_40S)<-c("Mes","Latitud","Desvio")
colnames(desvio_30S)<-c("Mes","Latitud","Desvio")


#Agrego columnas con la suma y la resta del promedio con el desvio
onda_anual_30s$Resta_Desvio<-(onda_anual_30s$Promedio-desvio_30S$Desvio)
onda_anual_30s$Suma_Desvio<-(onda_anual_30s$Promedio+desvio_30S$Desvio)

onda_anual_40s$Resta_Desvio<-(onda_anual_40s$Promedio-desvio_40S$Desvio)
onda_anual_40s$Suma_Desvio<-(onda_anual_40s$Promedio+desvio_40S$Desvio)



ggplot(onda_anual_30s,aes(x=Mes,y=(Promedio)))+
  geom_line(color="cyan3",size=2)+
  geom_point(color="cornflowerblue",size=5,alpha=0.9)+
  geom_ribbon(onda_anual_30s,mapping=aes(ymin=Resta_Desvio,ymax=Suma_Desvio),color="#bee8f9",fill="#89d6f5",alpha=0.4)+
  labs(title="Onda Anual",
       subtitle = "Latitud: 30 grados sur
Promedio de temperatura",
       x="Meses",
       y="Temperatura(C)")+
  theme_get()+
  scale_x_continuous(breaks = c(1:12))



ggplot(onda_anual_40s,aes(x=Mes,y=Promedio))+
  geom_line(color="cyan3",size=2)+
  geom_point(color="cornflowerblue",size=5,alpha=0.9)+
  geom_ribbon(onda_anual_40s,mapping=aes(ymin=Resta_Desvio,ymax=Suma_Desvio),color="#bee8f9",fill="#89d6f5",alpha=0.4)+
  labs(title="Onda Anual",
       subtitle = "Latitud: 40 grados sur
Promedio de Temperatura",
       x="Meses",
       y="Temperatura (C)")+
  theme_get()+
  scale_x_continuous(breaks = c(1:12))




############# ITEM E #############

serie_30S_ordenada<-serie_temp_30s[order(serie_temp_30s$Promedio,decreasing=T),]
serie_40S_ordenada<-serie_temp_40s[order(serie_temp_40s$Promedio),]

info_30s<-data.frame(Anio=serie_30S_ordenada$Anio[1:20],Mes=serie_30S_ordenada$Mes[1:20],Temperatura=serie_30S_ordenada$Promedio[1:20]) 
info_40s<-data.frame(Anio=serie_40S_ordenada$Anio[1:20],Mes=serie_40S_ordenada$Mes[1:20],Temperatura=serie_40S_ordenada$Promedio[1:20])
#Selecciono los 20 primeros meses para ver mejor


#Guardo las tablas en archivo ascii y se guarda en mi diretorio
write.table(info_30s,file="Temp_Mar_Argentino_30S.txt")
write.table(info_40s,file="Temp_Mar_Argentino_40S.txt")




############################ Resultados destacados extra #######################


info_30s$Fecha<-paste(info_30s$Anio,info_30s$Mes,sep="-")

time<-ym(info_30s$Fecha)  #lo determino como clase "date" para poder usar scale_x_date

info_30s$Date<-time  #lo agrego al data frame 

info_30s$Fecha<-NULL  #borro la la anterior columna de fecha con distinto formato

## GRAFICO DE INFLUENCIA DE LA CORRIENTE CALIDA DE BRASIL ###
ggplot(info_30s,aes(x=Date,y=Temperatura))+
  geom_col(color="red2")+
  scale_x_date(date_breaks = "1 years",date_labels = "%m-%Y")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(title = "Influencia de la Cte de Brasil",
       x="Fecha",
       y="Temperatura (C°)")+
  ylim(0,26.75)





info_40s$Fecha<-paste(info_40s$Anio,info_40s$Mes,sep="-")

time<-ym(info_40s$Fecha)  #lo determino como clase "date" para poder usar scale_x_date

info_40s$Date<-time  #lo agrego al data frame 

info_40s$Fecha<-NULL  #borro la la anterior columna de fecha con distinto formato

## GRAFICO DE INFLUENCIA DE LA CORRIENTE FRIA DE MALVINAS ###
ggplot(info_40s,aes(x=Date,y=Temperatura))+
  geom_col(color="#8ED9F0")+ 
  scale_x_date(date_breaks = "1 years",date_labels = "%m-%Y")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(title = "Influencia de la Cte de Malvinas",
       x="Fecha",
       y="Temperatura (C°)")+
  ylim(0,20)





