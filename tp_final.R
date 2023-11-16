require(ncdf4)
require(udunits2)
require(metR)
require(ggplot2)
#cargo las librerias necesarias

GlanceNetCDF(archivo)
nc<-nc_open(archivo)
nc
datos_sst<-ReadNetCDF(archivo,vars = "sst")
#no es necesario abrirlo completo, de una ya completado esta ok


############ ITEM A ###############


archivo<-"/home/clinux01/Documentos/TP FINAL labo_cande/sst.mnmean_ERSST.nc" #COMPU JUEVES
archivo<-"C:/Users/cannm/OneDrive/Documentos/sst.mnmean_ERSST.nc" #CASA
archivo<-"~/Downloads/sst.mnmean_ERSST.nc"  #COMPU LUNES
GlanceNetCDF(archivo)

#abro para seleccionando la region mar argentino y el periodo de 1990-2019 (30 años) 
sst_mar_argentino<-ReadNetCDF(archivo,vars = "sst",subset = list(lat=c(-60,-20),lon=c(290,320),time=c("1990-01-01","2019-12-01")))

#arreglo las longitudes para que tome bien el formato (-180 a 180)
sst_mar_argentino$lon<-ConvertLongitude(sst_mar_argentino$lon)

#grafico un mapa para conocer la region trabajada
mapa<-map_data("world")

mi_mapa<-geom_path(data=mapa,mapping=aes(long,lat,group=group))
ggplot(sst_mar_argentino,aes(x=lon,y=lat))+
  geom_raster(aes(fill=sst))+
  mi_mapa+
  coord_sf(xlim=c(-180,20),ylim=c(-90,0))+
  geom_rect(xmin=-70,xmax=-40,ymin=-60,ymax=-20,fill=NA,colour="red")+
  theme(panel.background = element_rect(fill="skyblue"),
        panel.ontop = F)+
  labs(x="Longitud",
       y="Latitud",
       fill="Temp superficial del mar (C°)")
  



##############  ITEM B ##############

require(lubridate)

sst_mar_argentino$mes<-month(sst_mar_argentino$time)

climatologia<-aggregate(sst_mar_argentino$sst,list(sst_mar_argentino$mes,sst_mar_argentino$lat,sst_mar_argentino$lon),mean,na.rm=T) 
#a los datos de sst, le aplico la funcion mean, para cada mes lat y lon y me devuelve un nuevo data frame

#hay valores Nan pero son esperables porque son coordenadas de sup terrestre y no voy a tener datos

colnames(climatologia)<-c("Mes","Latitud","Longitud","Climatologia.Mensual")
#nombro a las columnas con las variables



#opcion 1  graficar en el ciclo y guardarlo , ES LA IDEA
meses<-1:12
for (i in meses) {
  mes<-meses[i]
  #datos_clima_mes<-climatologia[climatologia$Mes==mes,]
  g<-ggplot(climatologia,aes(x=Longitud,y=Latitud))+
    geom_raster(aes(fill=Climatologia.Mensual))+
    coord_sf(xlim=c(360-80,360-20),ylim=c(-70,-20))+
    facet_wrap(~Mes,ncol=6)+
    labs(x="Longitud",
         y="Latitud",
         fill="Climatologia Mensual")
  }






###########  ITEM C ###################

#con 40s elijo 60W,51w,45w
#con 30s elijo 50w,46w,40w

#abro nuevamente los datos con las coordenadas seleccionadas

sst_40S<-ReadNetCDF(archivo,vars = "sst",subset = list(lat=-40,lon=c(360-60,360-51,360-45)))
sst_40S$mes<-month(sst_40S$time)
sst_40S$anio<-year(sst_40S$time)

sst_30S<-ReadNetCDF(archivo,vars = "sst",subset = list(lat=-30,lon=c(360-50,360-40,360-46)))
sst_30S$mes<-month(sst_30S$time)
sst_30S$anio<-year(sst_30S$time)


#promedio para 40°S y otro para 30°S
serie_temp_40s<-aggregate(sst_40S$sst,list(sst_40S$mes,sst_40S$lat,sst_40S$anio),mean)
serie_temp_30s<-aggregate(sst_30S$sst,list(sst_30S$mes,sst_30S$lat,sst_30S$anio),mean)

colnames(serie_temp_40s)<-c("Mes","Latitud","Anio","Promedio")
colnames(serie_temp_30s)<-c("Mes","Latitud","Anio","Promedio")


serie_temp_30s$Fecha<-paste(serie_temp_30s$Anio,serie_temp_30s$Mes,sep = "-")
##arreglar
ggplot(serie_temp_30s,mapping=aes(x=Fecha,y=Promedio))+
  geom_line(color="red")+
  labs(title="Serie Temporal Promedio",
       subtitle = "Latitud: 30°S",
       fill="Anio")+
  theme_linedraw()

#serie_temp_40s$Fecha<-paste(serie_temp_40s$Anio,serie_temp_40s$Mes,sep="-")

ggplot(serie_temp_40s,aes(x=Mes,y=Promedio,color=Anio))+
  geom_col(aes(fill=factor(Anio)),position = "dodge")+
  scale_color_continuous(aes("Anios"))+
  labs(title="Serie Temporal Promedio de la latitud 40°S",
       subtitle = "Latitud: 40°S ",
       x="Meses",
       fill="Anio")+
  theme_get()



############# ITEM D #############


#promedio para 40°S y otro para 30°S
onda_anual_40s<-aggregate(serie_temp_40s$Promedio,list(serie_temp_40s$Mes,serie_temp_40s$Latitud),mean)
onda_anual_30s<-aggregate(serie_temp_30s$Promedio,list(serie_temp_30s$Mes,serie_temp_30s$Latitud),mean)

colnames(onda_anual_40s)<-c("Mes","Latitud","Promedio")
colnames(onda_anual_30s)<-c("Mes","Latitud","Promedio")

#calculo el desvio estandar de todos los E,F,M etc del periodo
desvio_40S<-aggregate(serie_temp_40s$Promedio,list(serie_temp_40s$Mes,serie_temp_40s$Latitud),sd)
desvio_30S<-aggregate(serie_temp_30s$Promedio,list(serie_temp_30s$Mes,serie_temp_30s$Latitud),sd)

#nombro las columnas
colnames(desvio_40S)<-c("Mes","Latitud","Desvio")
colnames(desvio_30S)<-c("Mes","Latitud","Desvio")

#AGREGAR EL DESVO Y GRAFICAR

ggplot(onda_anual_30s,aes(x=Mes,y=(Promedio)))+
  geom_line(color="cornflowerblue")+
  geom_point(color="midnightblue",size=4,alpha=0.6)+
  labs(title="Onda Anual",
       subtitle = "Latitud: 30°S
Promedio de temperatura",
       x="Meses",
       y="Temperatura(C°)")+
  theme_get()+
  scale_x_continuous(breaks = c(1:12))



ggplot(onda_anual_40s,aes(x=Mes,y=Promedio))+
  geom_line()+
  geom_point(size=4,alpha=0.6)+
  scale_color_continuous()+
  labs(title="Onda Anual",
       subtitle = "Latitud: 40°S
Promedio de Temperatura",
       x="Meses",
       y="Temperatura (C°)")+
  theme_get()+
  scale_x_continuous(breaks = c(1:12))




############# ITEM E #############

serie_30S_ordenada<-serie_temp_30s[order(serie_temp_30s$Promedio,decreasing=T),]
serie_40S_ordenada<-serie_temp_40s[order(serie_temp_40s$Promedio),]

info_30s<-data.frame(Anio=serie_30S_ordenada$Anio[1:20],Mes=serie_30S_ordenada$Mes[1:20],Temperatura=serie_30S_ordenada$Promedio[1:20]) 
info_40s<-data.frame(Anio=serie_40S_ordenada$Anio[1:20],Mes=serie_40S_ordenada$Mes[1:20],Temperatura=serie_40S_ordenada$Promedio[1:20])
##es seleccionar los 20 primeros meses q aparecen sim importar que sean el mismo, cambia el anioo, osea SELECCIONO LAS 10 primeras FILAS
#cambie a 20 para ver mejor porq sino era muy corto


#Guardo las tablas en archivo ascii y se guarda en mi diretorio
write.table(info_30s,file="Temp_Mar_Argentino_30S.txt")
write.table(info_40s,file="Temp_Mar_Argentino_40S.txt")





####EXTRA PARA CHEQUEAR LOS GRAFICOS DE SERIES TEMPORALES##
serie_temp_30s$nro=1:length(serie_temp_30s$Fecha)
g<-ggplot(data=serie_temp_30s,aes(x=nro,y=Promedio,color=Promedio))+
  geom_line(aes(color="Promedio"))+
  labs(title="Serie Temporal Promedio",
       subtitle = "Latitud: 30°S",
       x="Meses",
       fill="Anio")+xlim(0,2030)+ylim(min(serie_temp_30s$Promedio),max(serie_temp_30s$Promedio))+theme_linedraw()+ theme(legend.position = "none")
g<-g+ scale_x_continuous(labels=serie_temp_30s$Fecha[seq(0, 2030, 50)],breaks = seq(0, 2030, 50))
g
