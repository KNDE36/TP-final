require(ncdf4)
require(udunits2)
require(metR)
require(ggplot2)
#cargo las librerias necesarias


############ ITEM A ###############


archivo<-"/home/clinux01/Documentos/TP FINAL labo_cande/COBE_sst.mon.mean.nc" #COMPU JUEVES
archivo<-"C:/Users/cannm/OneDrive/Documentos/sst.mnmean_ERSST.nc" #CASA
archivo<-"~/Downloads/sst.mnmean_ERSST.nc"  #COMPU LUNES
GlanceNetCDF(archivo)

#abro para seleccionando la region mar argentino y el periodo de 1990-2019 (30 anios) 

#datos completos de sudamerica para graficar zona de trabajo
sst_mar_argentino1<-ReadNetCDF(archivo,vars = "sst",subset = list(lat=c(-60,0),lon=c(280,325),time=c("1990-01-01","2019-12-01")))

#datos para trabajar de la region
sst_mar_argentino<-ReadNetCDF(archivo,vars = "sst",subset = list(lat=c(-60,-20),lon=c(290,320),time=c("1990-01-01","2019-12-01")))

#arreglo las longitudes para que tome bien el formato (-180 a 180)
sst_mar_argentino1$lon<-ConvertLongitude(sst_mar_argentino1$lon)
sst_mar_argentino$lon<-ConvertLongitude(sst_mar_argentino2$lon)

#grafico un mapa para conocer la region trabajada
mapa<-map_data("world")

mi_mapa<-geom_path(data=mapa,mapping=aes(long,lat,group=group))


ggplot(sst_mar_argentino1,aes(x=lon,y=lat))+
  geom_raster(aes(fill=sst))+
  mi_mapa+
  coord_sf(xlim=c(-80,-20),ylim=c(-65,0))+
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

climatologia$Longitud<-ConvertLongitude(climatologia$Longitud)

require(RColorBrewer)
require(scales)

my_scale <- scale_colour_gradientn(name="Temperatura (C)",colours=rev(brewer.pal(9,"RdYlBu")),limits=c(min(climatologia$Climatologia.Mensual),max(climatologia$Climatologia.Mensual)),na.value = "white",breaks=pretty_breaks(n=9),aesthetics = c("colour","fill"))

#escala de colores ROJO-AMARILLO-AZUL que arranca al reves con rev, AZUL-AMARILLO-ROJO


 
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
#para ver los campos graficados





###########  ITEM C ###################

#con 40s elijo 60W,51w,45w
#con 30s elijo 50w,46w,40w

#abro nuevamente los datos con las coordenadas seleccionadas

sst_40S<-ReadNetCDF(archivo,vars = "sst",subset = list(lat=-40,lon=c(360-60,360-51,360-45),time=c("1990-01-01","2019-12-01")))
sst_40S$mes<-month(sst_40S$time)
sst_40S$anio<-year(sst_40S$time)

sst_30S<-ReadNetCDF(archivo,vars = "sst",subset = list(lat=-30,lon=c(360-48,360-40,360-46),time=c("1990-01-01","2019-12-01")))
sst_30S$mes<-month(sst_30S$time)
sst_30S$anio<-year(sst_30S$time)


       #######  40 S ###########

#promedio para 40S y otro para 30S
serie_temp_40s<-aggregate(sst_40S$sst,list(sst_40S$mes,sst_40S$lat,sst_40S$anio),mean)
serie_temp_30s<-aggregate(sst_30S$sst,list(sst_30S$mes,sst_30S$lat,sst_30S$anio),mean)

colnames(serie_temp_40s)<-c("Mes","Latitud","Anio","Promedio")
colnames(serie_temp_30s)<-c("Mes","Latitud","Anio","Promedio")

#reescribo fecha para poder graficar
serie_temp_30s$Fecha<-paste(serie_temp_30s$Anio,serie_temp_30s$Mes,sep = "-")

#recorto solo 6 años para ver mejor
recorte_30<-subset(serie_temp_30s,serie_temp_30s$Anio>=2003 & serie_temp_30s$Anio<=2008)

ggplot(recorte_30,aes(x=Fecha,y=Promedio))+
  geom_col(aes(fill=factor(Anio)),position="dodge")+
  labs(title="Serie Temporal Promedio",
       subtitle = "Latitud: 30°S",
       x="Meses",
       y="Temperatura (C°)",
       fill="Anios")+
  theme_get()



      ########  30 S #############

#reescribo fecha para poder graficar
serie_temp_40s$Fecha<-paste(serie_temp_40s$Anio,serie_temp_40s$Mes,sep="-")

#recorto y tomo solo 6 años para ver mejor
recorte_40<-subset(serie_temp_40s,serie_temp_40s$Anio>=2003 & serie_temp_40s$Anio<=2008)
recorte_40$Numero=1:length(recorte_40$Mes)
ggplot(recorte_40,aes(x=Numero,y=Promedio))+
  geom_col(aes(fill=factor(Anio)),position = "dodge")+
  labs(title="Serie Temporal Promedio",
       subtitle = "Latitud: 40°S ",
       x="Meses",
       y="Temperatura (C°)",
       fill="Anios")+
   scale_x_continuous(labels = month(recorte_40$Mes[seq(1,length(recorte_40$Numero),2)],label = T),breaks=seq(1,length(recorte_40$Numero),2))+
  theme_get()

#scale_x_continuous(labels = month(recorte_40$Mes[seq(1,length(recorte_40$Numero),2)],label = T),breaks=seq(1,length(recorte_40$Numero),2))
#PARA QUE DIGA LOS NOMBRES


############# ITEM D #############


#promedio para 40S y otro para 30S
onda_anual_40s<-aggregate(serie_temp_40s$Promedio,list(serie_temp_40s$Mes,serie_temp_40s$Latitud),mean)
onda_anual_30s<-aggregate(serie_temp_30s$Promedio,list(serie_temp_30s$Mes,serie_temp_30s$Latitud),mean)

colnames(onda_anual_40s)<-c("Mes","Latitud","Promedio")
colnames(onda_anual_30s)<-c("Mes","Latitud","Promedio")

#calculo el desvio estandar de todos los E,F,M etc del periodo
desvio_40S<-aggregate(serie_temp_40s$Promedio,list(serie_temp_40s$Mes,serie_temp_40s$Latitud),sd)
desvio_30S<-aggregate(serie_temp_30s$Promedio,list(serie_temp_30s$Mes,serie_temp_30s$Latitud),sd)

#Agrego columnas con la suma y la resta del promedio con el desvio
onda_anual_30s$Resta_Desvio<-(onda_anual_30s$Promedio-desvio_30S$Desvio)
onda_anual_30s$Suma_Desvio<-(onda_anual_30s$Promedio+desvio_30S$Desvio)

onda_anual_40s$Resta_Desvio<-(onda_anual_40s$Promedio-desvio_40S$Desvio)
onda_anual_40s$Suma_Desvio<-(onda_anual_40s$Promedio+desvio_40S$Desvio)

#nombro las columnas
colnames(desvio_40S)<-c("Mes","Latitud","Desvio")
colnames(desvio_30S)<-c("Mes","Latitud","Desvio")



ggplot(onda_anual_30s,aes(x=Mes,y=(Promedio)))+
  geom_line(color="cyan3",size=2)+
  geom_point(color="cornflowerblue",size=5,alpha=0.9)+
  geom_ribbon(onda_anual_30s,mapping=aes(ymin=Resta_Desvio,ymax=Suma_Desvio),color="#bee8f9",fill="#89d6f5",alpha=0.4)+
  labs(title="Onda Anual",
       subtitle = "Latitud: 30°S
Promedio de temperatura",
       x="Meses",
       y="Temperatura(C°)")+
  theme_get()+
  scale_x_continuous(breaks = c(1:12))



ggplot(onda_anual_40s,aes(x=Mes,y=Promedio))+
  geom_line(color="cyan3",size=2)+
  geom_point(color="cornflowerblue",size=5,alpha=0.9)+
  geom_ribbon(onda_anual_40s,mapping=aes(ymin=Resta_Desvio,ymax=Suma_Desvio),color="#bee8f9",fill="#89d6f5",alpha=0.4)+
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
       subtitle = "Latitud: 30?S",
       x="Meses",
       fill="Anio")+xlim(0,2030)+ylim(min(serie_temp_30s$Promedio),max(serie_temp_30s$Promedio))+theme_linedraw()+ theme(legend.position = "none")
g<-g+ scale_x_continuous(labels=serie_temp_30s$Fecha[seq(0, 2030, 50)],breaks = seq(0, 2030, 50))
g
