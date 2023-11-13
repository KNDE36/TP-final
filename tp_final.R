require(ncdf4)
require(udunits2)
require(metR)

archivo<-"~/Escritorio/Labo_Cande/TP_final/sst.mnmean_ERSST.nc"
GlanceNetCDF(archivo)
nc<-nc_open(archivo)
nc
datos_sst<-ReadNetCDF(archivo,vars = "sst")
#no es necesario abrirlo completo, de una ya completado esta ok


############ ITEM A ##############
#abro para quedarme solo con la region mar argentino y cercanias 

archivo<-"C:/Users/cannm/OneDrive/Documentos/sst.mnmean_ERSST.nc"
GlanceNetCDF(archivo)
sst_mar_argentino<-ReadNetCDF(archivo,vars = "sst",subset = list(lat=c(-60,-20),lon=c(290,320)))


require(ggplot2)
#datos del mapa global
mapa<-map_data("world2")
ggplot(sst_mar_argentino,aes(x=lon, y=lat)) +
  geom_path(mapa,mapping=aes(group=group,fill=NULL),colour ="black")+ coord_quickmap("orthographic",orientation= c(-60,320,0))+
  labs(title = "Region Mar Argentino y cercanias", x="Longitud",y="Latitud")
###CHEQUEAR DESPUES



##############  ITEM B ##############

require(lubridate)

sst_mar_argentino$mes<-month(sst_mar_argentino$time)

climatologia<-aggregate(sst_mar_argentino$sst,list(sst_mar_argentino$mes,sst_mar_argentino$lat,sst_mar_argentino$lon),mean) 
#a los datos de sst, le aplico la funcion mean, para cada mes lat y lon y me devuelve un nuevo data frame

colnames(climatologia)<-c("Mes","Latitud","Longitud","Climatologia.Mensual")
#nombro a las columnas con las variables



#opcion 1 pero no se como graficar en el ciclo y guardarlo, NO ES NECESARIO CREO!!!
meses<-1:12
for (i in meses) {
  mes<-meses[i]
  datos_clima_mes<-climatologia[climatologia$Mes==mes,]
}



ggplot(climatologia,aes(x=Longitud,y=Latitud))+
  geom_point(size=3,alpha=0.8)+
  geom_smooth(method = "lm",se=F,)+
  facet_wrap(~Mes)  #con los paneles safo de separar 12data frames 
##COMO PONGO QUE GRAFIQUE LOS DATOS DE CLIMATOLOGIA??

#opcion 2 creo poco eficiente
#separo en 12 data frames,CREO QUE NO ES NECESARIO!!!

clima_enero<-climatologia[climatologia$Mes==1,]
clima_feb<-climatologia[climatologia$Mes==2,]
clima_mar<-climatologia[climatologia$Mes==3,]

ggplot(clima_enero,aes(x=Longitud,y=Latitud,fill=Climatologia.Mensual))+
  geom_point() #COMO GRAFICO 3 VARIABLES???




###########  ITEM C ###################

#con 40s elijo 60W,51w,45w
#con 30s elijo 50w,46w,40w

#abro nuevamente los datos con las coordenadas seleccionadas

sst_40S<-ReadNetCDF(archivo,vars = "sst",subset = list(lat=-40,lon=c(360-60,360-51,360-45)))
sst_40S$mes<-month(sst_40S$time)

sst_30S<-ReadNetCDF(archivo,vars = "sst",subset = list(lat=-30,lon=c(360-50,360-40,360-46)))
sst_30S$mes<-month(sst_30S$time)


#promedio para 40°S y otro para 30°S
serie_temp_40s<-aggregate(sst_40S$sst,list(sst_40S$mes,sst_40S$lat,sst_40S$lon),mean)
serie_temp_30s<-aggregate(sst_30S$sst,list(sst_30S$mes,sst_30S$lat,sst_30S$lon),mean)

colnames(serie_temp_40s)<-c("Mes","Latitud","Longitud","Promedio")
colnames(serie_temp_30s)<-c("Mes","Latitud","Longitud","Promedio")


ggplot(serie_temp_30s,aes(x=Mes,y=Promedio,color=Longitud))+
  geom_point(aes(color=Longitud),size=4,alpha=0.5)+
  scale_fill_continuous(aes("Longitudes"))+
  labs(title="Serie Temporal Promedio de la latitud 30°S",
       x="Meses")+
  theme_get()

#como se arregla los meses en el grafico??
 


ggplot(serie_temp_40s,aes(x=Mes,y=Promedio,color=Longitud))+
  geom_point(aes(color=Longitud),size=4,alpha=0.5)+
  scale_color_continuous(aes("Longitudes"))+
  labs(title="Serie Temporal Promedio de la latitud 40°S",
       x="Meses")+
  theme_get()



############# ITEM D #############

#que diferencia hay con la serie temporal??


############# ITEM E #############

serie_30S_ordenada<-serie_temp_30s[order(serie_temp_30s$Promedio,decreasing=T),]
serie_40S_ordenada<-serie_temp_40s[order(serie_temp_40s$Promedio),]

datos_30s<-data.frame(Anio=) #creo que la serie temporal promedio es la onda anual xq sino aca no tengo los años