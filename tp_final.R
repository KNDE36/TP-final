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
sst_mar_argentino<-ReadNetCDF(archivo,vars = "sst",subset = list(lat=c(-60,-20),lon=c(290,320)))


require(ggplot2)
#datos del mapa global
mapa<-map_data("world2")
ggplot(sst_mar_argentino,aes(x=lon, y=lat)) +
  geom_path(mapa,mapping=aes(group=group,fill=NULL),colour ="black")+ coord_quickmap("orthographic",orientation= c(-60,320,0))+
  labs(title = "Region Mar Argentino y cercanias", x="Longitud",y="Latitud")
###CHEQUEAR DESPUES


############### ITEM B ##############
require(lubridate)

sst_mar_argentino$mes<-month(sst_mar_argentino$time)
sst_mar_argentino$año<-year(sst_mar_argentino$time)

climatologia<-aggregate(sst_mar_argentino$sst,list(sst_mar_argentino$mes,sst_mar_argentino$lat,sst_mar_argentino$lon),mean) 
#a los datos de sst, le aplico la funcion mean, para cada mes lat y lon y me devuelve un nuevo data frame

colnames(climatologia)<-c("Mes","Latitud","Longitud","Climatologia Mensual")
#nombro a las columnas con las variables
