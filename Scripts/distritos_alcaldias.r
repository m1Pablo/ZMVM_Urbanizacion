setwd("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion") #Working directory


###############################################################################

# Cargar librerias que se van a utilizar

library("leaflet") #Paquete para usar cosas de mapas
library("rgdal") #Paquete para leer archivos shapefiles .shp
library("dplyr") #Paquete para filtrar datos de dataframes
library("fontawesome") #Paquete para íconos de marcadores
library("htmlwidgets") #Paquete para salvar mapa en html
library("leaflet.extras") #Paquete para poder buscar en los mapas
library("sf") #Paquete de herramientas para mapas
library("tidyr")

###############################################################################


ubicacion_alc_cdmx <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/df/df_municipio.shp") #Ubicación del archivo shapefile 
alc_cdmx <- ubicacion_alc_cdmx %>% readOGR(layer = paste0("df_municipio"), verbose = FALSE, GDAL1_integer64_policy=TRUE) #Leer shapefile
alc_cdmx <- alc_cdmx %>% spTransform(CRS("+proj=longlat +ellps=WGS84 +no_defs"))

ubicacion_alc_mex <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/mex/mex_municipio.shp") #Ubicación del archivo shapefile 
alc_mex <- ubicacion_alc_mex %>% readOGR(layer = paste0("mex_municipio"), verbose = FALSE, GDAL1_integer64_policy=TRUE) #Leer shapefile
alc_mex <- alc_mex %>% spTransform(CRS("+proj=longlat +ellps=WGS84 +no_defs"))

ubicacion_alc_hgo <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/hgo/hgo_municipio.shp") #Ubicación del archivo shapefile 
alc_hgo <- ubicacion_alc_hgo %>% readOGR(layer = paste0("hgo_municipio"), verbose = FALSE, GDAL1_integer64_policy=TRUE) #Leer shapefile
alc_hgo <- alc_hgo %>% spTransform(CRS("+proj=longlat +ellps=WGS84 +no_defs"))

ubicacion_dist <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/DistritosEODHogaresZMVM2017/DistritosEODHogaresZMVM2017.shp") #Ubicación del archivo shapefile
dist <- ubicacion_dist %>% readOGR(layer = paste0("DistritosEODHogaresZMVM2017"), verbose = FALSE, GDAL1_integer64_policy=TRUE) #Leer shapefile
dist <- dist %>% spTransform(CRS("+proj=longlat +ellps=WGS84 +no_defs"))

dist_sf <- st_as_sf(dist)
dist_cent <- st_centroid(dist_sf)

testpoints_dist_cdmx <- matrix(nrow = nrow(dist_cent), ncol = 2) #Iniciar lista vacia para determinar la Clave INEGI de cada Oxxo
testpoints_dist_mex <- matrix(nrow = nrow(dist_cent), ncol = 2) #Iniciar lista vacia para determinar la Clave INEGI de cada Oxxo
testpoints_dist_hgo <- matrix(nrow = nrow(dist_cent), ncol = 2) #Iniciar lista vacia para determinar la Clave INEGI de cada Oxxo

for (i in 1:nrow(dist_cent)){
  point <- SpatialPoints(cbind(as.numeric(unlist(dist_cent$geometry[i]))[1],as.numeric(unlist(dist_cent$geometry[i]))[2]),proj4string=CRS(as.character("+proj=longlat +ellps=WGS84 +no_defs")))
  testpoints_dist_cdmx[i,] <- cbind(as.numeric(unlist(dist_cent$Distrito[i]))[1],as.character(unlist(over(point,alc_cdmx,returnList = FALSE)$CVEGEO)))
  testpoints_dist_mex[i,] <- cbind(as.numeric(unlist(dist_cent$Distrito[i]))[1],as.character(unlist(over(point,alc_mex,returnList = FALSE)$CVEGEO)))
  testpoints_dist_hgo[i,] <- cbind(as.numeric(unlist(dist_cent$Distrito[i]))[1],as.character(unlist(over(point,alc_hgo,returnList = FALSE)$CVEGEO)))
}

ubicacion_datos <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/ZMVM_municipios.csv") 
datos <- ubicacion_datos %>% read.csv(header = TRUE) 
datos <- datos %>% lapply(as.character) %>% as.data.frame(stringsAsFactors = FALSE)

k <- 0

for (codigo in datos$region){
  
  k <- k+1
  # Escoger código postal, salvar version con prefijo de cero, y estado al que corresponde
  clave_alc <- as.character(codigo)
  # Salvar version de código postal con prefijo de cero
  
  try(if (nchar(as.character(clave_alc))==4){
    datos$region[k] <-paste0("0",clave_alc)
  } else if  (nchar(as.character(clave_alc))==3){
    datos$region[k] <-paste0("00",clave_alc)
  } else if (nchar(as.character(clave_alc))==2){
    datos$region[k] <-paste0("000",clave_alc)
  } else {
    datos$region[k] <-paste0(clave_alc)
  })}


cdmx_data <- subset(datos, state_abbr=="CDMX") 
cdmx <- subset(alc_cdmx,alc_cdmx$CVEGEO %in% cdmx_data$region)
mex_data <- subset(datos, state_abbr=="MEX")
mex <- subset(alc_mex,alc_mex$CVEGEO %in% mex_data$region)
hgo_data <- subset(datos, state_abbr=="HGO")
hgo <- subset(alc_hgo,alc_hgo$CVEGEO %in% hgo_data$region)

map <- leaflet() #Crear mapa en blanco

k <- 0
  ###############################################################################

map <- map %>% #Agregar al mapa shapefile con datos del nuevo municipio

    addPolygons(data= cdmx,
                stroke = TRUE,
                color = 'blue',
                opacity=1, 
                smoothFactor = 0.5,
                weight = 7,
                fillColor = "#2062e6",
                fillOpacity = 0.1,
                popup = paste(
                    '<br><b>Clave municipio/alcaldía: </b>', paste(cdmx$CVEGEO)
                ))

map <- map %>% #Agregar al mapa shapefile con datos del nuevo municipio

    addPolygons(data= mex,
                stroke = TRUE,
                color = 'red',
                opacity=1, 
                smoothFactor = 0.5,
                weight = 7,
                fillColor = "#e8133a",
                fillOpacity = 0.1,
                popup = paste(
                    '<br><b>Clave municipio/alcaldía: </b>', paste(mex$CVEGEO)
                ))

map <- map %>% #Agregar al mapa shapefile con datos del nuevo municipio

    addPolygons(data= hgo,
                stroke = TRUE,
                color = 'green', 
                opacity=1,
                smoothFactor = 0.5,
                weight = 7, 
                fillColor = "#0da353",
                fillOpacity = 0.1,
                popup = paste(
                    '<br><b>Clave municipio/alcaldía: </b>', paste(hgo$CVEGEO)
                ))

map <- map %>% #Agregar al mapa shapefile con datos del nuevo municipio

    addPolygons(data= dist,
                stroke = TRUE,
                color='black',
                opacity=1,
                smoothFactor = 0.5,
                weight = 4,
                fillColor = "#26b8e8",
                fillOpacity = 0,
                popup = paste(
                    '<br><b>Clave distrito: </b>', paste(dist$Distrito)
                ))

map <- addTiles(map)

map <- map %>% #Agregar al mapa shapefile con datos del nuevo municipio

    addAwesomeMarkers(dist_cent$geometry)

map <- addTiles(map)

setwd(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/")) #Working directory: cambiar según computadora

saveWidget(map, file=paste0("distritos_alcaldias_map.html"))

