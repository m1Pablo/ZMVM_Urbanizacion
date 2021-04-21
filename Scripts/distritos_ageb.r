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

ubicacion_dist <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/DistritosEODHogaresZMVM2017/DistritosEODHogaresZMVM2017.shp") #Ubicación del archivo shapefile
dist <- ubicacion_dist %>% readOGR(layer = paste0("DistritosEODHogaresZMVM2017"), verbose = FALSE, GDAL1_integer64_policy=TRUE) #Leer shapefile
dist <- dist %>% spTransform(CRS("+proj=longlat +ellps=WGS84 +no_defs"))

ubicacion_ageb_u_cdmx <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/df/df_ageb_urbana.shp") #Ubicación del archivo shapefile 
ageb_u_cdmx <- ubicacion_ageb_u_cdmx %>% readOGR(layer = paste0("df_ageb_urbana"), verbose = FALSE, GDAL1_integer64_policy=TRUE) #Leer shapefile
ageb_u_cdmx <- ageb_u_cdmx %>% spTransform(CRS("+proj=longlat +ellps=WGS84 +no_defs"))

ubicacion_ageb_r_cdmx <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/df/df_ageb_rural.shp") #Ubicación del archivo shapefile 
ageb_r_cdmx <- ubicacion_ageb_r_cdmx %>% readOGR(layer = paste0("df_ageb_rural"), verbose = FALSE, GDAL1_integer64_policy=TRUE) #Leer shapefile
ageb_r_cdmx <- ageb_r_cdmx %>% spTransform(CRS("+proj=longlat +ellps=WGS84 +no_defs"))

ubicacion_manzana_cdmx <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/df/df_manzana.shp") #Ubicación del archivo shapefile 
manzana_cdmx <- ubicacion_manzana_cdmx %>% readOGR(layer = paste0("df_manzana"), verbose = FALSE, GDAL1_integer64_policy=TRUE) #Leer shapefile
manzana_cdmx <- manzana_cdmx %>% spTransform(CRS("+proj=longlat +ellps=WGS84 +no_defs"))

ageb_cdmx_u_sf <- st_as_sf(ageb_u_cdmx)
ageb_cdmx_u_cent <- st_centroid(ageb_cdmx_u_sf)

ageb_cdmx_r_sf <- st_as_sf(ageb_r_cdmx)
ageb_cdmx_r_cent <- st_centroid(ageb_cdmx_r_sf)


testpoints_ageb_u_cdmx <- matrix(nrow = nrow(ageb_cdmx_u_cent), ncol = 3) #Iniciar lista vacia para determinar la Clave INEGI de cada Oxxo
testpoints_ageb_r_cdmx <- matrix(nrow = nrow(ageb_cdmx_r_cent), ncol = 3) #Iniciar lista vacia para determinar la Clave INEGI de cada Oxxo


pb <- txtProgressBar(min = 0, max = nrow(ageb_cdmx_u_cent), style = 3)
for (i in 1:nrow(ageb_cdmx_u_cent)){
  if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(ageb_cdmx_u_cent)){
  setTxtProgressBar(pb, i)
  }
  point <- SpatialPoints(cbind(as.numeric(unlist(ageb_cdmx_u_cent$geometry[i]))[1],as.numeric(unlist(ageb_cdmx_u_cent$geometry[i]))[2]),proj4string=CRS(as.character("+proj=longlat +ellps=WGS84 +no_defs")))
  testpoints_ageb_u_cdmx[i,] <- cbind(as.character(unlist(ageb_cdmx_u_cent$CVEGEO[i]))[1],as.character(unlist(over(point,dist,returnList = FALSE)$Distrito)),"urbana")
}

pb <- txtProgressBar(min = 0, max = nrow(ageb_cdmx_r_cent), style = 3)
for (i in 1:nrow(ageb_cdmx_r_cent)){
  if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(ageb_cdmx_r_cent)){
  setTxtProgressBar(pb, i)
  }
  point <- SpatialPoints(cbind(as.numeric(unlist(ageb_cdmx_r_cent$geometry[i]))[1],as.numeric(unlist(ageb_cdmx_r_cent$geometry[i]))[2]),proj4string=CRS(as.character("+proj=longlat +ellps=WGS84 +no_defs")))
  testpoints_ageb_r_cdmx[i,] <- cbind(as.character(unlist(ageb_cdmx_r_cent$CVEGEO[i]))[1],as.character(unlist(over(point,dist,returnList = FALSE)$Distrito)),"rural")
}

map <- leaflet() #Crear mapa en blanco

k <- 0
  ###############################################################################

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

map <- map %>% #Agregar al mapa shapefile con datos del nuevo municipio

    addPolygons(data= ageb_u_cdmx,
                stroke = TRUE,
                color = 'blue',
                opacity=1, 
                smoothFactor = 0.5,
                weight = 7,
                fillColor = "#2062e6",
                fillOpacity = 0.1,
                popup = paste(
                    '<br><b>Clave municipio/alcaldía: </b>', paste(ageb_u_cdmx$CVEGEO)
                ))

map <- map %>% #Agregar al mapa shapefile con datos del nuevo municipio

    addPolygons(data= ageb_r_cdmx,
                stroke = TRUE,
                color = 'red',
                opacity=1, 
                smoothFactor = 0.5,
                weight = 7,
                fillColor = "#2062e6",
                fillOpacity = 0.1,
                popup = paste(
                    '<br><b>Clave municipio/alcaldía: </b>', paste(ageb_r_cdmx$CVEGEO)
                ))


map <- addTiles(map)


setwd(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/")) #Working directory: cambiar según computadora

saveWidget(map, file=paste0("distritos_ageb_cdmx_map.html"))

