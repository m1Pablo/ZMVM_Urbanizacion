setwd("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion") #Working directory


###############################################################################

# Cargar librerias que se van a utilizar

library("leaflet") #Paquete para usar cosas de mapas
library("rgdal") #Paquete para leer archivos shapefiles .shp
library("dplyr") #Paquete para filtrar datos de dataframes
library("fontawesome") #Paquete para íconos de marcadores
library("htmlwidgets") #Paquete para salvar mapa en html
library("leaflet.extras") #Paquete para poder buscar en los mapas
library("sf")
library("raster")
library("geojsonsf")
library("tidyverse")
library("stringr")

###############################################################################

distritos <- as.character(100:207)

for (distrito in distritos){

  distrito_str <- distrito
  distrito_int <- as.numeric(distrito_str)

  ubicacion_dist <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/DistritosEODHogaresZMVM2017/DistritosEODHogaresZMVM2017.shp") #Ubicación del archivo shapefile
  dist <- ubicacion_dist %>% readOGR(layer = paste0("DistritosEODHogaresZMVM2017"), verbose = FALSE, GDAL1_integer64_policy=TRUE) #Leer shapefile
  dist <- dist %>% spTransform(CRS("+proj=longlat +ellps=WGS84 +no_defs"))
  dist <- subset(dist, Distrito==distrito_str)

  ubicacion_zh_mex <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/igecemTipologiaahA2015Cg/igecemTipologiaahA2015Cg.shp") #Ubicación del archivo shapefile 
  zh_mex <- ubicacion_zh_mex %>% readOGR(layer = paste0("igecemTipologiaahA2015Cg"), verbose = FALSE, GDAL1_integer64_policy=TRUE) #Leer shapefile
  zh_mex <- zh_mex %>% spTransform(CRS("+proj=longlat +ellps=WGS84 +no_defs"))

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

  mex_data <- subset(datos, state_abbr=="MEX")
  zh_mex <- subset(zh_mex,zh_mex$cveinegi %in% mex_data$region)

  zh_mex_sf <- st_as_sf(zh_mex)
  zh_mex_cent <- st_centroid(zh_mex_sf)

  pb <- txtProgressBar(min = 0, max = nrow(zh_mex_cent), style = 3)
  for (i in 1:nrow(zh_mex_cent)){
    if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(zh_mex_cent)){
    setTxtProgressBar(pb, i)
    }
    point <- SpatialPoints(cbind(as.numeric(unlist(zh_mex_cent$geometry[i]))[1],as.numeric(unlist(zh_mex_cent$geometry[i]))[2]),proj4string=CRS(as.character("+proj=longlat +ellps=WGS84 +no_defs")))
    zh_mex$distrito[i]<-as.character(unlist(over(point,dist,returnList = FALSE)$Distrito))
    zh_mex$area[i]<-area(zh_mex[i,1])
  }

  zh_dist <- subset(zh_mex,zh_mex$distrito==distrito_str)

  write.csv(zh_mex@data,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/zh_mex.csv"), row.names = FALSE)

  map <- leaflet() #Crear mapa en blanco

  grupos <- c()

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

  ###############################################################################
  #Residencia

  habitacional_dist <- subset(zh_dist,zh_dist$tipologia=="Habitacional")
  grupo="Residencia"
  if(nrow(habitacional_dist@data)>0){
    map <- map %>%

        addPolygons(data= habitacional_dist,
                    stroke = TRUE, 
                    color="#105166",
                    smoothFactor = 0.5,
                    weight = 0.3,
                    fillOpacity = 0.6, 
                    fillColor = "#26b8e8",
                    group = grupo,
                    popup = paste(
                        '<br><b>Gid: </b>', paste(habitacional_dist@data$X__gid),
                        '<br><br><b>Uso de suelo: </b>', paste(habitacional_dist@data$tipologia)
                    ))
    if(!(grupo %in% grupos)){
        grupos <- grupos %>% c(grupo)
    }
  }

  ###############################################################################
  #Trabajo

  trabajo_dist <- subset(zh_dist,zh_dist$tipologia %in% c("Comercial","Industrial","Equipamiento"))
  grupo="Trabajo"
  if(nrow(trabajo_dist@data)>0){
    map <- map %>%

        addPolygons(data= trabajo_dist,
                    stroke = TRUE, 
                    color="#170f42",
                    smoothFactor = 0.5,
                    weight = 0.3,
                    fillOpacity = 0.6, 
                    fillColor = "#3c29ab",
                    group = grupo,
                    popup = paste(
                        '<br><b>Gid: </b>', paste(trabajo_dist@data$X__gid),
                        '<br><br><b>Uso de suelo: </b>', paste(trabajo_dist@data$tipologia)
                    ))
    if(!(grupo %in% grupos)){
        grupos <- grupos %>% c(grupo)
    }
  }

  map <- addTiles(map)


  setwd(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Mapas_distritos")) #Working directory: cambiar según computadora

  saveWidget(map, file=paste0(distrito_str,"_map.html"))
}
