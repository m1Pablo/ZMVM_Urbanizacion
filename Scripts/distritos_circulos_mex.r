setwd("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion") 

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
library('geojsonio')
library("spdplyr")
library("tidyverse")
library("DBI")
library('RPostgres')

###############################################################################
# SQl database

db <- 'ZMVM_Urbanizacion'
host_db <- 'localhost'  
db_port <- '5432'
db_user <- 'postgres' 
db_key <- 'ouKv5TM6uG7'
con <- dbConnect(Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_key)

###############################################################################
distritos<-c(100:207)

for(i in 1:length(distritos)){
    if(str_length(as.character(distritos[i]))==1){
        distritos[i]<-paste0("00",distritos[i])
    }
    if(str_length(as.character(distritos[i]))==2){
        distritos[i]<-paste0("0",distritos[i])
    }
}

###############################################################################
# Get data, create empty map and groups list

for(distrito_str in distritos){
    
    map <- leaflet()
    grupos<-c()
    
    distrito_int<-as.numeric(distrito_str)

    #Distritos

    ubicacion_dist <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/DistritosEODHogaresZMVM2017/DistritosEODHogaresZMVM2017.shp") 
    dist <- readOGR(ubicacion_dist, layer = paste0("DistritosEODHogaresZMVM2017"), verbose = FALSE, GDAL1_integer64_policy=TRUE) 
    dist <- spTransform(dist,CRS("+proj=longlat +ellps=WGS84 +no_defs"))
    dist <- subset(dist, dist$Distrito %in% distrito_str)

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
        
    area_dist <-area(dist)
    radius_dist <- sqrt(area_dist/pi)
    point_dist <- dist %>% geojson_json() %>% geojson_sf() %>% st_centroid()

    trabajo_data <- subset(zh_dist@data, tipologia %in% c("Industrial","Comercial","Equipamiento"))
    area_trabajo <- sum(trabajo_data$area)
    radius_trabajo <- sqrt(area_trabajo/pi)

    residencia_data <- subset(zh_dist@data, tipologia=="Habitacional")
    area_residencia <- sum(residencia_data$area)
    radius_residencia <- sqrt((area_residencia+area_trabajo)/pi)
   
    ###############################################################################
    #distrito

    map <- map %>%

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
    #sin zonificación

    grupo="Sin Zonificación"

    map <- map %>% 

        addCircles(lng=unlist(point_dist$geometry)[1],
                lat=unlist(point_dist$geometry)[2],
                radius=radius_dist,
                stroke = TRUE,
                color='black',
                opacity=1,
                weight = 4,
                fillColor = "#26b8e8",
                fillOpacity = 0,
                popup = paste(
                    '<br><b>Clave distrito: </b>', paste(grupo),
                    '<br><br><b>Uso de suelo: </b>', paste0(round(((area_dist-(area_residencia+area_trabajo))/area_dist)*100,2),'%')
                ))


    ###############################################################################
    #residencia

    grupo="Residencia"

    if(nrow(residencia_data)>0){
    
        map <- map %>% 

        addCircles(lng=unlist(point_dist$geometry)[1],
                lat=unlist(point_dist$geometry)[2],
                radius=radius_residencia,
                stroke = TRUE,
                color="#105166",
                weight = 4,
                fillColor = "#26b8e8",
                group = grupo,
                fillOpacity = 0.7,
                popup = paste(
                    '<br><b>Uso de suelo: </b>', paste(grupo),
                    '<br><br><b>Uso de suelo: </b>', paste0(round((area_residencia/area_dist)*100,2),'%')
                ))

        if(!(grupo %in% grupos)){
            grupos<-c(grupos,grupo)
        }
    }

    ###############################################################################
    #Trabajo

    grupo="Trabajo"

    if(nrow(trabajo_data)>0){
    
        map <- map %>% 

        addCircles(lng=unlist(point_dist$geometry)[1],
                lat=unlist(point_dist$geometry)[2],
                radius=radius_trabajo,
                stroke = TRUE,
                color="#170f42",
                weight = 4,
                fillColor = "#3c29ab",
                group = grupo,
                fillOpacity = 1,
                popup = paste(
                    '<br><b>Uso de suelo: </b>', paste(grupo),
                    '<br><br><b>Uso de suelo: </b>', paste0(round((area_trabajo/area_dist)*100,2),'%')
                ))

        if(!(grupo %in% grupos)){
            grupos<-c(grupos,grupo)
        }
    }
    ###############################################################################

    map <- addTiles(map)
    map <- map %>% addResetMapButton() 

    ###############################################################################
    setwd(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Mapas_distritos/")) 
    saveWidget(map, file=paste0(distrito_str,"_circle_map.html"))
}

