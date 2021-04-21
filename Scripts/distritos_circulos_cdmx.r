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
setwd(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/")) 
###############################################################################
# SQl database

db <- 'ZMVM_Urbanizacion'
host_db <- 'localhost'  
db_port <- '5432'
db_user <- 'postgres' 
db_password <- readLines('./database_key.txt')
con <- dbConnect(Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)

###############################################################################
distritos<-c(1:85)

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
    
    distrito_int<-as.numeric(distrito_str)
    query= paste0("select * from fid where distrito = ",distrito_str)
    datos<-dbGetQuery(con, query)
    map <- leaflet()
    grupos<-c()

    ###############################################################################
    #Distritos

    ubicacion_dist <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/DistritosEODHogaresZMVM2017/DistritosEODHogaresZMVM2017.shp") 
    dist <- readOGR(ubicacion_dist, layer = paste0("DistritosEODHogaresZMVM2017"), verbose = FALSE, GDAL1_integer64_policy=TRUE) 
    dist <- spTransform(dist,CRS("+proj=longlat +ellps=WGS84 +no_defs"))
    dist <- subset(dist, dist$Distrito %in% distrito_str)

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

    area_dist <-area(dist)
    radius_dist <- sqrt(area_dist/pi)
    point_dist <- dist %>% geojson_json() %>% geojson_sf() %>% st_centroid()

    trabajo_data <- subset(datos, uso_suelo %in% c("Industrial","Industrial y comercial","Equipamiento"))
    area_trabajo <- sum(trabajo_data$area)
    radius_trabajo <- sqrt(area_trabajo/pi)

    mixto_data <- subset(datos, uso_suelo=="Habitacional y comercial")
    area_mixto <- sum(mixto_data$area)
    radius_mixto <- sqrt((area_mixto+area_trabajo)/pi)

    residencia_data <- subset(datos, uso_suelo=="Habitacional")
    area_residencia <- sum(residencia_data$area)
    radius_residencia <- sqrt((area_residencia+area_mixto+area_trabajo)/pi)

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
                    '<br><br><b>Uso de suelo: </b>', paste0(round(((area_dist-(area_residencia+area_mixto+area_trabajo))/area_dist)*100,2),'%')
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
                fillOpacity = 1,
                popup = paste(
                    '<br><b>Uso de suelo: </b>', paste(grupo),
                    '<br><br><b>Uso de suelo: </b>', paste0(round((area_residencia/area_dist)*100,2),'%')
                ))

        if(!(grupo %in% grupos)){
            grupos<-c(grupos,grupo)
        }
    }

    ###############################################################################
    #residencia y Comercial
    grupo="Mixto"

    if(nrow(mixto_data)>0){

        map <- map %>% 

        addCircles(lng=unlist(point_dist$geometry)[1],
                lat=unlist(point_dist$geometry)[2],
                radius=radius_mixto,
                stroke = TRUE,
                color="#bd4913",
                weight = 4,
                fillColor = "#dea318",
                group = grupo,
                fillOpacity = 1,
                popup = paste(
                    '<br><b>Uso de suelo: </b>', paste(grupo),
                    '<br><br><b>Uso de suelo: </b>', paste0(round((area_mixto/area_dist)*100,2),'%')
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

