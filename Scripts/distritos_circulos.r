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
db_key <- readLines('./database_key.txt')
con <- dbConnect(Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_key)

###############################################################################
# Initiate map, get data, create empty map and groups list
grupo_d="Límite distrito"
grupos<-c()
map <- leaflet()

###############################################################################
#CDMX

distritos<-c(1:85)

for(i in 1:length(distritos)){
    if(str_length(as.character(distritos[i]))==1){
        distritos[i]<-paste0("00",distritos[i])
    }
    if(str_length(as.character(distritos[i]))==2){
        distritos[i]<-paste0("0",distritos[i])
    }
}

ubicacion_dist <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/DistritosEODHogaresZMVM2017/DistritosEODHogaresZMVM2017.shp") 
dist_todos <- readOGR(ubicacion_dist, layer = paste0("DistritosEODHogaresZMVM2017"), verbose = FALSE, GDAL1_integer64_policy=TRUE) 
dist_todos <- spTransform(dist_todos,CRS("+proj=longlat +ellps=WGS84 +no_defs"))


pb <- txtProgressBar(min = 0, max = length(distritos), style = 3)
i<-0
for(distrito_str in distritos){
    i<-i+1
    if (sample(1:1, 1)==sample(1:1, 1) || i==length(distritos)){
    setTxtProgressBar(pb, i)
    }
   
    distrito_int<-as.numeric(distrito_str)
    query= paste0("select * from fid where distrito = ",distrito_str)
    datos<-dbGetQuery(con, query)

    ###############################################################################
    #Distritos


    dist <- subset(dist_todos, dist_todos$Distrito %in% distrito_str)

    map <- map %>%

        addPolygons(data= dist,
                    stroke = TRUE,
                    color='grey',
                    opacity=1,
                    smoothFactor = 0.5,
                    weight = 4,
                    fillColor = "#26b8e8",
                    fillOpacity = 0,
                    group = grupo_d,
                    popup = paste(
                        '<br><b>Clave distrito: </b>', paste(dist$Distrito),
                        '<br><br><b>Estado: </b>', paste("CDMX")
                    ))

    area_dist <-area(dist)
    radius_dist <- sqrt(area_dist/pi)
    point_dist <- dist %>% geojson_json() %>% geojson_sf() %>% st_centroid()

    mixto_data <- subset(datos, uso_suelo=="Habitacional y comercial")
    area_mixto <- sum(mixto_data$area)

    trabajo_data <- subset(datos, uso_suelo %in% c("Industrial","Industrial y comercial","Equipamiento"))
    area_trabajo <- sum(trabajo_data$area)+area_mixto
    #area_trabajo <- 0
    radius_trabajo <- sqrt((area_trabajo)/pi)

    residencia_data <- subset(datos, uso_suelo=="Habitacional")
    #area_residencia <- sum(residencia_data$area)+area_mixto
    area_residencia <- 0
    radius_residencia <- sqrt((area_residencia+area_trabajo)/pi)

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
                    '<br><b>Distrito: </b>', paste(distrito_str),
                    '<br><br><b>Estado: </b>', paste("CDMX"),
                    '<br><br><b>Uso de suelo: </b>', paste(grupo),
                    '<br><br><b>Porcentaje del suelo del distrito: </b>', paste0(round(((area_dist-(area_residencia-area_mixto+area_trabajo))/area_dist)*100,2),'%')
                ))


    ###############################################################################
    #residencia

    grupo="Residencia"

    if(nrow(residencia_data)+nrow(mixto_data)>0){

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
                    '<br><b>Distrito: </b>', paste(distrito_str),
                    '<br><br><b>Estado: </b>', paste("CDMX"),
                    '<br><br><b>Uso de suelo: </b>', paste(grupo),
                    '<br><br><b>Porcentaje del suelo del distrito: </b>', paste0(round((area_residencia/area_dist)*100,2),'%')
                ))

        if(!(grupo %in% grupos)){
            grupos<-c(grupos,grupo)
        }
    }

    ###############################################################################
    #Trabajo

    grupo="Trabajo"

    if(nrow(trabajo_data)+nrow(mixto_data)>0){
    
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
                    '<br><b>Distrito: </b>', paste(distrito_str),
                    '<br><br><b>Estado: </b>', paste("CDMX"),
                    '<br><br><b>Uso de suelo: </b>', paste(grupo),
                    '<br><br><b>Porcentaje del suelo del distrito: </b>', paste0(round((area_trabajo/area_dist)*100,2),'%')
                ))

        if(!(grupo %in% grupos)){
            grupos<-c(grupos,grupo)
        }
    }
}

###############################################################################
#MEX

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


ubicacion_datos <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/ZMVM_municipios.csv") 
datos <- ubicacion_datos %>% read.csv(header = TRUE) 
datos <- datos %>% lapply(as.character) %>% as.data.frame(stringsAsFactors = FALSE)
mex_data <- subset(datos, state_abbr=="MEX")

ubicacion_dist <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/DistritosEODHogaresZMVM2017/DistritosEODHogaresZMVM2017.shp") 
dist_todos <- readOGR(ubicacion_dist, layer = paste0("DistritosEODHogaresZMVM2017"), verbose = FALSE, GDAL1_integer64_policy=TRUE) 
dist_todos <- spTransform(dist_todos,CRS("+proj=longlat +ellps=WGS84 +no_defs"))

ubicacion_zh_mex_todos <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/igecemTipologiaahA2015Cg/igecemTipologiaahA2015Cg.shp") #Ubicación del archivo shapefile 
zh_mex_todos <- ubicacion_zh_mex_todos %>% readOGR(layer = paste0("igecemTipologiaahA2015Cg"), verbose = FALSE, GDAL1_integer64_policy=TRUE) #Leer shapefile
zh_mex_todos <- zh_mex_todos %>% spTransform(CRS("+proj=longlat +ellps=WGS84 +no_defs"))
zh_mex_todos <- subset(zh_mex_todos,zh_mex_todos$cveinegi %in% mex_data$region)
zh_mex_todos_sf <- st_as_sf(zh_mex_todos)
zh_mex_todos_cent <- st_centroid(zh_mex_todos_sf)

pb <- txtProgressBar(min = 0, max = nrow(zh_mex_todos_cent), style = 3)
for (i in 1:nrow(zh_mex_todos_cent)){
    if (sample(1:40, 1)==sample(1:40, 1) || i==length(zh_mex_todos_cent)){
    setTxtProgressBar(pb, i)
    }
    point <- SpatialPoints(cbind(as.numeric(unlist(zh_mex_todos_cent$geometry[i]))[1],as.numeric(unlist(zh_mex_todos_cent$geometry[i]))[2]),proj4string=CRS(as.character("+proj=longlat +ellps=WGS84 +no_defs")))
    zh_mex_todos$distrito[i]<-as.character(unlist(over(point,dist_todos,returnList = FALSE)$Distrito))
    zh_mex_todos$area[i]<-area(zh_mex_todos[i,1])
}

pb <- txtProgressBar(min = 0, max = length(distritos), style = 3)
ii<-0
for(distrito_str in distritos){
    ii<-ii+1
    if (sample(1:1, 1)==sample(1:1, 1) || ii==length(distritos)){
    setTxtProgressBar(pb, ii)
    }
    zh_mex<-zh_mex_todos
    distrito_int<-as.numeric(distrito_str)

    #Distritos

    dist <- subset(dist_todos, dist_todos$Distrito %in% distrito_str)

    k <- 0




    zh_dist <- subset(zh_mex,zh_mex$distrito==distrito_str)
        
    area_dist <-area(dist)
    radius_dist <- sqrt(area_dist/pi)
    point_dist <- dist %>% geojson_json() %>% geojson_sf() %>% st_centroid()

    trabajo_data <- subset(zh_dist@data, tipologia %in% c("Industrial","Comercial","Equipamiento"))
    area_trabajo <- sum(trabajo_data$area)
    #area_trabajo <- 0
    radius_trabajo <- sqrt(area_trabajo/pi)

    residencia_data <- subset(zh_dist@data, tipologia=="Habitacional")
    #area_residencia <- sum(residencia_data$area)
    area_residencia <- 0
    radius_residencia <- sqrt((area_residencia+area_trabajo)/pi)

    ###############################################################################
    #distrito

    map <- map %>%

        addPolygons(data= dist,
                    stroke = TRUE,
                    color='grey',
                    opacity=1,
                    smoothFactor = 0.5,
                    weight = 4,
                    fillColor = "#26b8e8",
                    fillOpacity = 0,
                    group=grupo_d,
                    popup = paste(
                        '<br><b>Clave distrito: </b>', paste(dist$Distrito),
                        '<br><br><b>Estado: </b>', paste("CDMX")
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
                    '<br><b>Distrito: </b>', paste(distrito_str),
                    '<br><br><b>Estado: </b>', paste("MEX"),
                    '<br><br><b>Uso de suelo: </b>', paste(grupo),
                    '<br><br><b>Porcentaje del suelo del distrito: </b>', paste0(round(((area_dist-(area_residencia+area_trabajo))/area_dist)*100,2),'%')
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
                    '<br><b>Distrito: </b>', paste(distrito_str),
                    '<br><br><b>Estado: </b>', paste("MEX"),
                    '<br><br><b>Uso de suelo: </b>', paste(grupo),
                    '<br><br><b>Porcentaje del suelo del distrito: </b>', paste0(round((area_residencia/area_dist)*100,2),'%')
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
                    '<br><b>Distrito: </b>', paste(distrito_str),
                    '<br><br><b>Estado: </b>', paste("MEX"),
                    '<br><br><b>Uso de suelo: </b>', paste(grupo),
                    '<br><br><b>Porcentaje del suelo del distrito: </b>', paste0(round((area_trabajo/area_dist)*100,2),'%')
                ))

        if(!(grupo %in% grupos)){
            grupos<-c(grupos,grupo)
        }
    }
###############################################################################
}

###############################################################################

map <- addTiles(map)
map<- map %>% addLayersControl( #Agrega control sobre visulización de Oxxos en el mapa
overlayGroups = grupo_d,
options = layersControlOptions(collapsed = TRUE)
)
map <- map %>% addResetMapButton() 

###############################################################################
setwd(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Mapas_distritos/")) 
saveWidget(map, file=paste0("000_circle_work_map.html"))
