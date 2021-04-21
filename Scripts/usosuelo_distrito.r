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
library("tidyverse")
library("DBI")
library('RPostgres')

###############################################################################
# SQl database

db <- 'ZMVM_Urbanizacion'
host_db <- 'localhost'  
db_port <- '5432'
db_user <- 'postgres' 
db_password <- 'ouKv5TM6uG7'
con <- dbConnect(Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)

###############################################################################
# Get data, create empty map and groups list

distrito_str <- "002"
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

###############################################################################
#Habitacional

habitacional_data <- subset(datos, uso_suelo=="Habitacional")

grupo="Residencia"
if(nrow(habitacional_data)>0){
    pb <- txtProgressBar(min = 0, max = nrow(habitacional_data), style = 3)
    for (i in 1:nrow(habitacional_data)){
        if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(habitacional_data)){
            setTxtProgressBar(pb, i)
        }
        habitacional_data$geo_shape[i]<-str_replace(habitacional_data$geo_shape[i],"type",'"type"')
        habitacional_data$geo_shape[i]<-str_replace(habitacional_data$geo_shape[i],"MultiPolygon",'"MultiPolygon"')
        habitacional_data$geo_shape[i]<-str_replace(habitacional_data$geo_shape[i],"coordinates",'"coordinates"')
        }
    polygons <-  geojson_sf(habitacional_data$geo_shape)
    map <- map %>%

        addPolygons(data= polygons,
                    stroke = TRUE, 
                    color="#105166",
                    smoothFactor = 0.5,
                    weight = 0.3,
                    fillOpacity = 0.6, 
                    fillColor = "#26b8e8",
                    group = grupo,
                    popup = paste(
                        '<br><b>Fid: </b>', paste(habitacional_data$fid),
                        '<br><br><b>Uso de suelo: </b>', paste(habitacional_data$uso_suelo)
                    ))
    if(!(grupo %in% grupos)){
        grupos<-c(grupos,grupo)
    }
}
###############################################################################
#Habitacional y Comercial

habitacional_comercial_data <- subset(datos, uso_suelo=="Habitacional y comercial")
grupo="Mixto"
if(nrow(habitacional_comercial_data)>0){
    pb <- txtProgressBar(min = 0, max = nrow(habitacional_comercial_data), style = 3)
    for (i in 1:nrow(habitacional_comercial_data)){
        if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(habitacional_comercial_data)){
            setTxtProgressBar(pb, i)
        }
        habitacional_comercial_data$geo_shape[i]<-str_replace(habitacional_comercial_data$geo_shape[i],"type",'"type"')
        habitacional_comercial_data$geo_shape[i]<-str_replace(habitacional_comercial_data$geo_shape[i],"MultiPolygon",'"MultiPolygon"')
        habitacional_comercial_data$geo_shape[i]<-str_replace(habitacional_comercial_data$geo_shape[i],"coordinates",'"coordinates"')
        }
    polygons <-  geojson_sf(habitacional_comercial_data$geo_shape)
    map <- map %>%

        addPolygons(data= polygons,
                    stroke = TRUE, 
                    color="#bd4913",
                    smoothFactor = 0.5,
                    weight = 0.3,
                    fillOpacity = 0.6, 
                    fillColor = "#dea318",
                    group = grupo,
                    popup = paste(
                        '<br><b>Fid: </b>', paste(habitacional_comercial_data$fid),
                        '<br><br><b>Uso de suelo: </b>', paste(habitacional_comercial_data$uso_suelo)
                    ))
    if(!(grupo %in% grupos)){
        grupos<-c(grupos,grupo)
    }
}
###############################################################################
#Industrial

industrial_data <- subset(datos, uso_suelo=="Industrial")
grupo="Trabajo"
if(nrow(industrial_data)>0){
    pb <- txtProgressBar(min = 0, max = nrow(industrial_data), style = 3)
    for (i in 1:nrow(industrial_data)){
        if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(industrial_data)){
            setTxtProgressBar(pb, i)
        }
        industrial_data$geo_shape[i]<-str_replace(industrial_data$geo_shape[i],"type",'"type"')
        industrial_data$geo_shape[i]<-str_replace(industrial_data$geo_shape[i],"MultiPolygon",'"MultiPolygon"')
        industrial_data$geo_shape[i]<-str_replace(industrial_data$geo_shape[i],"coordinates",'"coordinates"')
        }
    polygons <-  geojson_sf(industrial_data$geo_shape)
    map <- map %>%

        addPolygons(data= polygons,
                    stroke = TRUE, 
                    color="#170f42",
                    smoothFactor = 0.5,
                    weight = 0.3,
                    fillOpacity = 0.6, 
                    fillColor = "#3c29ab",
                    group = grupo,
                    popup = paste(
                        '<br><b>Fid: </b>', paste(industrial_data$fid),
                        '<br><br><b>Uso de suelo: </b>', paste(industrial_data$uso_suelo)
                    ))
    if(!(grupo %in% grupos)){
        grupos<-c(grupos,grupo)
    }
}

###############################################################################
#Industrial y comercial

industrial_comercial_data <- subset(datos, uso_suelo=="Industrial y comercial")
grupo="Trabajo"
if(nrow(industrial_comercial_data)>0){
    pb <- txtProgressBar(min = 0, max = nrow(industrial_comercial_data), style = 3)
    for (i in 1:nrow(industrial_comercial_data)){
        if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(industrial_comercial_data)){
            setTxtProgressBar(pb, i)
        }
        industrial_comercial_data$geo_shape[i]<-str_replace(industrial_comercial_data$geo_shape[i],"type",'"type"')
        industrial_comercial_data$geo_shape[i]<-str_replace(industrial_comercial_data$geo_shape[i],"MultiPolygon",'"MultiPolygon"')
        industrial_comercial_data$geo_shape[i]<-str_replace(industrial_comercial_data$geo_shape[i],"coordinates",'"coordinates"')
        }
    polygons <-  geojson_sf(industrial_comercial_data$geo_shape)
    map <- map %>%

        addPolygons(data= polygons,
                    stroke = TRUE, 
                    color="#170f42",
                    smoothFactor = 0.5,
                    weight = 0.3,
                    fillOpacity = 0.6, 
                    fillColor = "#3c29ab",
                    group = grupo,
                    popup = paste(
                        '<br><b>Fid: </b>', paste(industrial_comercial_data$fid),
                        '<br><br><b>Uso de suelo: </b>', paste(industrial_comercial_data$uso_suelo)
                    ))
    if(!(grupo %in% grupos)){
        grupos<-c(grupos,grupo)
    }
}
###############################################################################
#Equipamientos

equipamiento_data <- subset(datos, uso_suelo=="Equipamiento")
grupo="Trabajo"
if(nrow(equipamiento_data)>0){
pb <- txtProgressBar(min = 0, max = nrow(equipamiento_data), style = 3)
    for (i in 1:nrow(equipamiento_data)){
        if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(equipamiento_data)){
            setTxtProgressBar(pb, i)
        }
        equipamiento_data$geo_shape[i]<-str_replace(equipamiento_data$geo_shape[i],"type",'"type"')
        equipamiento_data$geo_shape[i]<-str_replace(equipamiento_data$geo_shape[i],"MultiPolygon",'"MultiPolygon"')
        equipamiento_data$geo_shape[i]<-str_replace(equipamiento_data$geo_shape[i],"coordinates",'"coordinates"')
        }
    polygons <-  geojson_sf(equipamiento_data$geo_shape)
    map <- map %>%

        addPolygons(data= polygons,
                    stroke = TRUE, 
                    color="#170f42",
                    smoothFactor = 0.5,
                    weight = 0.3,
                    fillOpacity = 0.6, 
                    fillColor = "#3c29ab",
                    group = grupo,
                    popup = paste(
                        '<br><b>Fid: </b>', paste(equipamiento_data$fid),
                        '<br><br><b>Uso de suelo: </b>', paste(equipamiento_data$uso_suelo)
                    ))
    if(!(grupo %in% grupos)){
        grupos<-c(grupos,grupo)
    }
}
###############################################################################
#Áreas verdes

areas_verdes_data <- subset(datos, uso_suelo=="Áreas Verdes")
grupo="Áreas verdes/Centros de Barrio"
if(nrow(areas_verdes_data)>0){
    pb <- txtProgressBar(min = 0, max = nrow(areas_verdes_data), style = 3)
    for (i in 1:nrow(areas_verdes_data)){
        if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(areas_verdes_data)){
            setTxtProgressBar(pb, i)
        }
        areas_verdes_data$geo_shape[i]<-str_replace(areas_verdes_data$geo_shape[i],"type",'"type"')
        areas_verdes_data$geo_shape[i]<-str_replace(areas_verdes_data$geo_shape[i],"MultiPolygon",'"MultiPolygon"')
        areas_verdes_data$geo_shape[i]<-str_replace(areas_verdes_data$geo_shape[i],"coordinates",'"coordinates"')
        }
    polygons <-  geojson_sf(areas_verdes_data$geo_shape)
    map <- map %>%

        addPolygons(data= polygons,
                    stroke = TRUE, 
                    color="#044d0e",
                    smoothFactor = 0.5,
                    weight = 0.3,
                    fillOpacity = 0.6, 
                    fillColor = "#07911a",
                    group = grupo,
                    popup = paste(
                        '<br><b>Fid: </b>', paste(areas_verdes_data$fid),
                        '<br><br><b>Uso de suelo: </b>', paste(areas_verdes_data$uso_suelo)
                    ))
    if(!(grupo %in% grupos)){
        grupos<-c(grupos,grupo)
    }
}
###############################################################################
#Centros de barrio

centros_barrio_data <- subset(datos, uso_suelo=="Centro de Barrio")
grupo="Áreas verdes/Centros de Barrio"
if(nrow(centros_barrio_data)>0){
    pb <- txtProgressBar(min = 0, max = nrow(centros_barrio_data), style = 3)
    for (i in 1:nrow(centros_barrio_data)){
        if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(centros_barrio_data)){
            setTxtProgressBar(pb, i)
        }
        centros_barrio_data$geo_shape[i]<-str_replace(centros_barrio_data$geo_shape[i],"type",'"type"')
        centros_barrio_data$geo_shape[i]<-str_replace(centros_barrio_data$geo_shape[i],"MultiPolygon",'"MultiPolygon"')
        centros_barrio_data$geo_shape[i]<-str_replace(centros_barrio_data$geo_shape[i],"coordinates",'"coordinates"')
        }
    polygons <-  geojson_sf(centros_barrio_data$geo_shape)
    map <- map %>%

        addPolygons(data= polygons,
                    stroke = TRUE, 
                    color="#044d0e",
                    smoothFactor = 0.5,
                    weight = 0.3,
                    fillOpacity = 0.6, 
                    fillColor = "#07911a",
                    group = grupo,
                    popup = paste(
                        '<br><b>Fid: </b>', paste(centros_barrio_data$fid),
                        '<br><br><b>Uso de suelo: </b>', paste(centros_barrio_data$uso_suelo)
                    ))
    if(!(grupo %in% grupos)){
        grupos<-c(grupos,grupo)
    }
}

###############################################################################
#Sin zonificación

sin_zonificacion_data <- subset(datos, uso_suelo=="Sin Zonificación")
grupo="Sin Zonificación"
if(nrow(sin_zonificacion_data)>0){ 
    pb <- txtProgressBar(min = 0, max = nrow(sin_zonificacion_data), style = 3)
    for (i in 1:nrow(sin_zonificacion_data)){
        if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(sin_zonificacion_data)){
            setTxtProgressBar(pb, i)
        }
        sin_zonificacion_data$geo_shape[i]<-str_replace(sin_zonificacion_data$geo_shape[i],"type",'"type"')
        sin_zonificacion_data$geo_shape[i]<-str_replace(sin_zonificacion_data$geo_shape[i],"MultiPolygon",'"MultiPolygon"')
        sin_zonificacion_data$geo_shape[i]<-str_replace(sin_zonificacion_data$geo_shape[i],"coordinates",'"coordinates"')
        }
    polygons <-  geojson_sf(sin_zonificacion_data$geo_shape)
    map <- map %>%

        addPolygons(data= polygons,
                    stroke = TRUE, 
                    color="#2b2b2a",
                    smoothFactor = 0.5,
                    weight = 0.3,
                    fillOpacity = 0.6, 
                    fillColor = "#868783",
                    group = grupo,
                    popup = paste(
                        '<br><b>Fid: </b>', paste(sin_zonificacion_data$fid),
                        '<br><br><b>Uso de suelo: </b>', paste(sin_zonificacion_data$uso_suelo)
                    ))
    if(!(grupo %in% grupos)){
        grupos<-c(grupos,grupo)
    }
}

###############################################################################

map <- addTiles(map)
map<- map %>% addLayersControl( #Agrega control sobre visulización de Oxxos en el mapa
  overlayGroups = grupos,
  options = layersControlOptions(collapsed = FALSE)
)
map <- map %>% addResetMapButton() 

###############################################################################
setwd(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Mapas_distritos/")) 
saveWidget(map, file=paste0(distrito_str,"_map.html"))
