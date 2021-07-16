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
setwd(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion_Keys/")) 
###############################################################################
# SQl database

db_key <- readLines('./database_key.txt')
con <- DBI::dbConnect(Postgres(), 
                      dbname = "ZMVM_Urbanizacion",
                      host = "localhost", 
                      port = 5432,
                      user = "postgres", 
                      password = db_key)

###############################################################################
# Initiate map, get data, create empty map and groups list
grupo_d="Límite distrito"
grupos<-c()

ubicacion_dist <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion_Datos/DistritosEODHogaresZMVM2017/DistritosEODHogaresZMVM2017.shp") 
dist_todos <- readOGR(ubicacion_dist, layer = paste0("DistritosEODHogaresZMVM2017"), verbose = FALSE, GDAL1_integer64_policy=TRUE) 
dist_todos <- spTransform(dist_todos,CRS("+proj=longlat +ellps=WGS84 +no_defs"))

###############################################################################
#CDMX

distritos_cdmx<-c(1:85)

for(i in 1:length(distritos_cdmx)){
    if(str_length(as.character(distritos_cdmx[i]))==1){
        distritos_cdmx[i]<-paste0("00",distritos_cdmx[i])
    }
    if(str_length(as.character(distritos_cdmx[i]))==2){
        distritos_cdmx[i]<-paste0("0",distritos_cdmx[i])
    }
}

dist_todos_cdmx <- subset(dist_todos, dist_todos$Distrito %in% distritos_cdmx)

pb <- txtProgressBar(min = 0, max = length(distritos_cdmx), style = 3)
i<-0
for(distrito_str in distritos_cdmx){
    i<-i+1
    if (sample(1:1, 1)==sample(1:1, 1) || i==length(distritos_cdmx)){
    setTxtProgressBar(pb, i)
    }
   
    distrito_int<-as.numeric(distrito_str)
    query= paste0("select * from fid where distrito = ",distrito_str)
    datos<-dbGetQuery(con, query)
    query_residentes=paste0('select SUM("FACTOR") as "',distrito_int,'_residentes" from "viajes" where "P5_3"=1 and "P5_6"=1 and "P5_13"=2 and "DTO_ORIGEN"=',distrito_int)
    dist_todos_cdmx$dist_residentes[i]<-unlist(dbGetQuery(con, query_residentes))
    query_trabajadores=paste0('select SUM("FACTOR") as "',distrito_int,'_residentes" from "viajes" where "P5_3"=1 and "P5_6"=1 and "P5_13"=2 and "DTO_DEST"=',distrito_int)
    dist_todos_cdmx$dist_trabajadores[i]<-unlist(dbGetQuery(con, query_trabajadores))
    ###############################################################################
    #Distritos

    dist <- subset(dist_todos_cdmx, dist_todos_cdmx$Distrito %in% distrito_str)
    dist_todos_cdmx$Estado<-"CDMX"
    dist_todos_cdmx$area_dist[i] <-area(dist)/1000**2
    dist_todos_cdmx$radius_dist[i] <- sqrt((dist_todos_cdmx$area_dist[i]*1000**2)/pi)
    point_dist <- dist %>% geojson_json() %>% geojson_sf() %>% st_centroid()
    dist_todos_cdmx$Longitude[i]<-unlist(point_dist$geometry)[1]
    dist_todos_cdmx$Latitude[i]<-unlist(point_dist$geometry)[2]

    mixto_data <- subset(datos, uso_suelo=="Habitacional y comercial")
    area_mixto <- sum(mixto_data$area)/1000**2

    trabajo_data <- subset(datos, uso_suelo %in% c("Industrial","Industrial y comercial","Equipamiento"))
    ####
    #dist_todos_cdmx$area_trabajo[i] <- 0
    dist_todos_cdmx$area_trabajo[i] <- sum(trabajo_data$area)/1000**2+area_mixto
    ####
    dist_todos_cdmx$radius_trabajo[i] <- sqrt((dist_todos_cdmx$area_trabajo[i]*1000**2)/pi)

    residencia_data <- subset(datos, uso_suelo=="Habitacional")
    ####
    #dist_todos_cdmx$area_residencia[i] <- sum(residencia_data$area)/1000**2+area_mixto
    dist_todos_cdmx$area_residencia[i] <- 0
    ####
    #dist_todos_cdmx$radius_residencia[i] <- sqrt(((dist_todos_cdmx$area_residencia[i]+dist_todos_cdmx$area_trabajo[i])*1000**2)/pi)
    dist_todos_cdmx$radius_residencia[i] <- 0
    ####
    
    dist_todos_cdmx$densidad_residentes[i]<-dist_todos_cdmx$dist_residentes[i]/dist_todos_cdmx$area_dist[i]
    dist_todos_cdmx$densidad_trabajador[i]<-dist_todos_cdmx$dist_trabajadores[i]/dist_todos_cdmx$area_dist[i]
    
    ###############################################################################
    #sin zonificación
}


###############################################################################
#MEX

distritos_mex<-c(100:207)

for(i in 1:length(distritos_mex)){
    if(str_length(as.character(distritos_mex[i]))==1){
        distritos_mex[i]<-paste0("00",distritos_mex[i])
    }
    if(str_length(as.character(distritos_mex[i]))==2){
        distritos_mex[i]<-paste0("0",distritos_mex[i])
    }
}

###############################################################################
# Get data, create empty map and groups list


ubicacion_datos <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion_Datos/ZMVM_municipios.csv") 
datos <- ubicacion_datos %>% read.csv(header = TRUE) 
datos <- datos %>% lapply(as.character) %>% as.data.frame(stringsAsFactors = FALSE)
mex_data <- subset(datos, state_abbr=="MEX")


dist_todos_mex <- subset(dist_todos, dist_todos$Distrito %in% distritos_mex)


ubicacion_zh_mex_todos <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion_Datos/igecemTipologiaahA2015Cg/igecemTipologiaahA2015Cg.shp") #Ubicación del archivo shapefile 
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

pb <- txtProgressBar(min = 0, max = length(distritos_mex), style = 3)
i<-0

for(distrito_str in distritos_mex){
    i<-i+1
    if (sample(1:1, 1)==sample(1:1, 1) || i==length(distritos_mex)){
    setTxtProgressBar(pb, i)
    }
    zh_mex<-zh_mex_todos
    distrito_int<-as.numeric(distrito_str)
    query_residentes=paste0('select SUM("FACTOR") as "',distrito_int,'_residentes" from "viajes" where "P5_3"=1 and "P5_6"=1 and "P5_13"=2 and "DTO_ORIGEN"=',distrito_int)
    dist_todos_mex$dist_residentes[i]<-unlist(dbGetQuery(con, query_residentes))
    query_trabajadores=paste0('select SUM("FACTOR") as "',distrito_int,'_residentes" from "viajes" where "P5_3"=1 and "P5_6"=1 and "P5_13"=2 and "DTO_DEST"=',distrito_int)
    dist_todos_mex$dist_trabajadores[i]<-unlist(dbGetQuery(con, query_trabajadores))
    #Distritos

    dist <- subset(dist_todos_mex, dist_todos_mex$Distrito %in% distrito_str)

    k <- 0

    zh_dist <- subset(zh_mex,zh_mex$distrito==distrito_str)
    
    dist_todos_mex$Estado<-"MEX"
    dist_todos_mex$area_dist[i] <-area(dist)/1000**2
    dist_todos_mex$radius_dist[i] <- sqrt((dist_todos_mex$area_dist[i]*1000**2)/pi)
    point_dist <- dist %>% geojson_json() %>% geojson_sf() %>% st_centroid()
    dist_todos_mex$Longitude[i]<-unlist(point_dist$geometry)[1]
    dist_todos_mex$Latitude[i]<-unlist(point_dist$geometry)[2]

    trabajo_data <- subset(zh_dist@data, tipologia %in% c("Industrial","Comercial","Equipamiento"))
    ####
    #dist_todos_mex$area_trabajo[i] <- 0
    dist_todos_mex$area_trabajo[i] <- sum(trabajo_data$area)/1000**2
    ####
    dist_todos_mex$radius_trabajo[i] <- sqrt((dist_todos_mex$area_trabajo[i]*1000**2)/pi)

    residencia_data <- subset(zh_dist@data, tipologia=="Habitacional")
    ####
    #dist_todos_mex$area_residencia[i] <- sum(residencia_data$area)/1000**2
    dist_todos_mex$area_residencia[i] <- 0
    ####
    #dist_todos_mex$radius_residencia[i] <- sqrt(((dist_todos_mex$area_residencia[i]+dist_todos_mex$area_trabajo[i])*1000**2)/pi)
    dist_todos_mex$radius_residencia[i] <- 0
    ####

    dist_todos_mex$densidad_residentes[i]<-dist_todos_mex$dist_residentes[i]/dist_todos_mex$area_dist[i]
    dist_todos_mex$densidad_trabajador[i]<-dist_todos_mex$dist_trabajadores[i]/dist_todos_mex$area_dist[i]
}

###############################################################################
# COLORES

dist_todos <- rbind(dist_todos_cdmx, dist_todos_mex)
dist_todos@data$Descripcio <- iconv(dist_todos@data$Descripcio, from="UTF-8", to="LATIN1")
dist_todos <- dist_todos %>% filter_all(all_vars(!is.na(.)))
escala <- log2(c(0,0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1)+1)

max_residentes<- max(dist_todos$dist_residentes, na.rm = TRUE)
min_residentes<- min(dist_todos$dist_residentes, na.rm = TRUE)
bins_residentes <- escala*(max_residentes)+min_residentes
bins_residentes[1]<-0

max_trabajadores<- max(dist_todos$dist_trabajadores, na.rm = TRUE)
min_trabajadores<- min(dist_todos$dist_trabajadores, na.rm = TRUE)

bins_trabajadores <- escala*(max_trabajadores)+min_trabajadores
bins_trabajadores[1]<-0

pal_residentes <- colorBin("Blues", domain = dist_todos$dist_residentes, bins = bins_residentes)
pal_trabajadores <- colorBin("Purples", domain = dist_todos$dist_trabajadores, bins = bins_trabajadores)

pal_residentes_contorno <- colorBin("Blues", domain = dist_todos$dist_residentes, bins = bins_residentes/2)
pal_trabajadores_contorno <- colorBin("Purples", domain = dist_todos$dist_trabajadores, bins = bins_trabajadores/2)


###############################################################################
# MAPAS DISTIRTOS
map <- leaflet()

map <- map %>%

    addPolygons(data=dist_todos,
                stroke = TRUE,
                color='grey',
                opacity=1,
                smoothFactor = 0.5,
                weight = 1,
                fillColor = "#26b8e8",
                fillOpacity = 0,
                group = grupo_d,
                popup = ~paste(
                    '<br><b>', paste(Descripcio),
                    '<br><br><b>Clave distrito: </b>', paste(Distrito),
                    '<br><br><b>Estado: </b>', paste(Estado)
                ))


grupo="Habitacional y mixto"

map <- map %>% 

addCircles(data=dist_todos,
        lng=~Longitude,
        lat=~Latitude,
        radius=~radius_residencia,
        stroke = TRUE,
        color='black',
        weight = 2,
        fillColor = ~pal_residentes(dist_residentes),
        group = grupo,
        fillOpacity = 1,
        popup = ~paste(
            '<br><b>', paste(Descripcio),
            '<br><br><b>Distrito: </b>', paste(Distrito),
            '<br><br><b>Estado: </b>', paste(Estado),
            '<br><br><b>Uso de suelo: </b>', paste(grupo),
            '<br><br><b>Porcentaje del suelo del distrito: </b>', paste0(round((area_residencia/area_dist)*100,2),'%'),
            '<br><br><b>Residentes: </b>', paste0(round(dist_residentes,0))
        ))

grupo="Industrial, comercial, equipamiento y mixto"

map <- map %>% 

addCircles(data=dist_todos,
        lng=~Longitude,
        lat=~Latitude,
        radius=~radius_trabajo,
        stroke = TRUE,
        color='black',
        weight = 2,
        fillColor = ~pal_trabajadores(dist_trabajadores),
        group = grupo,
        fillOpacity = 1,
        popup = ~paste(
            '<br><b>', paste(Descripcio),
            '<br><br><b>Distrito: </b>', paste(Distrito),
            '<br><br><b>Estado: </b>', paste(Estado),
            '<br><br><b>Uso de suelo: </b>', paste(grupo),
            '<br><br><b>Porcentaje del suelo del distrito: </b>', paste0(round((area_trabajo/area_dist)*100,2),'%'),
            '<br><br><b>Trabajadores: </b>', paste0(round(dist_trabajadores,0))
        ))

################################################################################

map <- addTiles(map)
map <-map %>% addProviderTiles(providers$CartoDB.Positron)
map <- map %>% addLayersControl( #Agrega control sobre visulización de Oxxos en el mapa
    overlayGroups = grupo_d,
    options = layersControlOptions(collapsed = TRUE)
)
map <- map %>% addLegend(
    "bottomright",
    ####
    #pal = pal_residentes,
    pal = pal_trabajadores,
    ####
    #values = round(dist_todos$dist_residentes,0),
    values = round(dist_todos$dist_trabajadores,0),
    ####
    #title = "Residentes",
    title = "Trabajadores",
    ####
    opacity = 1
)
map <- map %>% hideGroup(grupo_d)
map <- map %>% addResetMapButton() 



###############################################################################
setwd(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Mapas_distritos/")) 
saveWidget(map, file=paste0("000_circle_work_heatmap.html"))
