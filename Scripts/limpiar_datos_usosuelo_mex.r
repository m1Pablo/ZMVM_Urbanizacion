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

ubicacion_dist <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/DistritosEODHogaresZMVM2017/DistritosEODHogaresZMVM2017.shp") 
dist_todos <- readOGR(ubicacion_dist, layer = paste0("DistritosEODHogaresZMVM2017"), verbose = FALSE, GDAL1_integer64_policy=TRUE) 
dist_todos <- spTransform(dist_todos,CRS("+proj=longlat +ellps=WGS84 +no_defs"))



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


ubicacion_datos <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/ZMVM_municipios.csv") 
datos <- ubicacion_datos %>% read.csv(header = TRUE) 
datos <- datos %>% lapply(as.character) %>% as.data.frame(stringsAsFactors = FALSE)
mex_data <- subset(datos, state_abbr=="MEX")

ubicacion_distritos_a_corregir<-paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Uso_suelo_MEX/usos_de_suelo_a_corregir.csv") 
distritos_a_corregir<- ubicacion_distritos_a_corregir %>% read.csv(header = TRUE) 
distritos_a_corregir <- distritos_a_corregir %>% lapply(as.character) %>% as.data.frame(stringsAsFactors = FALSE)

dist_todos_mex <- subset(dist_todos, dist_todos$Distrito %in% distritos_mex)

ubicacion_zh_mex_todos <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/igecemTipologiaahA2015Cg/igecemTipologiaahA2015Cg.shp") #Ubicación del archivo shapefile 
zh_mex_todos <- ubicacion_zh_mex_todos %>% readOGR(layer = paste0("igecemTipologiaahA2015Cg"), verbose = FALSE, GDAL1_integer64_policy=TRUE) #Leer shapefile
zh_mex_todos <- zh_mex_todos %>% spTransform(CRS("+proj=longlat +ellps=WGS84 +no_defs"))
zh_mex_todos <- subset(zh_mex_todos,zh_mex_todos$cveinegi %in% mex_data$region)
zh_mex_todos <- subset(zh_mex_todos, zh_mex_todos$tipologia %in% c("Habitacional","Industrial","Comercial","Equipamiento"))
zh_mex_todos_sf <- st_as_sf(zh_mex_todos)
zh_mex_todos_cent <- st_centroid(zh_mex_todos_sf)
total_puntos<-1000
zh_mex_todos$distrito_centro<-0
zh_mex_todos$area<-0
zh_mex_todos$distrito1_nombre<-0
zh_mex_todos$distrito1_puntos<-0
zh_mex_todos$distrito2_nombre<-0
zh_mex_todos$distrito2_puntos<-0
zh_mex_todos$distrito3_nombre<-0
zh_mex_todos$distrito3_puntos<-0
zh_mex_todos$distrito4_nombre<-0
zh_mex_todos$distrito4_puntos<-0
zh_mex_todos$distrito5_nombre<-0
zh_mex_todos$distrito5_puntos<-0
zh_mex_todos$distrito6_nombre<-0
zh_mex_todos$distrito6_puntos<-0
zh_mex_todos$distrito7_nombre<-0
zh_mex_todos$distrito7_puntos<-0
zh_mex_todos$distrito8_nombre<-0
zh_mex_todos$distrito8_puntos<-0
zh_mex_todos$distrito9_nombre<-0
zh_mex_todos$distrito9_puntos<-0
zh_mex_todos$distrito10_nombre<-0
zh_mex_todos$distrito10_puntos<-0

pb <- txtProgressBar(min = 0, max = nrow(zh_mex_todos_cent), style = 3)

map <- leaflet()

for (i in 1:nrow(zh_mex_todos_cent)){
    setTxtProgressBar(pb, i)
    point <- SpatialPoints(cbind(as.numeric(unlist(zh_mex_todos_cent$geometry[i]))[1],as.numeric(unlist(zh_mex_todos_cent$geometry[i]))[2]),proj4string=CRS(as.character("+proj=longlat +ellps=WGS84 +no_defs")))
    zh_mex_todos$distrito_centro[i]<-as.character(unlist(over(point,dist_todos,returnList = FALSE)$Distrito))
    zh_mex_todos$area[i]<-area(zh_mex_todos[i,1])
    sample_points<-st_as_sf(spsample(as(zh_mex_todos[i,1],'Spatial'),n=total_puntos,"random"))
    testpoints<- matrix(nrow = nrow(sample_points), ncol = 1)
    for (j in 1:nrow(sample_points)){
        sample_point <- SpatialPoints(cbind(as.numeric(unlist(sample_points$geometry[j]))[1],as.numeric(unlist(sample_points$geometry[j]))[2]),proj4string=CRS(as.character("+proj=longlat +ellps=WGS84 +no_defs")))
        if (paste0(unlist(over(sample_point,zh_mex_todos[i,],returnList = FALSE)$X__gid))==zh_mex_todos$X__gid[i]){
            testpoints[j,] <- as.character(unlist(over(sample_point,dist_todos,returnList = FALSE)$Distrito))
        }
    }
    testpoints <- testpoints %>% table()
    for (k in 1:length(testpoints)){
        if (k==1){
            zh_mex_todos$distrito1_nombre[i]<-names(testpoints)[k]
            zh_mex_todos$distrito1_puntos[i]<-testpoints[k]
        }
        if (k==2){
            zh_mex_todos$distrito2_nombre[i]<-names(testpoints)[k]
            zh_mex_todos$distrito2_puntos[i]<-testpoints[k]
        }
        if (k==3){
            zh_mex_todos$distrito3_nombre[i]<-names(testpoints)[k]
            zh_mex_todos$distrito3_puntos[i]<-testpoints[k]
        }
        if (k==4){
            zh_mex_todos$distrito4_nombre[i]<-names(testpoints)[k]
            zh_mex_todos$distrito4_puntos[i]<-testpoints[k]
        }
        if (k==5){
            zh_mex_todos$distrito5_nombre[i]<-names(testpoints)[k]
            zh_mex_todos$distrito5_puntos[i]<-testpoints[k]
        }
        if (k==6){
            zh_mex_todos$distrito6_nombre[i]<-names(testpoints)[k]
            zh_mex_todos$distrito6_puntos[i]<-testpoints[k]
        }
        if (k==7){
            zh_mex_todos$distrito7_nombre[i]<-names(testpoints)[k]
            zh_mex_todos$distrito7_puntos[i]<-testpoints[k]
        }
        if (k==8){
            zh_mex_todos$distrito8_nombre[i]<-names(testpoints)[k]
            zh_mex_todos$distrito8_puntos[i]<-testpoints[k]
        }
        if (k==9){
            zh_mex_todos$distrito9_nombre[i]<-names(testpoints)[k]
            zh_mex_todos$distrito9_puntos[i]<-testpoints[k]
        }
        if (k==10){
            zh_mex_todos$distrito10_nombre[i]<-names(testpoints)[k]
            zh_mex_todos$distrito10_puntos[i]<-testpoints[k]
        }
    }
}

write.csv(zh_mex_todos@data,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/USO_suelo_MEX/correccion_uso_suelo_MEX.csv"), row.names = FALSE)
