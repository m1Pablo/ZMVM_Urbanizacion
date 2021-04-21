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
library('mapsapi')
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
    dist_todos_cdmx$area_trabajo[i] <- 0
    #dist_todos_cdmx$area_trabajo[i] <- sum(trabajo_data$area)/1000**2+area_mixto
    ####
    dist_todos_cdmx$radius_trabajo[i] <- sqrt((dist_todos_cdmx$area_trabajo[i]*1000**2)/pi)

    residencia_data <- subset(datos, uso_suelo=="Habitacional")
    ####
    dist_todos_cdmx$area_residencia[i] <- sum(residencia_data$area)/1000**2+area_mixto
    #dist_todos_cdmx$area_residencia[i] <- 0
    ####
    dist_todos_cdmx$radius_residencia[i] <- sqrt(((dist_todos_cdmx$area_residencia[i]+dist_todos_cdmx$area_trabajo[i])*1000**2)/pi)
    #dist_todos_cdmx$radius_residencia[i] <- 0
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


ubicacion_datos <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/ZMVM_municipios.csv") 
datos <- ubicacion_datos %>% read.csv(header = TRUE) 
datos <- datos %>% lapply(as.character) %>% as.data.frame(stringsAsFactors = FALSE)
mex_data <- subset(datos, state_abbr=="MEX")


dist_todos_mex <- subset(dist_todos, dist_todos$Distrito %in% distritos_mex)


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
    dist_todos_mex$area_trabajo[i] <- 0
    #dist_todos_mex$area_trabajo[i] <- sum(trabajo_data$area)/1000**2
    ####
    dist_todos_mex$radius_trabajo[i] <- sqrt((dist_todos_mex$area_trabajo[i]*1000**2)/pi)

    residencia_data <- subset(zh_dist@data, tipologia=="Habitacional")
    ####
    dist_todos_mex$area_residencia[i] <- sum(residencia_data$area)/1000**2
    #dist_todos_mex$area_residencia[i] <- 0
    ####
    dist_todos_mex$radius_residencia[i] <- sqrt(((dist_todos_mex$area_residencia[i]+dist_todos_mex$area_trabajo[i])*1000**2)/pi)
    #dist_todos_mex$radius_residencia[i] <- 0
    ####

    dist_todos_mex$densidad_residentes[i]<-dist_todos_mex$dist_residentes[i]/dist_todos_mex$area_dist[i]
    dist_todos_mex$densidad_trabajador[i]<-dist_todos_mex$dist_trabajadores[i]/dist_todos_mex$area_dist[i]
}


dist_todos <- rbind(dist_todos_cdmx, dist_todos_mex)
dist_todos <- dist_todos[order(dist_todos@data$Distrito),]

locations <- matrix(nrow = nrow(dist_todos@data), ncol = 2) #Iniciar lista vacia para determinar la Clave INEGI de cada Oxxo
locations[,1]<-dist_todos@data$Longitude
locations[,2]<-dist_todos@data$Latitude

google_api_key <- readLines('./google_api_key.txt')

loc_inicial=c(1,11,21,31,41,51,61,71,81,91,101,111,121,131,141,151,161,171,181,191)
loc_final=c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,193)
loc_str=c('1a10','11a20','21a30','31a40','41a50','51a60','61a70','71a80','81a90','91a100','101a110','111a120','121a130','131a140','141a150','151a160','161a170','171a180','181a190','191a193')

##############################################################
# Repetir por cada rango

pb <- txtProgressBar(min = 0, max = length(loc_str), style = 3)
for (i in 1:length(loc_str)){
  setTxtProgressBar(pb, i)
  current<-locations[loc_inicial[i]:loc_final[i],]
  current_str<-loc_str[i]
  current_dist<-dist_todos@data$Distrito[loc_inicial[i]:loc_final[i]]

  ##############################################################
  # Repetir por cada rango

  locations1a10<-locations[1:10,]
  doc = mp_matrix(
    origins = current,
    destinations = locations1a10,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[1:10]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[1:10]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_1a10_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_1a10_t.csv"), row.names = FALSE)

  locations11a20<-locations[11:20,]
  doc = mp_matrix(
    origins = current,
    destinations = locations11a20,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[11:20]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[11:20]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_11a20_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_11a20_t.csv"), row.names = FALSE)

  locations21a30<-locations[21:30,]
  doc = mp_matrix(
    origins = current,
    destinations = locations21a30,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[21:30]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[21:30]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_21a30_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_21a30_t.csv"), row.names = FALSE)

  locations31a40<-locations[31:40,]
  doc = mp_matrix(
    origins = current,
    destinations = locations31a40,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[31:40]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[31:40]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_31a40_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_31a40_t.csv"), row.names = FALSE)

  locations41a50<-locations[41:50,]
  doc = mp_matrix(
    origins = current,
    destinations = locations41a50,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[41:50]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[41:50]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_41a50_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_41a50_t.csv"), row.names = FALSE)

  locations51a60<-locations[51:60,]
  doc = mp_matrix(
    origins = current,
    destinations = locations51a60,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[51:60]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[51:60]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_51a60_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_51a60_t.csv"), row.names = FALSE)

  locations61a70<-locations[61:70,]
  doc = mp_matrix(
    origins = current,
    destinations = locations61a70,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[61:70]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[61:70]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_61a70_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_61a70_t.csv"), row.names = FALSE)

  locations71a80<-locations[71:80,]
  doc = mp_matrix(
    origins = current,
    destinations = locations71a80,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[71:80]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[71:80]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_71a80_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_71a80_t.csv"), row.names = FALSE)

  locations81a90<-locations[81:90,]
  doc = mp_matrix(
    origins = current,
    destinations = locations81a90,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[81:90]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[81:90]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_81a90_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_81a90_t.csv"), row.names = FALSE)

  locations91a100<-locations[91:100,]
  doc = mp_matrix(
    origins = current,
    destinations = locations91a100,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[91:100]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[91:100]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_91a100_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_91a100_t.csv"), row.names = FALSE)

  locations101a110<-locations[101:110,]
  doc = mp_matrix(
    origins = current,
    destinations = locations101a110,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[101:110]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[101:110]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_101a110_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_101a110_t.csv"), row.names = FALSE)

  locations111a120<-locations[111:120,]
  doc = mp_matrix(
    origins = current,
    destinations = locations111a120,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[111:120]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[111:120]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_111a120_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_111a120_t.csv"), row.names = FALSE)

  locations121a130<-locations[121:130,]
  doc = mp_matrix(
    origins = current,
    destinations = locations121a130,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[121:130]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[121:130]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_121a130_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_121a130_t.csv"), row.names = FALSE)

  locations131a140<-locations[131:140,]
  doc = mp_matrix(
    origins = current,
    destinations = locations131a140,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[131:140]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[131:140]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_131a140_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_131a140_t.csv"), row.names = FALSE)

  locations141a150<-locations[141:150,]
  doc = mp_matrix(
    origins = current,
    destinations = locations141a150,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[141:150]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[141:150]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_141a150_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_141a150_t.csv"), row.names = FALSE)

  locations151a160<-locations[151:160,]
  doc = mp_matrix(
    origins = current,
    destinations = locations151a160,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[151:160]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[151:160]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_151a160_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_151a160_t.csv"), row.names = FALSE)

  locations161a170<-locations[161:170,]
  doc = mp_matrix(
    origins = current,
    destinations = locations161a170,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[161:170]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[161:170]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_161a170_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_161a170_t.csv"), row.names = FALSE)

  locations171a180<-locations[171:180,]
  doc = mp_matrix(
    origins = current,
    destinations = locations171a180,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[171:180]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[171:180]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_171a180_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_171a180_t.csv"), row.names = FALSE)

  locations181a190<-locations[181:190,]
  doc = mp_matrix(
    origins = current,
    destinations = locations181a190,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[181:190]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[181:190]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_181a190_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_181a190_t.csv"), row.names = FALSE)

  locations191a193<-locations[191:193,]
  doc = mp_matrix(
    origins = current,
    destinations = locations191a193,
    mode='driving',
    key = google_api_key,
    quiet = TRUE
  )
  d = mp_get_matrix(doc, value = "distance_m")
  colnames(d) = dist_todos@data$Distrito[191:193]
  rownames(d) = current_dist
  t = mp_get_matrix(doc, value = "duration_s")
  colnames(t) = dist_todos@data$Distrito[191:193]
  rownames(t) = current_dist
  write.csv(d,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_191a193_d.csv"), row.names = FALSE)
  write.csv(t,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_191a193_t.csv"), row.names = FALSE)

  d_1a10 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_1a10_d.csv"), header = TRUE)
  t_1a10 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_1a10_t.csv"), header = TRUE)
  d_11a20 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_11a20_d.csv"), header = TRUE)
  t_11a20 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_11a20_t.csv"), header = TRUE)
  d_final <- cbind(d_1a10,d_11a20)
  t_final <- cbind(t_1a10,t_11a20)
  d_21a30 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_21a30_d.csv"), header = TRUE)
  t_21a30 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_21a30_t.csv"), header = TRUE)
  d_final <- d_final %>% cbind(d_21a30)
  t_final <- t_final %>% cbind(t_21a30)
  d_31a40 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_31a40_d.csv"), header = TRUE)
  t_31a40 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_31a40_t.csv"), header = TRUE)
  d_final <- d_final %>% cbind(d_31a40)
  t_final <- t_final %>% cbind(t_31a40)
  d_41a50 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_41a50_d.csv"), header = TRUE)
  t_41a50 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_41a50_t.csv"), header = TRUE)
  d_final <- d_final %>% cbind(d_41a50)
  t_final <- t_final %>% cbind(t_41a50)
  d_51a60 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_51a60_d.csv"), header = TRUE)
  t_51a60 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_51a60_t.csv"), header = TRUE)
  d_final <- d_final %>% cbind(d_51a60)
  t_final <- t_final %>% cbind(t_51a60)
  d_61a70 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_61a70_d.csv"), header = TRUE)
  t_61a70 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_61a70_t.csv"), header = TRUE)
  d_final <- d_final %>% cbind(d_61a70)
  t_final <- t_final %>% cbind(t_61a70)
  d_71a80 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_71a80_d.csv"), header = TRUE)
  t_71a80 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_71a80_t.csv"), header = TRUE)
  d_final <- d_final %>% cbind(d_71a80)
  t_final <- t_final %>% cbind(t_71a80)
  d_81a90 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_81a90_d.csv"), header = TRUE)
  t_81a90 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_81a90_t.csv"), header = TRUE)
  d_final <- d_final %>% cbind(d_81a90)
  t_final <- t_final %>% cbind(t_81a90)
  d_91a100 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_91a100_d.csv"), header = TRUE)
  t_91a100 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_91a100_t.csv"), header = TRUE)
  d_final <- d_final %>% cbind(d_91a100)
  t_final <- t_final %>% cbind(t_91a100)
  d_101a110 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_101a110_d.csv"), header = TRUE)
  t_101a110 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_101a110_t.csv"), header = TRUE)
  d_final <- d_final %>% cbind(d_101a110)
  t_final <- t_final %>% cbind(t_101a110)
  d_111a120 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_111a120_d.csv"), header = TRUE)
  t_111a120 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_111a120_t.csv"), header = TRUE)
  d_final <- d_final %>% cbind(d_111a120)
  t_final <- t_final %>% cbind(t_111a120)
  d_121a130 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_121a130_d.csv"), header = TRUE)
  t_121a130 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_121a130_t.csv"), header = TRUE)
  d_final <- d_final %>% cbind(d_121a130)
  t_final <- t_final %>% cbind(t_121a130)
  d_131a140 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_131a140_d.csv"), header = TRUE)
  t_131a140 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_131a140_t.csv"), header = TRUE)
  d_final <- d_final %>% cbind(d_131a140)
  t_final <- t_final %>% cbind(t_131a140)
  d_141a150 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_141a150_d.csv"), header = TRUE)
  t_141a150 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_141a150_t.csv"), header = TRUE)
  d_final <- d_final %>% cbind(d_141a150)
  t_final <- t_final %>% cbind(t_141a150)
  d_151a160 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_151a160_d.csv"), header = TRUE)
  t_151a160 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_151a160_t.csv"), header = TRUE)
  d_final <- d_final %>% cbind(d_151a160)
  t_final <- t_final %>% cbind(t_151a160)
  d_161a170 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_161a170_d.csv"), header = TRUE)
  t_161a170 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_161a170_t.csv"), header = TRUE)
  d_final <- d_final %>% cbind(d_161a170)
  t_final <- t_final %>% cbind(t_161a170)
  d_171a180 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_171a180_d.csv"), header = TRUE)
  t_171a180 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_171a180_t.csv"), header = TRUE)
  d_final <- d_final %>% cbind(d_171a180)
  t_final <- t_final %>% cbind(t_171a180)
  d_181a190 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_181a190_d.csv"), header = TRUE)
  t_181a190 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_181a190_t.csv"), header = TRUE)
  d_final <- d_final %>% cbind(d_181a190)
  t_final <- t_final %>% cbind(t_181a190)
  d_191a193 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_191a193_d.csv"), header = TRUE)
  t_191a193 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"/locations",current_str,"_191a193_t.csv"), header = TRUE)
  d_final <- d_final %>% cbind(d_191a193)
  t_final <- t_final %>% cbind(t_191a193)

  write.csv(d_final,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"_d.csv"), row.names = FALSE)
  write.csv(t_final,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations",current_str,"_t.csv"), row.names = FALSE)
}
###############################################################################3

D_1a10 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations1a10_d.csv"), header = TRUE)
T_1a10 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations1a10_t.csv"), header = TRUE)
D_11a20 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations11a20_d.csv"), header = TRUE)
T_11a20 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations11a20_t.csv"), header = TRUE)
D_final <- rbind(D_1a10,D_11a20)
T_final <- rbind(T_1a10,T_11a20)
D_21a30 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations21a30_d.csv"), header = TRUE)
T_21a30 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations21a30_t.csv"), header = TRUE)
D_final <- D_final %>% rbind(D_21a30)
T_final <- T_final %>% rbind(T_21a30)
D_31a40 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations31a40_d.csv"), header = TRUE)
T_31a40 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations31a40_t.csv"), header = TRUE)
D_final <- D_final %>% rbind(D_31a40)
T_final <- T_final %>% rbind(T_31a40)
D_41a50 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations41a50_d.csv"), header = TRUE)
T_41a50 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations41a50_t.csv"), header = TRUE)
D_final <- D_final %>% rbind(D_41a50)
T_final <- T_final %>% rbind(T_41a50)
D_51a60 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations51a60_d.csv"), header = TRUE)
T_51a60 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations51a60_t.csv"), header = TRUE)
D_final <- D_final %>% rbind(D_51a60)
T_final <- T_final %>% rbind(T_51a60)
D_61a70 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations61a70_d.csv"), header = TRUE)
T_61a70 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations61a70_t.csv"), header = TRUE)
D_final <- D_final %>% rbind(D_61a70)
T_final <- T_final %>% rbind(T_61a70)
D_71a80 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations71a80_d.csv"), header = TRUE)
T_71a80 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations71a80_t.csv"), header = TRUE)
D_final <- D_final %>% rbind(D_71a80)
T_final <- T_final %>% rbind(T_71a80)
D_81a90 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations81a90_d.csv"), header = TRUE)
T_81a90 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations81a90_t.csv"), header = TRUE)
D_final <- D_final %>% rbind(D_81a90)
T_final <- T_final %>% rbind(T_81a90)
D_91a100 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations91a100_d.csv"), header = TRUE)
T_91a100 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations91a100_t.csv"), header = TRUE)
D_final <- D_final %>% rbind(D_91a100)
T_final <- T_final %>% rbind(T_91a100)
D_101a110 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations101a110_d.csv"), header = TRUE)
T_101a110 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations101a110_t.csv"), header = TRUE)
D_final <- D_final %>% rbind(D_101a110)
T_final <- T_final %>% rbind(T_101a110)
D_111a120 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations111a120_d.csv"), header = TRUE)
T_111a120 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations111a120_t.csv"), header = TRUE)
D_final <- D_final %>% rbind(D_111a120)
T_final <- T_final %>% rbind(T_111a120)
D_121a130 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations121a130_d.csv"), header = TRUE)
T_121a130 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations121a130_t.csv"), header = TRUE)
D_final <- D_final %>% rbind(D_121a130)
T_final <- T_final %>% rbind(T_121a130)
D_131a140 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations131a140_d.csv"), header = TRUE)
T_131a140 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations131a140_t.csv"), header = TRUE)
D_final <- D_final %>% rbind(D_131a140)
T_final <- T_final %>% rbind(T_131a140)
D_141a150 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations141a150_d.csv"), header = TRUE)
T_141a150 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations141a150_t.csv"), header = TRUE)
D_final <- D_final %>% rbind(D_141a150)
T_final <- T_final %>% rbind(T_141a150)
D_151a160 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations151a160_d.csv"), header = TRUE)
T_151a160 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations151a160_t.csv"), header = TRUE)
D_final <- D_final %>% rbind(D_151a160)
T_final <- T_final %>% rbind(T_151a160)
D_161a170 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations161a170_d.csv"), header = TRUE)
T_161a170 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations161a170_t.csv"), header = TRUE)
D_final <- D_final %>% rbind(D_161a170)
T_final <- T_final %>% rbind(T_161a170)
D_171a180 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations171a180_d.csv"), header = TRUE)
T_171a180 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations171a180_t.csv"), header = TRUE)
D_final <- D_final %>% rbind(D_171a180)
T_final <- T_final %>% rbind(T_171a180)
D_181a190 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations181a190_d.csv"), header = TRUE)
T_181a190 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations181a190_t.csv"), header = TRUE)
D_final <- D_final %>% rbind(D_181a190)
T_final <- T_final %>% rbind(T_181a190)
D_191a193 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations191a193_d.csv"), header = TRUE)
T_191a193 <- read.csv(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations191a193_t.csv"), header = TRUE)
D_final <- D_final %>% rbind(D_191a193)
T_final <- T_final %>% rbind(T_191a193)

write.csv(D_final,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations_distancia_google.csv"), row.names = FALSE)
write.csv(T_final,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Matrices_distancia_tiempo/locations_tiempo_google.csv"), row.names = FALSE)
