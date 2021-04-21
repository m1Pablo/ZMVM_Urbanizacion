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
library("stringr")

###############################################################################

clc <- function() cat(rep("\n", 101))

###############################################################################

#09002 AZCAPOTZALCO ✅
#09003 COYOACAN ✅
#09004 CUAJIMALPA_DE_MORELOS ✅
#09005 GUSTAVO_A_MADERO ✅
#09006 IZTACALCO ✅
#09007 IZTAPALAPA ✅
#09008 MAGDALENA_CONTRERAS ✅
#09009 MILPA_ALTA ✅
#09010 ALVARO_OBREGON ✅
#09011 TLAHUAC ✅
#09012 TLALPAN ✅
#09013 XOCHIMILCO ✅
#09014 BENITO_JUAREZ ✅
#09015 CUAUHTEMOC ✅
#09016 MIGUEL_HIDALGO ✅
#09017 VENUSTIANO_CARRANZA ✅

alcaldia <- "VENUSTIANO_CARRANZA"
alcaldia_clave_str <- "09017"
alcaldia_clave <- as.numeric(alcaldia_clave_str)

ubicacion_alc <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/df/df_municipio.shp")  
alc <- readOGR(ubicacion_alc, layer = paste0("df_municipio"), verbose = FALSE, GDAL1_integer64_policy=TRUE) 
alc <- spTransform(alc,CRS("+proj=longlat +ellps=WGS84 +no_defs"))
alc <- subset(alc,alc$CVEGEO %in% alcaldia_clave_str)

###############################################################################
#Distritos


ubicacion_dist <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/DistritosEODHogaresZMVM2017/DistritosEODHogaresZMVM2017.shp") 
dist <- readOGR(ubicacion_dist, layer = paste0("DistritosEODHogaresZMVM2017"), verbose = FALSE, GDAL1_integer64_policy=TRUE) 
dist <- spTransform(dist,CRS("+proj=longlat +ellps=WGS84 +no_defs"))

ubicacion_dist_data <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/Distritos_Municipios.csv") 
dist_data <- read.csv(ubicacion_dist_data, header = TRUE) 
dist_data <- as.data.frame(lapply(dist_data, as.character))

ubicacion_vecinos <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/cdmx_alc_vecinos.csv") 
vecinos_datos <- read.csv(ubicacion_vecinos, header = TRUE) 
vecinos_datos <- as.data.frame(lapply(vecinos_datos, as.character))
vecinos <- subset(vecinos_datos, CVEGEO==alcaldia_clave)

vecindad_claves <- c()
for (i in 1:length(vecinos[1,])){
    vecindad_claves <- vecindad_claves %>% c(as.numeric(as.character(vecinos[1,i])))
}

###############################################################################
#Datos uso de suelo

ubicacion_alc_us <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/",alcaldia,"/",alcaldia,".shp")  
alc_us <- readOGR(ubicacion_alc_us, layer = paste0(alcaldia), verbose = FALSE, GDAL1_integer64_policy=TRUE) 
alc_us <- spTransform(alc_us,CRS("+proj=longlat +ellps=WGS84 +no_defs"))

ubicacion_datos <- paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/",alcaldia,"/",alcaldia,"_datos.csv") 
datos <- read.csv(ubicacion_datos, header = TRUE)[,c('fid', 'geo_shape','uso_construccion')]
datos <- as.data.frame(lapply(datos, as.character))
datos <- datos %>% distinct(fid, .keep_all = TRUE)

###############################################################################
#Habitacional

habitacional_data <- subset(datos, uso_construccion=="Habitacional") 
habitacional <- subset(alc_us,alc_us$fid %in% habitacional_data$fid)
testpoints_habitacional <- matrix(nrow = nrow(habitacional_data), ncol = 3) 

pb <- txtProgressBar(min = 0, max = nrow(habitacional_data), style = 3)
for (i in 1:nrow(habitacional_data)){
    if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(habitacional_data)){
        setTxtProgressBar(pb, i)
    }
    polygon <-  geojson_sf(habitacional_data$geo_shape[i])
    point <- st_centroid(polygon)
    point <- SpatialPoints(cbind(unlist(point$geometry)[1],unlist(point$geometry)[2]),proj4string=CRS(as.character("+proj=longlat +ellps=WGS84 +no_defs")))
    distrito <- as.character(over(point,dist,returnList = FALSE)$Distrito)
    testpoints_habitacional[i,] <- cbind(as.character((habitacional_data$fid[i])),distrito[1],area(habitacional[i,1]))
    }

testpoints_habitacional <- testpoints_habitacional %>% as.data.frame()
colnames(testpoints_habitacional) <- c("fid","distrito","area")
testpoints_habitacional <- testpoints_habitacional %>% mutate(fid = as.numeric(as.character(fid)))
habitacional@data <- habitacional@data %>% left_join(testpoints_habitacional, by = "fid")
habitacional$uso_suelo<-"Habitacional"

###############################################################################
#Habitacional y Comercial

habitacional_comercial_data <- subset(datos, uso_construccion=="Habitacional y comercial") 
habitacional_comercial <- subset(alc_us,alc_us$fid %in% habitacional_comercial_data$fid)
testpoints_habitacional_comercial <- matrix(nrow = nrow(habitacional_comercial_data), ncol = 3) 

pb <- txtProgressBar(min = 0, max = nrow(habitacional_comercial_data), style = 3)
for (i in 1:nrow(habitacional_comercial_data)){
    if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(habitacional_comercial_data)){
        setTxtProgressBar(pb, i)
    }
    polygon <-  geojson_sf(habitacional_comercial_data$geo_shape[i])
    point <- st_centroid(polygon)
    point <- SpatialPoints(cbind(unlist(point$geometry)[1],unlist(point$geometry)[2]),proj4string=CRS(as.character("+proj=longlat +ellps=WGS84 +no_defs")))
    distrito <- as.character(over(point,dist,returnList = FALSE)$Distrito)
    testpoints_habitacional_comercial[i,] <- cbind(as.character((habitacional_comercial_data$fid[i])),distrito[1],area(habitacional_comercial[i,1]))
    }

testpoints_habitacional_comercial <- testpoints_habitacional_comercial %>% as.data.frame()
colnames(testpoints_habitacional_comercial) <- c("fid","distrito","area")
testpoints_habitacional_comercial <- testpoints_habitacional_comercial %>% mutate(fid = as.numeric(as.character(fid)))
habitacional_comercial@data <- habitacional_comercial@data %>% left_join(testpoints_habitacional_comercial, by = "fid")
habitacional_comercial$uso_suelo<-"Habitacional y comercial"


###############################################################################
#Industrial

industrial_data <- subset(datos, uso_construccion=="Industrial") 
industrial <- subset(alc_us,alc_us$fid %in% industrial_data$fid)
testpoints_industrial <- matrix(nrow = nrow(industrial_data), ncol = 3) 

pb <- txtProgressBar(min = 0, max = nrow(industrial_data), style = 3)
for (i in 1:nrow(industrial_data)){
    if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(industrial_data)){
        setTxtProgressBar(pb, i)
    }
    polygon <-  geojson_sf(industrial_data$geo_shape[i])
    point <- st_centroid(polygon)
    point <- SpatialPoints(cbind(unlist(point$geometry)[1],unlist(point$geometry)[2]),proj4string=CRS(as.character("+proj=longlat +ellps=WGS84 +no_defs")))
    distrito <- as.character(over(point,dist,returnList = FALSE)$Distrito)
    testpoints_industrial[i,] <- cbind(as.character((industrial_data$fid[i])),distrito[1],area(industrial[i,1]))
    }

testpoints_industrial <- testpoints_industrial %>% as.data.frame()
colnames(testpoints_industrial) <- c("fid","distrito","area")
testpoints_industrial <- testpoints_industrial %>% mutate(fid = as.numeric(as.character(fid)))
industrial@data <- industrial@data %>% left_join(testpoints_industrial, by = "fid")
industrial$uso_suelo<-"Industrial"

###############################################################################
#Industrial y comercial

industrial_comercial_data <- subset(datos, uso_construccion=="Industrial y comercial") 
industrial_comercial <- subset(alc_us,alc_us$fid %in% industrial_comercial_data$fid)
testpoints_industrial_comercial <- matrix(nrow = nrow(industrial_comercial_data), ncol = 3) 

pb <- txtProgressBar(min = 0, max = nrow(industrial_comercial_data), style = 3)
for (i in 1:nrow(industrial_comercial_data)){
    if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(industrial_comercial_data)){
        setTxtProgressBar(pb, i)
    }
    polygon <-  geojson_sf(industrial_comercial_data$geo_shape[i])
    point <- st_centroid(polygon)
    point <- SpatialPoints(cbind(unlist(point$geometry)[1],unlist(point$geometry)[2]),proj4string=CRS(as.character("+proj=longlat +ellps=WGS84 +no_defs")))
    distrito <- as.character(over(point,dist,returnList = FALSE)$Distrito)
    testpoints_industrial_comercial[i,] <- cbind(as.character((industrial_comercial_data$fid[i])),distrito[1],area(industrial_comercial[i,1]))
    }

testpoints_industrial_comercial <- testpoints_industrial_comercial %>% as.data.frame()
colnames(testpoints_industrial_comercial) <- c("fid","distrito","area")
testpoints_industrial_comercial <- testpoints_industrial_comercial %>% mutate(fid = as.numeric(as.character(fid)))
industrial_comercial@data <- industrial_comercial@data %>% left_join(testpoints_industrial_comercial, by = "fid")
industrial_comercial$uso_suelo<-"Industrial y comercial"

###############################################################################
#Equipamientos

equipamiento_data <- subset(datos, uso_construccion=="Equipamiento") 
equipamiento <- subset(alc_us,alc_us$fid %in% equipamiento_data$fid)
testpoints_equipamiento <- matrix(nrow = nrow(equipamiento_data), ncol = 3) 

pb <- txtProgressBar(min = 0, max = nrow(equipamiento_data), style = 3)
for (i in 1:nrow(equipamiento_data)){
    if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(equipamiento_data)){
        setTxtProgressBar(pb, i)
    }
    polygon <-  geojson_sf(equipamiento_data$geo_shape[i])
    point <- st_centroid(polygon)
    point <- SpatialPoints(cbind(unlist(point$geometry)[1],unlist(point$geometry)[2]),proj4string=CRS(as.character("+proj=longlat +ellps=WGS84 +no_defs")))
    distrito <- as.character(over(point,dist,returnList = FALSE)$Distrito)
    testpoints_equipamiento[i,] <- cbind(as.character((equipamiento_data$fid[i])),distrito[1],area(equipamiento[i,1]))
    }

testpoints_equipamiento <- testpoints_equipamiento %>% as.data.frame()
colnames(testpoints_equipamiento) <- c("fid","distrito","area")
testpoints_equipamiento <- testpoints_equipamiento %>% mutate(fid = as.numeric(as.character(fid)))
equipamiento@data <- equipamiento@data %>% left_join(testpoints_equipamiento, by = "fid")
equipamiento$uso_suelo<-"Equipamiento"

###############################################################################
#Áreas verdes

areas_verdes_data <- subset(datos, uso_construccion=="Áreas Verdes") 
areas_verdes <- subset(alc_us,alc_us$fid %in% areas_verdes_data$fid)
testpoints_areas_verdes <- matrix(nrow = nrow(areas_verdes_data), ncol = 3) 

pb <- txtProgressBar(min = 0, max = nrow(areas_verdes_data), style = 3)
for (i in 1:nrow(areas_verdes_data)){
    if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(areas_verdes_data)){
        setTxtProgressBar(pb, i)
    }
    polygon <-  geojson_sf(areas_verdes_data$geo_shape[i])
    point <- st_centroid(polygon)
    point <- SpatialPoints(cbind(unlist(point$geometry)[1],unlist(point$geometry)[2]),proj4string=CRS(as.character("+proj=longlat +ellps=WGS84 +no_defs")))
    distrito <- as.character(over(point,dist,returnList = FALSE)$Distrito)
    testpoints_areas_verdes[i,] <- cbind(as.character((areas_verdes_data$fid[i])),distrito[1],area(areas_verdes[i,1]))
    }

testpoints_areas_verdes <- testpoints_areas_verdes %>% as.data.frame()
colnames(testpoints_areas_verdes) <- c("fid","distrito","area")
testpoints_areas_verdes <- testpoints_areas_verdes %>% mutate(fid = as.numeric(as.character(fid)))
areas_verdes@data <- areas_verdes@data %>% left_join(testpoints_areas_verdes, by = "fid")
areas_verdes$uso_suelo<-"Areas verdes"

###############################################################################
#Centros de barrio

centros_barrio_data <- subset(datos, uso_construccion=="Centro de Barrio") 
centros_barrio <- subset(alc_us,alc_us$fid %in% centros_barrio_data$fid)
testpoints_centros_barrio <- matrix(nrow = nrow(centros_barrio_data), ncol = 3) 

pb <- txtProgressBar(min = 0, max = nrow(centros_barrio_data), style = 3)
for (i in 1:nrow(centros_barrio_data)){
    if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(centros_barrio_data)){
        setTxtProgressBar(pb, i)
    }
    polygon <-  geojson_sf(centros_barrio_data$geo_shape[i])
    point <- st_centroid(polygon)
    point <- SpatialPoints(cbind(unlist(point$geometry)[1],unlist(point$geometry)[2]),proj4string=CRS(as.character("+proj=longlat +ellps=WGS84 +no_defs")))
    distrito <- as.character(over(point,dist,returnList = FALSE)$Distrito)
    testpoints_centros_barrio[i,] <- cbind(as.character((centros_barrio_data$fid[i])),distrito[1],area(centros_barrio[i,1]))
    }

testpoints_centros_barrio <- testpoints_centros_barrio %>% as.data.frame()
colnames(testpoints_centros_barrio) <- c("fid","distrito","area")
testpoints_centros_barrio <- testpoints_centros_barrio %>% mutate(fid = as.numeric(as.character(fid)))
centros_barrio@data <- centros_barrio@data %>% left_join(testpoints_centros_barrio, by = "fid")
centros_barrio$uso_suelo<-"Centros de barrio"

###############################################################################
#Sin zonificación

sin_zonificacion_data <- subset(datos, uso_construccion=="Sin Zonificación") 
sin_zonificacion <- subset(alc_us,alc_us$fid %in% sin_zonificacion_data$fid)
testpoints_sin_zonificacion <- matrix(nrow = nrow(sin_zonificacion_data), ncol = 3) 

pb <- txtProgressBar(min = 0, max = nrow(sin_zonificacion_data), style = 3)
for (i in 1:nrow(sin_zonificacion_data)){
    if (sample(1:40, 1)==sample(1:40, 1) || i==nrow(sin_zonificacion_data)){
        setTxtProgressBar(pb, i)
    }
    polygon <-  geojson_sf(sin_zonificacion_data$geo_shape[i])
    point <- st_centroid(polygon)
    point <- SpatialPoints(cbind(unlist(point$geometry)[1],unlist(point$geometry)[2]),proj4string=CRS(as.character("+proj=longlat +ellps=WGS84 +no_defs")))
    distrito <- as.character(over(point,dist,returnList = FALSE)$Distrito)
    testpoints_sin_zonificacion[i,] <- cbind(as.character((sin_zonificacion_data$fid[i])),distrito[1],area(sin_zonificacion[i,1]))
    }

testpoints_sin_zonificacion <- testpoints_sin_zonificacion %>% as.data.frame()
colnames(testpoints_sin_zonificacion) <- c("fid","distrito","area")
testpoints_sin_zonificacion <- testpoints_sin_zonificacion %>% mutate(fid = as.numeric(as.character(fid)))
sin_zonificacion@data <- sin_zonificacion@data %>% left_join(testpoints_sin_zonificacion, by = "fid")
sin_zonificacion$uso_suelo<-"Sin Zonificacion"

###############################################################################

df<-habitacional@data
df<-df %>% rbind(habitacional_comercial@data)
df<-df %>% rbind(industrial@data)
df<-df %>% rbind(industrial_comercial@data)
df<-df %>% rbind(equipamiento@data)
df<-df %>% rbind(areas_verdes@data)
df<-df %>% rbind(centros_barrio@data)
df<-df %>% rbind(sin_zonificacion@data)

distritos<- distinct(df,distrito, .keep_all = TRUE)$distrito
dist <- subset(dist,as.character(dist$Distrito) %in% as.character(distritos))

###############################################################################

write.csv(df,paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/",alcaldia,"/",alcaldia,"_us.csv"), row.names = FALSE)

###############################################################################

map <- leaflet()
map <- addTiles(map)

map <- map %>%

    addPolygons(data= alc,
                stroke = TRUE,
                color = 'blue',
                opacity=1, 
                smoothFactor = 0.5,
                weight = 7,
                fillColor = "#2062e6",
                fillOpacity = 0.1,
                popup = paste(
                    '<br><b>Clave municipio/alcaldía: </b>', paste(alc$CVEGEO)
                ))

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
map <- map %>%

    addPolygons(data= habitacional,
                stroke = TRUE, 
                color="#105166",
                smoothFactor = 0.5,
                weight = 0.3,
                fillOpacity = 0.6, 
                fillColor = "#26b8e8",
                popup = paste(
                    '<br><b>Clave fid: </b>', paste(habitacional$fid),
                    '<br><br><b>Distrito: </b>', paste(habitacional$distrito)
                ))

map <- map %>%

    addPolygons(data= habitacional_comercial,
                stroke = TRUE, 
                color="#bd4913",
                smoothFactor = 0.5,
                weight = 0.3,
                fillOpacity = 0.6, 
                fillColor = "#dea318",
                popup = paste(
                    '<br><b>Clave fid: </b>', paste(habitacional_comercial$fid),
                    '<br><br><b>Distrito: </b>', paste(habitacional_comercial$distrito)
                ))

map <- map %>%

    addPolygons(data= industrial,
                stroke = TRUE, 
                color="#170f42",
                smoothFactor = 0.5,
                weight = 0.3,
                fillOpacity = 0.6, 
                fillColor = "#3c29ab",
                popup = paste(
                    '<br><b>Clave fid: </b>', paste(industrial$fid),
                    '<br><br><b>Distrito: </b>', paste(industrial$distrito)
                ))

map <- map %>%

    addPolygons(data= industrial_comercial,
                stroke = TRUE, 
                color="#170f42",
                smoothFactor = 0.5,
                weight = 0.3,
                fillOpacity = 0.6, 
                fillColor = "#3c29ab",
                popup = paste(
                    '<br><b>Clave fid: </b>', paste(industrial_comercial$fid),
                    '<br><br><b>Distrito: </b>', paste(industrial_comercial$distrito)
                ))

map <- map %>%

    addPolygons(data= equipamiento,
                stroke = TRUE, 
                color="#170f42",
                smoothFactor = 0.5,
                weight = 0.3,
                fillOpacity = 0.6, 
                fillColor = "#3c29ab",
                popup = paste(
                    '<br><b>Clave fid: </b>', paste(equipamiento$fid),
                    '<br><br><b>Distrito: </b>', paste(equipamiento$distrito)
                ))

map <- map %>%

    addPolygons(data= areas_verdes,
                stroke = TRUE, 
                color="#044d0e",
                smoothFactor = 0.5,
                weight = 0.3,
                fillOpacity = 0.6, 
                fillColor = "#07911a",
                popup = paste(
                    '<br><b>Clave fid: </b>', paste(areas_verdes$fid),
                    '<br><br><b>Distrito: </b>', paste(areas_verdes$distrito)
                ))

map <- map %>%

    addPolygons(data= centros_barrio,
                stroke = TRUE, 
                color="#044d0e",
                smoothFactor = 0.5,
                weight = 0.3,
                fillOpacity = 0.6, 
                fillColor = "#07911a",
                popup = paste(
                    '<br><b>Clave fid: </b>', paste(centros_barrio$fid),
                    '<br><br><b>Distrito: </b>', paste(centros_barrio$distrito)
                ))

map <- map %>%

    addPolygons(data= sin_zonificacion,
                stroke = TRUE, 
                color="#2b2b2a",
                smoothFactor = 0.5,
                weight = 0.3,
                fillOpacity = 0.6, 
                fillColor = "#868783",
                popup = paste(
                    '<br><b>Clave fid: </b>', paste(sin_zonificacion$fid),
                    '<br><br><b>Distrito: </b>', paste(sin_zonificacion$distrito)
                ))


###############################################################################



setwd(paste0("C:/Users/DELL/OneDrive/CodeLibrary/R/ZMVM_Urbanizacion/",alcaldia)) 
#saveWidget(map, file=paste0(alcaldia,"_map.html"))
