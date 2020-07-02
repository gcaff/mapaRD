library(rgdal)
library(rgeos)
library(dplyr)
library(cartography)

##### República Dominicana

########### Region ###########
# importar SPDF
rd_spdf_raw <- readOGR(
  dsn= paste0("~/Documents/PascalAnalytics/proyectos/MapaPoliticoRD/mapa RD/ShapeFilesCenso2010") ,
  layer="REGCenso2010",
  verbose=FALSE
)

rd_spdf2 <- rd_spdf_raw
rd_spdf1 <- gSimplify(rd_spdf_raw, 100, topologyPreserve = TRUE) # reducir tamanio del SPDF

# introducir poligonos reducidos al SPDF original (que contiene la data)
rd_spdf2@polygons <- rd_spdf1@polygons
rd_spdf <- spTransform(rd_spdf2, CRS("+init=EPSG:4602")) # transformar a EPSG local

# introducir variable ID
rd_spdf@data <- rd_spdf@data %>%
  mutate(ID=REG,
         ID2=1:nrow(rd_spdf@data),
         NIVEL="REG")

reg_spdf <- rd_spdf # base
reg_spdf_fort <- broom::tidy(reg_spdf,region="ID") # fortify for ggplot

reg_spdf_fort <- reg_spdf_fort %>%
  dplyr::rename(ID=id) %>%
  mutate(ID=as.numeric(ID)) %>%
  dplyr::left_join(reg_spdf@data, by=c("ID"="ID2")) %>%
  dplyr::rename(ID2=ID,
                ID=ID.y)

########### Provincia ###########
# importar SPDF
rd_spdf_raw <- readOGR(
  dsn= paste0("~/Documents/PascalAnalytics/proyectos/MapaPoliticoRD/mapa RD/ShapeFilesCenso2010") ,
  layer="PROVCenso2010",
  verbose=FALSE
)

rd_spdf2 <- rd_spdf_raw
rd_spdf1 <- gSimplify(rd_spdf_raw, 100, topologyPreserve = TRUE) # reducir tamanio del SPDF

# introducir poligonos reducidos al SPDF original (que contiene la data)
rd_spdf2@polygons <- rd_spdf1@polygons
rd_spdf <- spTransform(rd_spdf2, CRS("+init=EPSG:4602")) # transformar a EPSG local

# introducir variable ID
rd_spdf@data <- rd_spdf@data %>%
  rename(ID=ENLACE) %>%
  mutate(ID2=1:nrow(rd_spdf@data),
         NIVEL="PROV")

prov_spdf <- rd_spdf

prov_spdf_fort <- broom::tidy(prov_spdf)
prov_spdf_fort <- prov_spdf_fort %>%
  dplyr::rename(ID=id) %>%
  mutate(ID=as.numeric(ID)+1) %>%
  dplyr::left_join(prov_spdf@data, by=c("ID"="ID2")) %>%
  dplyr::rename(ID2=ID,
                ID=ID.y)

########### Municipio ###########
# importar SPDF
rd_spdf_raw <- readOGR(
  dsn= paste0("~/Documents/PascalAnalytics/proyectos/MapaPoliticoRD/mapa RD/ShapeFilesCenso2010") ,
  layer="MUNCenso2010",
  verbose=FALSE
)

rd_spdf2 <- rd_spdf_raw
rd_spdf1 <- gSimplify(rd_spdf_raw, 100, topologyPreserve = TRUE) # reducir tamanio del SPDF

# introducir poligonos reducidos al SPDF original (que contiene la data)
rd_spdf2@polygons <- rd_spdf1@polygons
rd_spdf <- spTransform(rd_spdf2, CRS("+init=EPSG:4602")) # transformar a EPSG local

# introducir variable ID
rd_spdf@data <- rd_spdf@data %>%
  rename(ID=ENLACE) %>%
  mutate(ID2=1:nrow(rd_spdf@data),
         NIVEL="MUN")

mun_spdf <- rd_spdf

mun_spdf_fort <- broom::tidy(mun_spdf)

mun_spdf_fort <- mun_spdf_fort %>%
  dplyr::rename(ID=id) %>%
  mutate(ID=as.numeric(ID)+1) %>%
  dplyr::left_join(mun_spdf@data, by=c("ID"="ID2")) %>%
  dplyr::rename(ID=ID.y,
                ID2=ID)

######### Tabla Toponimia #########

tabla_toponimia <- list(regional = reg_spdf@data,
                        provincial = prov_spdf@data,
                        municipal = mun_spdf@data)

######### Guardar Vars ###########
# guardar variables de los mapas
save(reg_spdf,prov_spdf,mun_spdf, file="data/mapasRD_spdf.RData")
save(reg_spdf_fort,prov_spdf_fort,mun_spdf_fort, file="data/mapasRD_spdf_fort.RData")
save(tabla_toponimia, file="data/tabla_toponimia.RData")
