setwd('E:/Ing-Palacios')

# Instalacion de paquetes
install.packages("stringr")
install.packages('maxnet')

#Cargando librerias
library(readxl)
library(dbplyr)
library(dismo)
library(sf)
library(sp)
library(rgdal)
library(raster)
library(maxnet)
library(rworldxtra) # countriesHigh
library(stringr) # Usar la funcion str_remove_all

# Cargar datos de amfibios
dataset <- read_excel("ORPBDD-registros-anfibios-libro-loreto-20220405.xlsx")

# Cargar shp de provincias 
prov <- readOGR('Amfibios/SHP/Limite_Provincial/Limite_provincial.shp')
plot(prov)
# Seleccion de coordenadas de longitud y latitud
data_fil <- filter(dataset, ESPECIE=='femoralis')
data_coord <- select(data_fil, LON, LAT) %>% as.data.frame()
class(data_coord)
dataset_sf <- data_coord %>% st_as_sf(coords = c(1, 2),
                                      crs = "EPSG:4326") # na.fail=FALSE
plot(dataset_sf)

#Generar un buffer
Hull <- dataset_sf %>% st_union() %>% st_convex_hull()
#Buffer
Buffer <- Hull %>% st_buffer(dist = 1) %>% st_as_sf()

#background
data("countriesHigh")
SA <- countriesHigh %>% st_as_sf() %>% st_make_valid() %>% st_crop(Buffer)

prov_sf <- prov %>% st_as_sf()
plot(prov)


#Bajamos capas bioclimaticas de resolucion de 1cm
Bioclimatic <- getData(name = "worldclim", var = "bio", res = 0.5,
                       lon = -80, lat = -20)

plot(Bioclimatic)

## capas climaticas
Bioclimatic <- Bioclimatic %>% crop(Buffer) %>% trim()
plot(Bioclimatic)

## Cortamos el Tile
names(Bioclimatic) <- str_remove_all(names(Bioclimatic), "_43")

plot(Bioclimatic[[1]])
ras <- Bioclimatic[[12]] %>% as.data.frame(xy = TRUE)

ggplot()+
  geom_raster(data = ras, aes(x = x, y = y, fill = bio12_33))+
  geom_sf(data = SA, fill = 'transparent')+
  scale_fill_viridis_c(name = 'mm/yr', direction = -1)+
  geom_sf(data = dataset_sf, 
          pch = 21, 
          bg = "red",   # Color de fondo
          col = "white", # Color del borde
          cex = 2)+      # Tamaño del símbolo
  geom_sf(data = prov_sf, fill = 'transparent')+
  theme_bw()+
  labs(x='Logitud', y='Latitud', title = 'Distribucion de especies de femoralis', subtitle = 'Registro de amfibios')








## Generamos un Background
set.seed(2020)

#Presencia
Pres <- data_fil %>% dplyr::select(LON, LAT) %>%
  mutate(Pres = 1)

#Ausencia
Aus <- dismo::randomPoints(mask = Bioclimatic[[1]], n = 5000) %>%
  as.data.frame() %>% rename(LON = x, LAT = y) %>%
  mutate(Pres = 0)

#Unir presencia y ausencia
Pres_Aus <- bind_rows(Pres, Aus)

Pres_Aus_Sf <- Pres_Aus %>% st_as_sf(coords = c(1, 2), crs = "EPSG:4326")
plot(Pres_Aus_Sf)

#Extraccion de datos
Condiciones <- raster::extract(Bioclimatic, Pres_Aus_Sf) %>%
  as.data.frame() %>% bind_cols(Pres_Aus)


#Empezamos a modelar
set.seed(2020)
Mod1 <- maxnet(p = Condiciones$Pres,
               data = Condiciones[, 1:19],
               regmult = 1,
               maxnet.formula(p = Condiciones$Pres, data = Condiciones[, 1:19], classes = "lq"))
View(Mod1)
