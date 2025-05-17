# ------------------------------------------------------------
# Script: Mapeamento de UCs e TIs com buffers no estado de Roraima
# Autor: CARSON SILVEIRA
# Descrição: Este script carrega e processa dados geográficos de Unidades de
#            Conservação (UCs) e Terras Indígenas (TIs) no estado de Roraima,
#            utilizando o pacote `geobr`. O script corrige geometrias inválidas,
#            faz recortes espaciais, reprojeta os dados para SIRGAS2000/UTM 20N
#            e gera mapas com e sem buffers de 10 km em torno dessas áreas.
# Data: 06 de maio de 2025
# ------------------------------------------------------------


# Carrega pacotes necessários
library(sf)
library(dplyr)
library(ggplot2)
library(terra)
library(geobr)

# Lista os conjuntos de dados disponíveis no pacote geobr (opcional)
datasets <- list_geobr()

# Lê o shapefile do estado de Roraima
roraima <- read_state(code_state = "RR")

# Lê as unidades de conservação (UCs) e terras indígenas (TIs) do Brasil
data_cu <- read_conservation_units()
data_il <- read_indigenous_land()

# Verifica e corrige geometrias inválidas nas terras indígenas
invalid_il <- !st_is_valid(data_il)                             # Identifica geometrias inválidas
data_il[invalid_il, ] <- st_make_valid(data_il[invalid_il, ])   # Corrige
invalid_cu <- !st_is_valid(data_cu)       
data_cu[invalid_cu, ] <- st_make_valid(data_cu[invalid_cu, ])

# Recorta UCs e TIs pela área de Roraima (clipping espacial)
roraima_cu <- st_intersection(data_cu, roraima)   # UCs dentro de RR
roraima_il <- st_intersection(data_il, roraima)   # TIs dentro de RR

# Mapa com ggplot2: estado, UCs e TIs
ggplot() +
  geom_sf(data = roraima, fill = NA, color = "black") +    # Contorno do estado
  geom_sf(data = roraima_cu, fill = "green", alpha = 0.4) + # UCs em verde
  geom_sf(data = roraima_il, fill = "red", alpha = 0.4) +   # TIs em vermelho
  labs(title = "Unidades de Conservação e Terras Indígenas em Roraima")

# Reprojeta para SIRGAS 2000 / UTM zone 20N (EPSG: 31980), adequado para Roraima
roraima_proj <- st_transform(roraima, 31980)
roraima_il_proj <- st_transform(roraima_il, 31980)
roraima_cu_proj <- st_transform(roraima_cu, 31980)

# Aplica buffer de 10 km
buffer_il <- st_buffer(roraima_il_proj, dist = 3000)
buffer_cu <- st_buffer(roraima_cu_proj, dist = 3000)

ggplot() +
  geom_sf(data = roraima_proj, fill = NA, color = "black") +
  geom_sf(data = buffer_cu, fill = "green", alpha = 0.2) +
  geom_sf(data = buffer_il, fill = "red", alpha = 0.2) +
  geom_sf(data = roraima_cu_proj, fill = "green", alpha = 0.6) +
  geom_sf(data = roraima_il_proj, fill = "red", alpha = 0.6) +
  labs(title = "Buffers de 10 km em torno de UCs e TIs em Roraima")
