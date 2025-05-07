# Análise de Métricas de Paisagem - Manaus ----
# Autor: Carson Silveira

# Pacotes necessários
library(sf)  # Para trabalhar com dados espaciais (Shapefiles)
library(raster)  # Para manipulação de dados raster
library(landscapemetrics) # Para calcular métricas de paisagem
library(patchwork)  # Para combinar múltiplos gráficos
library(tidyverse)  # Para manipulação de dados (dplyr, tidyr, etc.)
library(terra)  # Para manipulação de dados raster e vetoriais de forma eficiente
library(here)  # Para garantir a reprodutibiliade do código

# Caminhos dos dados ----

trees <- rast(here("data", "tree_cover_manaus.tif"))  # Raster de vegetação
bairros <- sf::st_read(here("data", "bairros_manaus.gpkg"))  # Vetor dos bairros de Manaus

# Reprojeção dos dados de WGS 84 (EPSG:4326) para SIRGAS200 (EPSG:3198) ----

trees_reproj <- project(trees, "EPSG:31980")  # Reprojeta o raster 
bairros <- st_transform(bairros, crs = 31980)  # Reprojeta o vetor

# Cortar vetor pela extensão do raster
extensao <- ext(trees_reproj)  # Captura a extensão do raster reprojetado
bairros_v <- vect(bairros)  # Converte o shapefile para objeto 'vect' do pacote terra
bairros_crop <- crop(bairros_v, extensao)  # Faz o corte do shapefile usando a extensão do raster

# Recalcular área dos bairros e perímetro ----
head(bairros_crop[, c("SHAPE_AREA", "SHAPE_LEN")])
bairros_crop$SHAPE_AREA = expanse(bairros_crop)
bairros_crop$SHAPE_LEN = perim(bairros_crop)
head(bairros_crop[, c("SHAPE_AREA", "SHAPE_LEN")])

# Visualizando as camadas reprojetadas ----
plot(trees_reproj)  # 1 = arbóreo e 0 = não-arbóreo 
plot(bairros_crop, add = TRUE, border = "red")

# Cálculo das métricas de paisagem por bairro ----

pland_landscapes = sample_lsm(trees_reproj, bairros_crop,
                              plot_id = bairros_crop$NOME_BAIRR,  
                              what = c("lsm_c_pland", "lsm_c_ca")) %>%  # Seleciona as métricas PLAND e CA
  dplyr::filter(class == 1)  # Filtra para selecionar apenas a classe 1 (vegetação)

# Mudando o formato da tabela e transformando os resultados do CA----

pland_landscapes_w = pivot_wider(pland_landscapes,
                                 id_cols = plot_id,
                                 names_from = metric,
                                 values_from = value)  

#pland_landscapes_w$ca <- pland_landscapes_w$ca * 10000  # Transformando hectare em m2

# Salvando a tabela ----
write.csv(pland_landscapes_w, "~/manaus_lsm_osm.csv")

# Unindo as métricas de paisagem ao shapefile ----

bairros = bairros %>% 
  rename(plot_id = NOME_BAIRR)  # Renomeia a coluna para facilitar a junção

bairros_lsm = left_join(bairros, pland_landscapes_w, by = "plot_id")  # junção das métricas

# Visualizando as métricas no mapa ----
plot(bairros_lsm["pland"])  # pland = percentagem de vegetação em cada bairro
plot(bairros_lsm["ca"])  # área de vegetação em cada bairro em m2

# Salvando vetor ----
st_write(bairros_lsm, "~/manaus_lsm_osm.gpkg")
