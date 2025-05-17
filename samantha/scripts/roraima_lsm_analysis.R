# ------------------------------------------------------------
# Script: Cálculo de métricas de paisagem em zonas de buffer de TIs
# Autor: CARSON SILVEIRA
# Descrição: Este script carrega dados geoespaciais de uso e cobertura da terra
#            (MapBiomas) e das Terras Indígenas (TIs) em Roraima, reprojeta os
#            dados para o sistema SIRGAS2000/20S e calcula métricas de paisagem,
#            especificamente a área total (TA), dentro das zonas de buffer das TIs
#            para os anos de 1985 e 2023.
# Data: 09 de maio de 2025
# ------------------------------------------------------------


# Carregamento de pacotes
library(sf)
library(terra)
library(landscapemetrics)
library(dplyr)
library(ggplot2)

# --- 1. Leitura dos dados geográficos ---

# Terras Indígenas em Roraima (formato GeoPackage)
ti <- st_read("samantha_rr_ti/ti_rr.gpkg")

# Buffer das terras indígenas (também GeoPackage)
buffer <- st_read("samantha_rr_ti/ti_rr_buffer.gpkg")

# Uso e cobertura da terra (MapBiomas) para os anos de 1985 e 2023
lulc85_wgs <- terra::rast("samantha_rr_ti/mapbiomas-brazil-collection-90-roraimarr-1985.tif")
lulc23_wgs <- terra::rast("samantha_rr_ti/mapbiomas-brazil-collection-90-roraimarr-2023.tif")

# Reprojeção do uso e cobertura para SIRGAS2000/20S
lulc85 <- project(lulc85_wgs, "EPSG:31980")
lulc23 <- project(lulc23_wgs, "EPSG:31980")

# --- 2. Cálculo das métricas de paisagem dentro dos buffers das TIs ---

# Métrica de área total (TA) para o ano de 1985
metricas85 <- sample_lsm(landscape = lulc85, 
                         y = buffer, 
                         plot_id = buffer$plot_id, 
                         what = "lsm_l_ta")

# Métrica de área total (TA) para o ano de 2023
metricas23 <- sample_lsm(landscape = lulc23, 
                         y = buffer, 
                         plot_id = buffer$plot_id, 
                         what = "lsm_l_ta")
