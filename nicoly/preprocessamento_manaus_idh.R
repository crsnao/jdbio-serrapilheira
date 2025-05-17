# Análise Espacial e Socioeconômica da Cidade de Manaus ---
# Análise feita para explorar as relações entre variáveis ambientais e socioeconômicas
# Autor: Carson Silveira

# Carrega pacotes necessários para manipulação de dados, leitura de arquivos e análise espacial
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(sf)

# Lê um arquivo GeoPackage contendo dados espaciais da cidade de Manaus
manaus = st_read("/home/carson/R/nicoly_ma_utc/manaus_nicoly.gpkg")

# Lê uma planilha do Excel contendo dados do IDH e renomeia a coluna para compatibilidade no join
idh = readxl::read_excel("/home/carson/R/nicoly_ma_utc/idh.xlsm") %>% 
  rename(ZONAS = NOME_REG)

# Junta os dados espaciais com os dados do IDH
df = left_join(manaus, idh, by = ZONAS)  

# Salva o shapefile resultante da junção
write_sf(df, "/home/carson/R/nicoly_ma_utc/manaus_nicoly2.gpkg")

# Agrupa os dados por densidade e calcula a média das colunas numéricas para cada grupo
df_regioes_D <- df %>%
  group_by(ZONAS) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

# Tenta salvar `df_regioes`, mas o objeto correto é `df_regioes_D`
write_sf(df_regioes, "/home/carson/R/nicoly_ma_utc/manaus_regiões.gpkg")
