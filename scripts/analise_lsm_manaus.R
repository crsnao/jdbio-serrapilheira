# Análise das Paisagem de Manaus ----
# Autor: Carson Silveira

# Pacotes necessários
library(tidyverse)
library(patchwork)

# Comparação dos dados com e sem dados OSM ----
# Lendo os resultados sem e com OSM ----
mn_ = read.csv("~/resultados_JDBio_nicolly_lsm/manaus_lsm.csv") %>% 
  rename(pland_norm = pland, 
         ca_norm = ca)

mn_osm = read.csv("~/resultados_JDBio_nicolly_lsm/manaus_lsm_osm.csv") %>% 
  rename(pland_osm = pland, 
         ca_osm = ca)

# Junção dos dados numa única tabela ----
mn = left_join(mn_osm, mn_, "plot_id")
mn$X.x = NULL  # Remove a coluna desnecessária
mn$X.y = NULL  # Remove a coluna desnecessária

# Alterando formato da tabela para facilitar a construção do gráfico ----
mn.long = mn %>% 
  pivot_longer(cols = - plot_id,
               names_to = c(".value", "lsm"), 
               names_sep = "_") %>% 
  mutate(plot_id = factor(plot_id, levels = sort(unique(plot_id))))

# Criando o gráfico de comparação de 'pland' (percentagem de vegetação) ----
p = ggplot(mn.long, aes(x = plot_id, y = pland, fill = lsm)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_minimal() +
  theme(axis.text.x = element_text (angle = 90, vjust = 0.5, hjust = 1))

# Criando o gráfico de comparação de 'ca' (área de vegetação) ----
c = ggplot(mn.long, aes(x = plot_id, y = ca, fill = lsm)) +
  geom_bar(stat = "identity", position = "dodge") +  
  theme_bw() +
  theme(axis.text.x = element_text (angle = 90, vjust = 0.5, hjust = 1))

comparacao = p/c # Visualizar gráficos

# Salvando o gráfico ----
ggsave("comparação_ca.png", plot = c, width = 10, height = 6, dpi = 300)
ggsave("comparação_pland.png", plot = p, width = 10, height = 6, dpi = 300)

