# Carrega pacotes necessários
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# Lê os dados de uso da terra em 1985 dentro das terras indígenas (TI)
ti85 = read.csv("samantha_rr_ti/output/tables/values_reporttarget_ti_s_buffer_mapbiomas_1985.csv") %>% 
  rename_with(~ paste0(., "_ti85"), .cols = -value)

# Lê os dados da zona de buffer das TIs em 1985 e renomeia colunas para diferenciar
ti85_buffer = read.csv("samantha_rr_ti/output/tables/values_reporttarget_ti_mapbiomas_1985.csv") %>% 
  rename_with(~ paste0(., "_buffer85"), .cols = -value)

# Lê os dados de uso da terra em 2023 dentro das TIs
ti23 = read.csv("samantha_rr_ti/output/tables/values_reporttarget_ti_s_buffer_mapbiomas_2023.csv") %>% 
  rename_with(~ paste0(., "_ti23"), .cols = -value)

# Lê os dados da zona de buffer das TIs em 2023 e renomeia colunas para diferenciar
ti23_buffer = read.csv("samantha_rr_ti/output/tables/values_reporttarget_ti_mapbiomas_2023.csv") %>% 
  rename_with(~ paste0(., "_buffer23"), .cols = -value)

# Junta os dados de 1985 e 2023, reorganiza em formato longo e trata valores ausentes
compare_ti85_23 = ti85 %>% 
  full_join(ti23, by = "value") %>% 
  select(value, hectare_ti85, hectare_ti23) %>%
  pivot_longer(cols = contains("_ti"),  # reorganiza colunas para 'ano' e 'hectares'
               names_to = "ano", 
               names_prefix = "ano_", 
               values_to = "hectares") %>%
  replace_na(list(hectares = 0))  # preenche NAs com 0
write_csv(compare_ti85_23, "samantha_rr_ti/target_ti_roraima_comparacao_85_23.csv") # salvando as informações em tabela

# Une os dados da TI e do buffer em 1985 e calcula a área do buffer externo (fora da TI)
buffer85 = ti85 %>% 
  full_join(ti85_buffer, by = "value") %>% 
  mutate(buffer85_ha = hectare_buffer85 - hectare_ti85)

# Repete o mesmo processo para 2023
buffer23 = ti23 %>% 
  full_join(ti23_buffer, by = "value") %>% 
  mutate(buffer23_ha = hectare_buffer23 - hectare_ti23)

# Junta os dados de 1985 e 2023, reorganiza em formato longo e trata valores ausentes
compare_buffer85_23 = buffer85 %>% 
  full_join(buffer23, by = "value") %>% 
  select(value, buffer85_ha, buffer23_ha) %>%
  pivot_longer(cols = starts_with("buffer"),  # reorganiza colunas para 'ano' e 'hectares'
               names_to = "ano", 
               names_prefix = "ano_", 
               values_to = "hectares") %>%
  replace_na(list(hectares = 0))  # preenche NAs com 0
write_csv(compare_buffer85_23, "samantha_rr_ti/target_ti_buffer_roraima_comparacao_85_23.csv") # salvando as informações em tabela

# Gera gráfico da série histórica de uso da terra no buffer externo das TIs
plot = ggplot(compare_buffer85_23, aes(x = ano, y = hectares, color = factor(value), group = value)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    x = "Ano",
    y = "Área (hectares)",
    color = "Uso da Terra",
    title = "Série histórica de uso da terra"
  ) +
  theme_minimal()

plot