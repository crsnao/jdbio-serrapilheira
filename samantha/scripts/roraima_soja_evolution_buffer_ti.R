library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Função para extrair o nome da TI do nome do arquivo
extrair_nome_ti <- function(caminho) {
  nome_arquivo <- basename(caminho)
  nome_ti <- nome_arquivo %>%
    str_remove("value85_buffer_terrai_cod_") %>%
    str_remove("values23_buffer_terrai_cod_") %>%
    str_remove(".csv")
  return(nome_ti)
}

# Função para processar um arquivo e extrair os hectares da classe desejada
processa_arquivo <- function(caminho, classe_alvo, ano) {
  read_csv(caminho, show_col_types = FALSE) %>%
    filter(value == classe_alvo) %>%
    summarise(hectare = sum(m2, na.rm = TRUE) / 10000) %>%  # converte m² para ha 
    mutate(
      terrai_cod = extrair_nome_ti(caminho),
      ano = ano
    ) %>%
    select(terrai_cod, hectare, ano)
}

# Caminhos dos arquivos
arquivos_85 <- list.files("/home/carson/R/samantha_rr_ti/output/tables/1985", pattern = "value85.*\\.csv$", full.names = TRUE)
arquivos_23 <- list.files("/home/carson/R/samantha_rr_ti/output/tables/2023", pattern = "values23.*\\.csv$", full.names = TRUE)

# Classe que você quer extrair — você pode alterar esse valor
classe_escolhida <- 39  # Ex: 3 = Floresta (depende da codificação do MapBiomas)

# Lê e processa todos os arquivos de 1985
dados_85 <- map_dfr(arquivos_85, ~ processa_arquivo(.x, classe_escolhida, 1985))

# Lê e processa todos os arquivos de 2023
dados_23 <- map_dfr(arquivos_23, ~ processa_arquivo(.x, classe_escolhida, 2023))

# Junta os dois anos
dados_completos <- bind_rows(dados_85, dados_23)

# Reorganiza os dados para ter uma coluna por ano
tabela_final <- dados_completos %>%
  pivot_wider(names_from = ano, values_from = hectare, values_fill = 0) %>%
  arrange(terrai_cod)

# Salva como CSV
write_csv(tabela_final, "/home/carson/R/samantha_rr_ti/output/tables/evolucao_soja_tis_85_23.csv")
