# Jornalismo de Dados e Biodiversidade - Nicoly Ambrósio

Este repositório contém os dados do projeto de invenstigação desenvolvido no contexto do programa de **Jornalismo de Dados e Biodiversidade** do Serrapilheira. O objetivo principal é analisa a relação entre cobertura arbórea, sensação térmica, saúde e políticas públicas para o meio ambiente, destacando as desigualdades socioeconômicas no acesso a espaços verdes (parques e praças) e comparando a situação de Manaus com outras cidades amazônicas. O script `main.R` realiza todo o processamento necessário, e os resultados gerados são armazenados em arquivos de saída para análise posterior.

## Estrutura do Repositório

- `scripts/`: Contém os scripts responsáveis por realizar as análises e processamentos dos dados;
  - `main.R`: Script principal que realiza a análise de métricas de paisagem. O script carrega dados espaciais, realiza o pré-processamento e calcula as métricas de paisagem;
- `output/`: Contém os resultados da análise, incluindo as métricas de paisagem calculadas e gráficos gerados;
- `README.md`: Este arquivo

## Dados Utilizados

Este projeto fez uso de uma base de dados de Cobertura de Arborização Urbana chamada **UTB Dataset** desenvolvida por [Guo et al., (2021)](https://www.sciencedirect.com/science/article/pii/S0924271623000461). Os dados do UTB Dataset estão disponíveis publicamente no GitHub e podem ser acessados pelo seguinte link:

- [UTB Dataset - GitHub](https://github.com/usuario/UTB-Dataset)

Os arquivos de entrada utilizados nas análises deste projeto podem ser acessados e baixados através do seguinte link:

- [Grupo de Suporte Científico e Reportagens - Nicoly Ambrosion](https://drive.google.com/drive/folders/1G3AtjNxgz4qDBg2yRQWulnaxNG9BKE4Z?usp=drive_link)

## Requisitos

Para rodar o projeto, você precisará do R e de alguns pacotes específicos. Você pode instalar os pacotes necessários executando o seguinte código no R:

```r
install.packages(c("sf", "raster", "landscapemetrics", "patchwork", "tidyverse", "terra", "here"))
