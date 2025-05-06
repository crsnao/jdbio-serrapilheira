# Jornalismo de Dados e Biodiversidade

Este repositório contém os dados do projeto de invenstigação desenvolvido no contexto do grupo de Suporte Cientifico às reportagens do Programa de Jornalismo de Dados e Biodiversidade do Serrapilheira. O objetivo principal deste projeto é analisar a relação entre cobertura arbórea, sensação térmica, saúde e políticas públicas para o meio ambiente, destacando as desigualdades socioeconômicas no acesso a espaços verdes (parques e praças) e comparando a situação de Manaus com outras cidades amazônicas. O script `main.R` realiza todo o processamento necessário, e os resultados gerados são armazenados em arquivos de saída para análise posterior.

## Estrutura do Repositório para cada um dos jornalistas que trabalhei

- `Nicoly`: relação entre cobertura arbórea, sensação térmica, saúde e políticas públicas para o meio ambiente, destacando as desigualdades socioeconômicas em cidades amazônicas
- `Samantha`: O impacto do agronegócio e expansão da soja no modo de vida e na biodiversidade de terras indígenas demarcadas em ilhas em Roraima
- `Diego`: Aumento da mineração ilegal e do crime organizado na cidade de Tena, Equador, e em sua zona de influência, como um elemento determinante para o aumento da temperatura

### Dentro de cada pasta estão incluidos as seguintes informações

-  `scripts/`: Contém os scripts responsáveis por realizar as análises e processamentos dos dados
  - `main.R`: Script principal que realiza a análise das métricas de paisagem
- `output/`: Os resultados da análise, incluindo as métricas de paisagem calculadas e gráficos gerados
  - `figures/`: Gráficos e figuras do projeto, como histogramas, boxplots e outros tipos de visualização
  - `maps/`: Arquivos vetoriais e rasters resultantes da análise espacial
  - `tables/`: Tabelas de resultados com métricas calculadas, dados resumidos
- `docs/`: Outros documentos (ainda vou atualizar, ainda to pra organizar os metadados)
- `README.md`: Este arquivo

## Dados Utilizados

Este projeto fez uso de uma base de dados de Cobertura de Arborização Urbana chamada **UTB Dataset** desenvolvida por [Guo et al., (2021)](https://www.sciencedirect.com/science/article/pii/S0924271623000461). Os dados do UTB Dataset estão disponíveis publicamente no GitHub e podem ser acessados pelo seguinte link:

- [UTB Dataset - GitHub]([https://github.com/usuario/UTB-Dataset](https://nkszjx.github.io/projects/UTB.html))

Os arquivos de entrada utilizados nas análises deste projeto podem ser acessados e baixados através do seguinte link (ainda não ta completo):

- [Grupo de Suporte Científico e Reportagens - Nicoly Ambrosion](https://drive.google.com/drive/folders/1G3AtjNxgz4qDBg2yRQWulnaxNG9BKE4Z?usp=drive_link)

## Requisitos

Para rodar o projeto, você precisará do R e de alguns pacotes específicos. Você pode instalar os pacotes necessários executando o seguinte código no R:

```r
install.packages(c("sf", "raster", "landscapemetrics", "patchwork", "tidyverse", "terra", "here"))
