
## Nicoly Ambrosio

A investigação analisa a relação entre cobertura arbórea, sensação térmica, saúde e políticas públicas para o meio ambiente, destacando as desigualdades socioeconômicas no acesso a espaços verdes (parques e praças) e comparando a situação de Manaus com outras cidades amazônicas.

## Dados Utilizados

Este projeto fez uso de uma base de dados de Cobertura de Arborização Urbana chamada **UTB Dataset** desenvolvida por [Guo et al., (2021)](https://www.sciencedirect.com/science/article/pii/S0924271623000461). Os dados do UTB Dataset estão disponíveis publicamente no GitHub e podem ser acessados pelo seguinte link:

- [UTB Dataset - GitHub]([https://github.com/usuario/UTB-Dataset](https://nkszjx.github.io/projects/UTB.html))

Os arquivos de entrada utilizados nas análises deste projeto podem ser acessados e baixados através do seguinte link (ainda não ta completo):

- [Grupo de Suporte Científico e Reportagens - Nicoly Ambrosion](https://drive.google.com/drive/folders/1G3AtjNxgz4qDBg2yRQWulnaxNG9BKE4Z?usp=drive_link)

## Requisitos

Para rodar o projeto, você precisará do R e de alguns pacotes específicos. Você pode instalar os pacotes necessários executando o seguinte código no R:

```r
install.packages(c("sf", "raster", "landscapemetrics", "patchwork", "tidyverse", "terra", "here"))
