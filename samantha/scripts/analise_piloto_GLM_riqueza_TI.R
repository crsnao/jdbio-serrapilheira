getwd()
dir()
#### Instalar e carregar pacotes ####
if (!require("performance")) install.packages("performance"); library(performance)
if (!require("DHARMa")) install.packages("DHARMa"); library(DHARMa)
if (!require("car")) install.packages("car"); library(car)
if (!require("corrplot")) install.packages("corrplot")
if (!require("visreg")) install.packages("visreg")
if (!require("MASS")) install.packages("MASS"); library(MASS)
library(visreg)
library(corrplot)
library(here)
library(sf)        # para ler e manipular shapefiles
library(dplyr)     # para manipulação de dados
library(readr)     # para ler o CSV
library(tidyr)     # para formatar a saída
library(purrr)
library(AICcmodavg)
library(MuMIn)
library(ggplot2)


#### importar dados ####
dados <- read.csv(here("ocorrencia e riqueza por TI.csv"), sep=",")
head(dados)

#### Calculo do indice de expansão da soja ####
# Dados
area_ti <- c(1769.4, 30474.0, 7627.0, 3173.8, 50018.3, 12883.3, 859.1, 16354.1, 4303.8, 11182.4, 
             14210.7, 193493.6, 28631.8, 4063.7, 43336.7, 14213.0, 5555.9, 13573.0, 40095.0, 
             4607.6, 15597.1, 4276.8, 1747464.8, 654110.1, 29698.0, 11626.8, 5983.0, 13014.7, 
             3970898.0, 5652.8, 2585911.6, 405698.0, 9664975.5)

soja <- c(15.2, 9.3, 20.1, 84.1, 76.0, 1330.9, 131.2, 98.0, 186.1, 276.6, 
          2036.3, 2.6, 28.1, 71.3, 1204.3, 44.2, 28.1, 16.5, 0.0, 135.8, 
          28.0, 157.0, 1683.5, 2816.0, 0.6, 178.3, 1921.8, 12.4, 0.0, 
          540.0, 0.0, 0.0, 0.0)

# Área do buffer de 3km em torno da TI (em hectares)
buffer_3km_area <- 3 * 1000 * 3 * 1000 / 10000  # = 900 hectares (3000 m x 3000 m)


# Índice : Soja por proporção da TI no buffer de 3km
indice <- soja / (area_ti / buffer_3km_area)

#colocando no dataframe
dados$indice <- indice


#### Correlação das variáveis ####
# Selecionar apenas as variáveis preditoras contínuas
variaveis_preditoras <- dados %>%
  select(Dist_Tis, Dist_Uc, Area.ha., Soja.ha., indice)

# Verificar se há valores ausentes
any(is.na(variaveis_preditoras))

# Calcular a matriz de correlação
cor_matrix <- cor(variaveis_preditoras, method = "pearson")

# Visualizar a matriz
print(round(cor_matrix, 2))

# visualização gráfica
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", addCoef.col = "black")

#### Padronizar as variáveis contínuas ####
dados$n_Dist_Tis <- (dados$Dist_Tis-(min(dados$Dist_Tis)))/(max(dados$Dist_Tis)-min(dados$Dist_Tis))
dados$n_Dist_Uc  <- (dados$Dist_Uc-(min(dados$Dist_Uc)))/(max(dados$Dist_Uc)-min(dados$Dist_Uc))
dados$n_soja <- (dados$Soja.ha.-(min(dados$Soja.ha.)))/(max(dados$Soja.ha.)-min(dados$Soja.ha.))
dados$n_area <- (dados$Area.ha.-(min(dados$Area.ha.)))/(max(dados$Area.ha.)-min(dados$Area.ha.))
dados$n_indice <- (dados$indice-(min(dados$indice)))/(max(dados$indice)-min(dados$indice))

# Verificação rápida: se vai de 0 a 1
summary(dados[, c("n_Dist_Tis", "n_Dist_Uc", "n_soja", "n_area", "n_indice")])
apply(dados[, c("n_Dist_Tis", "n_Dist_Uc", "n_soja","n_area", "n_indice")], 2, sd)

#### Transformar Isolamento em fator ####
dados$Isolamento <- as.factor(dados$Isolamento)

#### Lista de grupos biológicos ####
grupos <- c("Anfibios", "Aves", "Mamiferos", "Repteis", "Arvores")

#transformar em numerico
dados[grupos] <- lapply(dados[grupos], function(x) as.numeric(as.character(x)))

# Verifica se há NA
sapply(dados[grupos], anyNA)

#### Lista de modelos ####
#tirei a parte aleatoria, devido as nossas fontes de dados serem precárias, podendo afetar esse estilo de analise
seleciona_modelos <- function(resp) {
  formulas <- list(
    nulo = as.formula(paste(resp, "~ 1")),
    ilhada = as.formula(paste(resp, "~ Isolamento")),
    completo1 = as.formula(paste(resp, "~ Isolamento + n_Dist_Uc + n_Dist_Tis + n_indice")),
    completo2 = as.formula(paste(resp, "~ Isolamento + n_Dist_Uc + n_area + n_indice")),
    dist_tis = as.formula(paste(resp, "~ Isolamento + n_Dist_Tis")),
    dist_uc  = as.formula(paste(resp, "~ Isolamento + n_Dist_Uc ")),
    area     = as.formula(paste(resp, "~ Isolamento + n_area")),
    soja     = as.formula(paste(resp, "~ Isolamento + n_indice")),
    soja_dist_ti = as.formula(paste(resp, "~ Isolamento + n_Dist_Tis + n_indice")),
    soja_dist_uc = as.formula(paste(resp, "~ Isolamento + n_Dist_Uc + n_indice")),
    soja_area = as.formula(paste(resp, "~ Isolamento + n_area + n_indice")),
  # area_dist_tis = as.formula(paste(resp, "~ Isolamento + n_Dist_Tis + n_area")), correlacao
    area_dist_uc  = as.formula(paste(resp, "~ Isolamento + n_Dist_Uc + n_area"))
  )
  
  modelos <- lapply(formulas, function(f) {
    glm(f, data = dados, family = poisson)
  })
  
  # AIC
  aics <- AICcmodavg::aictab(modelos, base = TRUE, sort = TRUE)
  print(aics)
  
  return(modelos)
}

#### Selecao de modelos para cada grupo biologico ####
mod_anfibios <- seleciona_modelos("Anfibios")

mod_aves <- seleciona_modelos("Aves")

mod_mamiferos <- seleciona_modelos("Mamiferos")

mod_repteis <- seleciona_modelos("Repteis")

mod_arvores <- seleciona_modelos("Arvores")

#### Analise de residuos dos modelos plausíveis para cada grupo biologico ####
x11()
simulateResiduals(mod_anfibios$soja_dist_ti) |> plot()
summary(mod_anfibios$soja_dist_ti)

simulateResiduals(mod_anfibios$soja_area) |> plot()
summary(mod_anfibios$soja_area)

simulateResiduals(mod_aves$completo1) |> plot() #problema em todos quartis, mudar familia
summary(mod_aves$completo1)

simulateResiduals(mod_mamiferos$dist_uc) |> plot()
summary(mod_mamiferos$dist_uc)

simulateResiduals(mod_mamiferos$soja_dist_uc) |> plot()
summary(mod_mamiferos$soja_dist_uc)

simulateResiduals(mod_mamiferos$completo1) |> plot()
summary(mod_mamiferos$completo1)

simulateResiduals(mod_mamiferos$area_dist_uc) |> plot()
summary(mod_mamiferos$area_dist_uc)

simulateResiduals(mod_repteis$dist_tis) |> plot() #problema no primeiro quartil, mudar familia
summary(mod_repteis$dist_tis)

simulateResiduals(mod_repteis$area) |> plot() #problema no primeiro quartil, mudar familia
summary(mod_repteis$area)

simulateResiduals(mod_repteis$soja_dist_ti) |> plot() #problema nos primeiros quartis, mudar familia
summary(mod_repteis$soja_dist_ti)

simulateResiduals(mod_arvores$completo1) |> plot() 
summary(mod_arvores$completo1)

simulateResiduals(mod_arvores$completo2) |> plot() 
summary(mod_arvores$completo2)


# Performande para os modelos que já estavam bons segundo o Dharma
performance::check_model(mod_anfibios$soja_dist_ti, check = c("vif", "qq", "homogeneity", "linearity")) #ok, plausível
performance::check_model(mod_anfibios$soja_area, check = c("vif", "qq", "homogeneity", "linearity")) #ok, plausível

performance::check_model(mod_mamiferos$dist_uc, check = c("vif", "qq", "homogeneity", "linearity")) #ok, plausível
performance::check_model(mod_mamiferos$soja_dist_uc, check = c("vif", "qq", "homogeneity", "linearity")) #ok, plausível
performance::check_model(mod_mamiferos$completo1, check = c("vif", "qq", "homogeneity", "linearity")) #ok, plausível
performance::check_model(mod_mamiferos$area_dist_uc, check = c("vif", "qq", "homogeneity", "linearity")) #ok, plausível

performance::check_model(mod_arvores$completo1, check = c("vif", "qq", "homogeneity", "linearity")) #MUITO RUIM
performance::check_model(mod_arvores$completo2, check = c("vif", "qq", "homogeneity", "linearity")) #MUITO RUIM

# vif
check_collinearity(mod_arvores$completo1)
check_collinearity(mod_arvores$completo2)

# Refitando com distribuição negativa binomial
mod_aves_completo1_nb <- glm.nb(Aves ~ Isolamento + n_Dist_Uc + n_Dist_Tis + n_indice, data = dados) #nao roda
mod_aves_completo1_qp <- glm(Aves ~ Isolamento + n_Dist_Uc + n_Dist_Tis + n_indice, data = dados, family = quasipoisson)


mod_repteis_dist_tis_nb <- glm.nb(Repteis ~ Isolamento + n_Dist_Tis, data = dados)
mod_repteis_area_nb <- glm.nb(Repteis ~ Isolamento + n_area, data = dados)
mod_repteis_soja_dist_ti_nb <- glm.nb(Repteis ~ Isolamento + n_Dist_Tis + n_indice, data = dados)

mod_arvores_completo1_nb <- glm.nb(Arvores ~ Isolamento + n_Dist_Uc + n_Dist_Tis + n_indice, data = dados)
mod_arvores_completo2_nb <- glm.nb(Arvores ~ Isolamento + n_Dist_Uc + n_area + n_indice, data = dados)

# Para os modelos refit com binomial negativa
x11()
performance::check_model(mod_aves_completo1_qp, check = c("vif", "qq", "homogeneity", "linearity")) #ok, plausível

performance::check_model(mod_repteis_dist_tis_nb, check = c("vif", "qq", "homogeneity", "linearity")) #ok, plausível
performance::check_model(mod_repteis_area_nb, check = c("vif", "qq", "homogeneity", "linearity")) #ok, plausível
performance::check_model(mod_repteis_soja_dist_ti_nb, check = c("vif", "qq", "homogeneity", "linearity")) #ok, plausível

performance::check_model(mod_arvores_completo2_nb, check = c("vif", "qq", "homogeneity", "linearity")) #Ainda horroroso, nem vale a pena plotar
performance::check_model(mod_arvores_completo1_nb, check = c("vif", "qq", "homogeneity", "linearity")) #Ainda horroroso, nem vale a pena plotar


#graficos
# ANFÍBIOS - modelo Poisson
mod_anfibios_soja_dist_ti <- glm(Anfibios ~ Isolamento + n_indice + Dist_Tis, data = dados, family = poisson)
summary(mod_anfibios_soja_dist_ti)
visreg(mod_anfibios_soja_dist_ti, xvar = "n_indice", by = "Isolamento",
       xlab = "Índice de expansão da soja",
       ylab = "Riqueza de anfíbios",
       #line = list(col = "#666666"),
       top = "points",
       gg = TRUE,
       band = TRUE,
       overlay = TRUE) + 
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))

visreg(mod_anfibios_soja_dist_ti, xvar = "Dist_Tis", by = "Isolamento",
       xlab = "Distância em metros da TI mais próxima",
       ylab = "Riqueza de anfíbios",
       line = list(col = "#666666"),
       gg = TRUE,
       band = TRUE) + 
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))

# AVES - modelo refit com binomial negativa
mod_aves_completo1_qp <- glm(Aves ~ Isolamento + n_indice + Dist_Tis + Dist_Uc, data = dados, family = quasipoisson)
summary(mod_aves_completo1_nb)

visreg(mod_aves_completo1_nb, xvar = "n_indice", by = "Isolamento",
       xlab = "Índice de expansão da soja",
       ylab = "Riqueza de aves",
       # line = list(col = "#666666"),
       top = "points",
       gg = TRUE,
       band = TRUE,
       overlay = TRUE) + 
  annotate("text", 
           x = Inf, y = Inf, 
           label = "Aves ~ Isolamento + Dist_Tis + Dist_Uc + Indice soja", 
           hjust = 1.1, vjust = 1.5, 
           size = 4) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))

visreg(mod_aves_completo1_qp, xvar = "Dist_Uc", by = "Isolamento",
       xlab = "Distância em metros da UC mais próxima",
       ylab = "Riqueza de aves",
       # line = list(col = "#666666"),
       top = "points",
       gg = TRUE,
       band = TRUE,
       overlay = TRUE) + 
  annotate("text", 
           x = Inf, y = Inf, 
           label = "Aves ~ Isolamento + Dist_Tis + Dist_Uc + Indice soja", 
           hjust = 1.1, vjust = 1.5, 
           size = 4) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))

visreg(mod_aves_completo1_qp, xvar = "Dist_Tis", by = "Isolamento",
       xlab = "Distância em metros da TI mais próxima",
       ylab = "Riqueza de aves",
       # line = list(col = "#666666"),
       top = "points",
       gg = TRUE,
       band = TRUE,
       overlay = TRUE) + 
  annotate("text", 
           x = Inf, y = Inf, 
           label = "Aves ~ Isolamento + Dist_Tis + Dist_Uc + Indice soja", 
           hjust = 1.1, vjust = 1.5, 
           size = 4) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))


# MAMÍFEROS - modelo Poisson 
mod_mamiferos_completo1_nb <- glm.nb(Mamiferos ~ Isolamento + indice2 + Dist_Uc + Dist_Tis , data = dados)
summary(mod_mamiferos_completo1_nb)
visreg(mod_mamiferos_completo1_nb, xvar = "indice2", by = "Isolamento",
       xlab = "Expansão da soja",
       ylab = "Riqueza de mamíferos",
       line = list(col = "#666666"),
       gg = TRUE,
       band = TRUE) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))

visreg(mod_mamiferos_completo2_nb, xvar = "Dist_Uc", by = "Isolamento",
       xlab = "Distância em metros da UC mais próxima",
       ylab = "Riqueza de mamíferos",
       line = list(col = "#666666"),
       gg = TRUE,
       band = TRUE) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))

visreg(mod_mamiferos_completo2_nb, xvar = "Area.ha.", by = "Isolamento",
       xlab = "Área da TI (ha)",
       ylab = "Riqueza de mamíferos",
       line = list(col = "#666666"),
       gg = TRUE,
       band = TRUE) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))


# RÉPTEIS - modelo refit com binomial negativa
mod_repteis_soja <- glm(Repteis ~ Soja.ha. + Isolamento, data = dados)
visreg(mod_repteis_soja, xvar = "Soja.ha.", by = "Isolamento",
       xlab = "Área de soja (ha)",
       ylab = "Riqueza de répteis",
       line = list(col = "#666666"),
       gg = TRUE,
       band = TRUE) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))



mod_repteis_soja_uc_nb <- glm.nb(Repteis ~ Soja.ha. + Dist_Uc + Isolamento, data = dados)

visreg(mod_repteis_soja_uc_nb, xvar = "Soja.ha.", by = "Isolamento",
       xlab = "Área de soja (ha)",
       ylab = "Riqueza de répteis",
       line = list(col = "#666666"),
       gg = TRUE,
       band = TRUE) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))

visreg(mod_repteis_soja_uc_nb, xvar = "Dist_Uc", by = "Isolamento",
       xlab = "Distância da UC mais próxima (padronizada)",
       ylab = "Riqueza de répteis",
       line = list(col = "#666666"),
       gg = TRUE,
       band = TRUE) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))


mod_repteis_completo2_nb <- glm.nb(Repteis ~ Isolamento + Dist_Uc + Area.ha. + Soja.ha., data = dados)

visreg(mod_repteis_completo2_nb, xvar = "Dist_Uc", by = "Isolamento",
       xlab = "Distância em metros da UC mais próxima",
       ylab = "Riqueza de répteis",
       line = list(col = "#666666"),
       gg = TRUE,
       band = TRUE) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))


visreg(mod_repteis_completo2_nb, xvar = "Area.ha.", by = "Isolamento",
       xlab = "Área da TI (ha)",
       ylab = "Riqueza de répteis",
       line = list(col = "#666666"),
       gg = TRUE,
       band = TRUE) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))

visreg(mod_repteis_completo2_nb, xvar = "Soja.ha.", by = "Isolamento",
       xlab = "Área de soja (ha)",
       ylab = "Riqueza de répteis",
       line = list(col = "#666666"),
       gg = TRUE,
       band = TRUE) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))




#### model average grupos biologicos com mais de um modelo plausivel####

library(MuMIn)
# Anfibios
M0a <- mod_anfibios$soja_area
M1a <- mod_anfibios$soja_dist_ti

# Seleção de modelos
models_anfibios <- model.sel(M1a, M0a, rank = "AIC")
print(models_anfibios)

# Somatório dos pesos dos modelos com delta < 2 = importancia relativa
sw(subset(models_anfibios, delta < 2, recalc.weights = TRUE))

# Média dos modelos (todos)
modavg_anfibios <- model.avg(models_anfibios, subset = NA, rank = "AIC")

summary(modavg_anfibios) #pegar os valores do conditional average - todos significativos

# Média dos modelos com peso cumulativo ≤ 0.95
summary(model.avg(models_anfibios, subset = cumsum(models_anfibios$weight) <= 0.95))

# Extrair sumário com p-valores
modavg_sum <- summary(modavg_anfibios)$coefmat.subset

# Identificar intervalos de confiança q não cruzam o 0
sig <- modavg_sum[, "Pr(>|z|)"] < 0.05

#grafico
plot(modavg_anfibios, full = FALSE, intercept = FALSE,
     dotcex = 1.2,         # tamanho do ponto
     pch = ifelse(sig[-1], 19, 1),   # 19: ponto sólido (significativo), 1: círculo (não)
     lwd = 1.2,            # espessura da linha de confiança
     lend = 1, width = 0,  # terminações retas, sem barras horizontais
     horizontal = TRUE,    # orienta horizontalmente
     main = "Riqueza de anfíbios",  # título
     xlab = "Estimativa média dos coeficientes",
     xaxt = "s", yaxt = "n",        # remove eixo y padrão
     bty = "l")

# Substituir nomes no eixo Y
axis(2, at = 1:4, 
     labels = rev(c("Ilhada", "Distância de TIs", "Índice soja", "Área TI")),
     las = 1, tick = FALSE, cex.axis = 0.8)





# Mamíferos
M0m = mod_mamiferos$dist_uc
M1m = mod_mamiferos$soja_dist_uc
M2m = mod_mamiferos$completo1
M3m = mod_mamiferos$area_dist_uc

# Seleção de modelos
models_mamiferos <- model.sel(M3m, M2m,M1m, M0m,rank = "AIC")
print(models_mamiferos)

# Somatório dos pesos dos modelos com delta < 2
sw(subset(models_mamiferos, delta < 2, recalc.weights = TRUE))

# Média dos modelos (todos)
modavg_mamiferos <- model.avg(models_mamiferos, subset = NA, rank = "AIC")
summary(modavg_mamiferos) 

# Média dos modelos com peso cumulativo ≤ 0.95
summary(model.avg(models_mamiferos, subset = cumsum(models_mamiferos$weight) <= 0.95))

# Extrair sumário com p-valores
modavg_sum <- summary(modavg_mamiferos)$coefmat.subset

# Identificar intervalos de confiança q não cruzam o 0
sig <- modavg_sum[, "Pr(>|z|)"] < 0.05

#grafico
plot(modavg_mamiferos, full = FALSE, intercept = FALSE,
     dotcex = 1.2,         # tamanho do ponto
     pch = ifelse(sig[-1], 19, 1),   # 19: ponto sólido (significativo), 1: círculo (não) [-1] no sig pq o primeiro valor é do intercepto
     lwd = 1.2,            # espessura da linha de confiança
     lend = 1, width = 0,  # terminações retas, sem barras horizontais
     horizontal = TRUE,    # orienta horizontalmente
     main = "Riqueza de Mamíferos",  # título
     xlab = "Estimativa média dos coeficientes",
     xaxt = "s", yaxt = "n",        # remove eixo y padrão
     bty = "l")

# Substituir nomes no eixo Y
axis(2, at = 1:5, 
     labels = rev(c("Ilhada", "Distância de UCs", "Distância de TIs", "Índice soja", "Área TI")),
     las = 1, tick = FALSE, cex.axis = 0.8)



# Répteis (com binomial negativa)
M0r = mod_repteis_dist_tis_nb
M1r = mod_repteis_area_nb
M2r = mod_repteis_soja_dist_ti_nb

# Seleção de modelos
models_repteis <- model.sel(M2r ,M1r, M0r,rank = "AIC")
print(models_repteis)

# Somatório dos pesos dos modelos com delta < 2
sw(subset(models_repteis, delta < 2, recalc.weights = TRUE))

# Média dos modelos (todos)
modavg_repteis <- model.avg(models_repteis, subset = NA, rank = "AIC")
summary(modavg_repteis) 

# Média dos modelos com peso cumulativo ≤ 0.95 - 95% de explicação do sistema
summary(model.avg(models_repteis, subset = cumsum(models_repteis$weight) <= 0.95)) # pegar os valores do conditionall average pq “Qual o efeito da variável, quando ela está presente nos melhores modelos?”

# Extrair sumário com p-valores
modavg_sum <- summary(modavg_repteis)$coefmat.subset

# Identificar intervalos de confiança q não cruzam o 0
sig <- modavg_sum[, "Pr(>|z|)"] < 0.05

#grafico
plot(modavg_repteis, full = FALSE, intercept = FALSE,
     dotcex = 1.2,         # tamanho do ponto
     pch = ifelse(sig[-1], 19, 1),   # 19: ponto sólido (significativo), 1: círculo (não)
     lwd = 1.2,            # espessura da linha de confiança
     lend = 1, width = 0,  # terminações retas, sem barras horizontais
     horizontal = TRUE,    # orienta horizontalmente
     main = "Riqueza de Répteis",  # título
     xlab = "Estimativa média dos coeficientes",
     xaxt = "s", yaxt = "n",        # remove eixo y padrão
     bty = "l")

# Substituir nomes no eixo Y
axis(2, at = 1:4, 
     labels = rev(c("Ilhada","Área TI", "Distância de TIs", "Índice soja" )),
     las = 1, tick = FALSE, cex.axis = 0.8)




#### Riqueza total ####
# Somar a riqueza total para cada TI
dados$riqueza_total <- rowSums(dados[, c("Anfibios", "Aves", "Mamiferos", "Repteis", "Arvores")])
head(dados)

#listagem de modelos, mas colocando binomial negativa pq tem muito 0
seleciona_modelos <- function(resp) {
  formulas <- list(
    nulo = as.formula(paste(resp, "~ 1")),
    ilhada = as.formula(paste(resp, "~ Isolamento")),
    completo1 = as.formula(paste(resp, "~ Isolamento + n_Dist_Uc + n_Dist_Tis + n_indice")),
    completo2 = as.formula(paste(resp, "~ Isolamento + n_Dist_Uc + n_area + n_indice")),
    dist_tis = as.formula(paste(resp, "~ Isolamento + n_Dist_Tis")),
    dist_uc  = as.formula(paste(resp, "~ Isolamento + n_Dist_Uc ")),
    area     = as.formula(paste(resp, "~ Isolamento + n_area")),
    soja     = as.formula(paste(resp, "~ Isolamento + n_indice")),
    soja_dist_ti = as.formula(paste(resp, "~ Isolamento + n_Dist_Tis + n_indice")),
    soja_dist_uc = as.formula(paste(resp, "~ Isolamento + n_Dist_Uc + n_indice")),
    soja_area = as.formula(paste(resp, "~ Isolamento + n_area + n_indice")),
    # area_dist_tis = as.formula(paste(resp, "~ Isolamento + n_Dist_Tis + n_area")), correlacao
    area_dist_uc  = as.formula(paste(resp, "~ Isolamento + n_Dist_Uc + n_area"))
  )
  
  modelos <- lapply(formulas, function(f) {
    glm(f, data = dados, family = poisson)
  })
  
  # AIC
  aics <- AICcmodavg::aictab(modelos, base = TRUE, sort = TRUE)
  print(aics)
  
  return(modelos)
}


# Seleção de modelos para a riqueza total
modelos_riqueza <- seleciona_modelos("riqueza_total")

# Análise dos resíduos dos melhores modelos 
simulateResiduals(modelos_riqueza$completo2) |> plot() #horroroso
summary(modelos_riqueza$completo2)

simulateResiduals(modelos_riqueza$area_dist_uc) |> plot() #horroroso
summary(modelos_riqueza$area_dist_uc)

performance::check_model(modelos_riqueza$completo2, check = c("vif", "qq", "homogeneity", "linearity")) #nao uniforme

performance::check_model(modelos_riqueza$area_dist_uc, check = c("vif", "qq", "homogeneity", "linearity")) #nao uniforme


mod_sel1_riqueza <- glm(riqueza_total ~ Isolamento + n_Dist_Uc + n_area + n_indice, data = dados, family = quasipoisson)
performance::check_model(mod_sel1_riqueza, check = c("vif", "qq", "homogeneity", "linearity")) #mais uniforme


mod_sel2_riqueza <- glm(riqueza_total ~ Isolamento + n_Dist_Uc + n_area, data = dados, family = quasipoisson)
performance::check_model(mod_sel2_riqueza, check = c("vif", "qq", "homogeneity", "linearity")) #mais uniforme

#model average
M0 = modelos_riqueza$completo2
M1 = modelos_riqueza$area_dist_uc

# Seleção de modelos
models_riqueza <- model.sel(M1, M0,rank = "AIC")
print(models_riqueza)

# Somatório dos pesos dos modelos com delta < 2
sw(subset(models_riqueza, delta < 2, recalc.weights = TRUE))

# Média dos modelos (todos)
modavg_riqueza <- model.avg(models_riqueza, subset = NA, rank = "AIC")
summary(modavg_riqueza) 

# Média dos modelos com peso cumulativo ≤ 0.95 - 95% de explicação do sistema
summary(model.avg(models_riqueza, subset = cumsum(models_riqueza$weight) <= 0.95)) # pegar os valores do conditionall average pq “Qual o efeito da variável, quando ela está presente nos melhores modelos?”

# Extrair sumário com p-valores
modavg_sum <- summary(modavg_riqueza)$coefmat.subset

# Identificar intervalos de confiança q não cruzam o 0
sig <- modavg_sum[, "Pr(>|z|)"] < 0.05

#grafico
plot(modavg_riqueza, full = FALSE, intercept = FALSE,
     dotcex = 1.2,         # tamanho do ponto
     pch = ifelse(sig[-1], 19, 1),   # 19: ponto sólido (significativo), 1: círculo (não)
     lwd = 1.2,            # espessura da linha de confiança
     lend = 1, width = 0,  # terminações retas, sem barras horizontais
     horizontal = TRUE,    # orienta horizontalmente
     main = "Riqueza total",  # título
     xlab = "Estimativa média dos coeficientes",
     xaxt = "s", yaxt = "n",        # remove eixo y padrão
     bty = "l")

# Substituir nomes no eixo Y
axis(2, at = 1:4, 
     labels = rev(c("Ilhada", "Distância de UCs", "Área TI", "Índice soja" )),
     las = 1, tick = FALSE, cex.axis = 0.8)
