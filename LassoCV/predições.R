# BANCO DE DADOS 
# SETORES CENSITÁRIOS

library(readxl)
library(dplyr)

dadossc1 <- read_excel("C:/Users/DLO/Downloads/BANCO_SP_EXCETOCAPITAL_setorcensitario.xlsx")
dadossc <- dadossc1[,-c(1:15)]
#dadosap <- read_excel("C:/Users/DLO/Downloads/BANCO_SPCAPITAL.xlsx")

AP <- dadossc1$`Área de Ponderação`
SC <- dadossc1$Cod_setor



# DIVISÃO DOMICÍLIOS
DPP_sc <- c(dadossc$`Domicílios particulares permanentes-DPP`)
DPP_divisor_sc <- list(`DPP SEM banheiro de uso exclusivo dos moradores e NEM sanitário` = dadossc$`DPP SEM banheiro de uso exclusivo dos moradores e NEM sanitário`,
                    `DPP com lixo coletado em caçamba de serviço de limpeza` = dadossc$`DPP com lixo coletado em caçamba de serviço de limpeza`,
                    `DPP com lixo queimado na propriedade` = dadossc$`DPP com lixo queimado na propriedade`,
                    `DPP com lixo enterrado na propriedade` = dadossc$`DPP com lixo enterrado na propriedade`,
                    `DPP com lixo jogado em terreno baldio ou logradouro` = dadossc$`DPP com lixo jogado em terreno baldio ou logradouro`,
                    `DPP com lixo jogado em rio, lago ou mar` = dadossc$`DPP com lixo jogado em rio, lago ou mar`,
                    `DPP SEM energia elétrica` = dadossc$`DPP SEM energia elétrica`,
                    `DPP do tipo casa próprios e em aquisição` = dadossc$`DPP do tipo casa próprios e em aquisição`,
                    `DPP do tipo casa alugados` = dadossc$`DPP do tipo casa alugados`,
                    `DPP do tipo casa cedidos por empregador` = dadossc$`DPP do tipo casa cedidos por empregador`,
                    `DPP com outra forma de destino do lixo e outra forma de abastecimento de água` = dadossc$`DPP com outra forma de destino do lixo e outra forma de abastecimento de água`,
                    
                    `Total de domicílios particulares improvisados` = dadossc$`Total de domicílios particulares improvisados`,
                    `Domicílios particulares sem rendimento nominal mensal domiciliar per capita` = dadossc$`Domicílios particulares sem rendimento nominal mensal domiciliar per capita`,
                    `DPP alugados – Não existe iluminação pública` = dadossc$`DPP alugados – Não existe iluminação pública`,
                    `DPP alugados – Não existe pavimentação` = dadossc$`DPP alugados – Não existe pavimentação`,
                    `DPP próprios – Existe esgoto a céu aberto` = dadossc$`DPP próprios – Existe esgoto a céu aberto`,
                    `DPP alugados – Existe esgoto a céu aberto` = dadossc$`DPP alugados – Existe esgoto a céu aberto`,
                    `DPP que não tinham banheiro ou sanitário – Não existe iluminação pública` = dadossc$`DPP que não tinham banheiro ou sanitário – Não existe iluminação pública`,
                    `DPP que não tinham banheiro ou sanitário – Não existe pavimentação` = dadossc$`DPP que não tinham banheiro ou sanitário – Não existe pavimentação`,
                    `DPP que não tinham banheiro ou sanitário – Não existe calçada` = dadossc$`DPP que não tinham banheiro ou sanitário – Não existe calçada`,
                    `DPP que não tinham banheiro ou sanitário – Não existe meio-fio/guia` = dadossc$`DPP que não tinham banheiro ou sanitário – Não existe meio-fio/guia`,
                    `DPP que não tinham banheiro ou sanitário – Não existe bueiro/boca-de-lobo` = dadossc$`DPP que não tinham banheiro ou sanitário – Não existe bueiro/boca-de-lobo`,
                    `DPP que não tinham banheiro ou sanitário – Não existe rampa para cadeirante` = dadossc$`DPP que não tinham banheiro ou sanitário – Não existe rampa para cadeirante`,
                    `DPP que não tinham banheiro ou sanitário – Não existe arborização` = dadossc$`DPP que não tinham banheiro ou sanitário – Não existe arborização`,
                    `DPP que não tinham banheiro ou sanitário – Existe esgoto a céu aberto` = dadossc$`DPP que não tinham banheiro ou sanitário – Existe esgoto a céu aberto`,
                    `DPP que não tinham banheiro ou sanitário – Existe lixo acumulado nos logradouros` = dadossc$`DPP que não tinham banheiro ou sanitário – Existe lixo acumulado nos logradouros`,
                    `Moradia Adequada_V202eV203` = dadossc$`Moradia Adequada_V202eV203`,
                    `Moradia Semi-Adequada_V204eV205` = dadossc$`Moradia Semi-Adequada_V204eV205`,
                    `Moradia Inadequada_V206eV207` = dadossc$`Moradia Inadequada_V206eV207`)

dividido_DPP_sc <- as.data.frame(
  sapply(DPP_divisor_sc, function(col) {
    resultado <- ifelse(DPP_sc == 0, 0, col / DPP_sc)  # trata divisão por zero
    resultado[is.na(resultado)] <- 0 
    return(resultado)
  })
)


a_sc <- dadossc$`Total do rendimento nominal mensal dos domi. partic.`
a_sc <- a_sc/DPP_sc
mediana_a_sc <- median(a_sc, na.rm = TRUE)
iqr_a_sc <- IQR(a_sc, na.rm = TRUE)
Robust_a_sc <- (a_sc - mediana_a_sc) / iqr_a_sc
Robust_a_sc[is.na(Robust_a_sc)] <- 0
Robust_a_sc[is.infinite(Robust_a_sc)] <- 0
summary(Robust_a_sc)

b_sc <- dadossc$`Total do rendimento nominal mensal dos domi. partic. improvisados`
b_sc <- b_sc/DPP_sc
mediana <- median(b_sc, na.rm = TRUE)
iqr_val <- IQR(b_sc, na.rm = TRUE)
Robust_b_sc <- (b_sc - mediana) / iqr_val
Robust_b_sc[is.na(Robust_b_sc)] <- 0
Robust_b_sc[is.infinite(Robust_b_sc)] <- 0
summary(Robust_b_sc)


# PESis.infinite()# PESSOAS RESIDENTES
Pessoas_sc <- c(`Pessoas Residentes` = dadossc$`Pessoas Residentes`)
divisor_pessoas_sc <- list(`Moradores em DPP próprios e em aquisição` = dadossc$`Moradores em DPP próprios e em aquisição`,
                        `Moradores em DPP alugados` = dadossc$`Moradores em DPP alugados`,
                        `Moradores em DPP cedidos por empregador` = dadossc$`Moradores em DPP cedidos por empregador`,
                        `Moradores em DPP com outra condição de ocupação (não são próprios, alugados, nem cedidos)` = dadossc$`Moradores em DPP com outra condição de ocupação (não são próprios, alugados, nem cedidos)`,
                        `Moradores em DPP SEM banheiro de uso exclusivo dos moradores e NEM sanitário` = dadossc$`Moradores em DPP SEM banheiro de uso exclusivo dos moradores e NEM sanitário`,
                        `Moradores em DPP SEM energia elétrica` = dadossc$`Moradores em DPP SEM energia elétrica`,
                        `Pessoas responsáveis, do sexo feminino` = dadossc$`Pessoas responsáveis, do sexo feminino`,
                        `Pessoas responsáveis, do sexo masculino` = dadossc$`Pessoas responsáveis, do sexo masculino`,
                        `Pessoas Residentes e cor ou raça - branca` = dadossc$`Pessoas Residentes e cor ou raça - branca`,
                        `Pessoas Residentes e cor ou raça - preta` = dadossc$`Pessoas Residentes e cor ou raça - preta`,
                        `Pessoas Residentes e cor ou raça - amarela` = dadossc$`Pessoas Residentes e cor ou raça - amarela`,
                        `Pessoas Residentes e cor ou raça - parda` = dadossc$`Pessoas Residentes e cor ou raça - parda`,
                        `Pessoas Residentes e cor ou raça - indígena` = dadossc$`Pessoas Residentes e cor ou raça - indígena`,
                        `Pessoas residentes em  DPP` = dadossc$`Pessoas residentes em  DPP`,
                        `Responsáveis pelos domicílios particulares` = dadossc$`Responsáveis pelos domicílios particulares`,
                        `Cônjuges ou companheiros(as) em domi. partic.` = dadossc$`Cônjuges ou companheiros(as) em domi. partic.`,
                        `Filhos(as) do responsável e do cônjuge em domi. partic.` = dadossc$`Filhos(as) do responsável e do cônjuge em domi. partic.`,
                        `Filhos(as) somente do responsável em domi. partic.` = dadossc$`Filhos(as) somente do responsável em domi. partic.`,
                        `Genros ou noras em domi. partic.` = dadossc$`Genros ou noras em domi. partic.`,
                        `Enteados(as) em domi. partic.` = dadossc$`Enteados(as) em domi. partic.`,  
                        `Pais, mães, padrastos ou madrastas em domi. partic.` = dadossc$`Pais, mães, padrastos ou madrastas em domi. partic.`,
                        `Sogros (as) em domi. partic.` = dadossc$`Sogros (as) em domi. partic.`,
                        `Netos(as) em domi. partic.` = dadossc$`Netos(as) em domi. partic.`,
                        `Bisnetos(as) em domi. partic.` = dadossc$`Bisnetos(as) em domi. partic.`,
                        `Irmãos ou irmãs em domi. partic.` = dadossc$`Irmãos ou irmãs em domi. partic.`,
                        `Avôs ou avós em domi. partic.` = dadossc$`Avôs ou avós em domi. partic.`,
                        `Outros parentes em domi. partic.` = dadossc$`Outros parentes em domi. partic.`,
                        `Agregados(as) em  domi. partic.` = dadossc$`Agregados(as) em  domi. partic.`,
                        `Conviventes em domi. partic.` = dadossc$`Conviventes em domi. partic.`)
dividido_pessoas_sc <- as.data.frame(sapply(divisor_pessoas_sc, function(col) col / Pessoas_sc))

which(is.na(dividido_pessoas_sc))

Xsc <- cbind(dividido_DPP_sc,Robust_a_sc,Robust_b_sc,dividido_pessoas_sc)
Xsc <- as.matrix(Xsc)
########################################################################
### LASSO validação cruzada ########################


library(glmnet)

# DOMICÍLIOS PRECÁRIOS

load("lasso.dm.RData")  # Isso recria o objeto modelo_lasso

pred.dp_sc <- predict(lasso.dm, newx = Xsc, s = "lambda.min")
summary(pred.dp_sc)
hist(pred.dp_sc, breaks = 30, main = "Distribuição das predições")
boxplot(pred.dp_sc)


# COABITAÇÃO

load("lasso.cb.RData")  # Isso recria o objeto modelo_lasso

pred.cb_sc <- predict(lasso.cb, newx = Xsc, s = "lambda.min")

summary(pred.cb_sc)
hist(pred.cb_sc, breaks = 30, main = "Distribuição das predições")
boxplot(pred.cb_sc)

# ÔNUS EXCESSIVO

load("lasso.oe.RData")  # Isso recria o objeto modelo_lasso

pred.oe_sc <- predict(lasso.oe, newx = Xsc, s = "lambda.min")

summary(pred.oe_sc)
hist(pred.oe_sc, breaks = 30, main = "Distribuição das predições")
boxplot(pred.oe_sc)

# ADENSAMENTO

load("lasso.ad.RData")  # Isso recria o objeto modelo_lasso

pred.ad_sc <- predict(lasso.ad, newx = Xsc, s = "lambda.min")

summary(pred.ad_sc)
hist(pred.ad_sc, breaks = 30, main = "Distribuição das predições")
boxplot(pred.ad_sc)

## Matriz

areadepond <- as.numeric(AP)

data.dp <- as.matrix(AP,SC,pred.dp_sc)
soma_por_ap_dp <- rowsum(pred.dp_sc, group = areadepond)

data.cb <- as.matrix(AP,SC,pred.cb_sc)
soma_por_ap_cb <- rowsum(pred.cb_sc, group = areadepond)

data.oe <- as.matrix(AP,SC,pred.oe_sc)
soma_por_ap_oe <- rowsum(pred.oe_sc, group = areadepond)

data.ad <- as.matrix(AP,SC, pred.ad_sc)
soma_por_ap_ad <- rowsum(pred.ad_sc, group = areadepond)

##################################################################
###################### PCA #######################################

# normalização min-max e depois scale no pca

# Normalização Min-Max
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
Zsc <- as.data.frame(lapply(dadossc, normalize))

# Recarrega a PCA
pca <- readRDS("pca.rds")

# Agora para projetar novos dados:
Zsc_proj <- predict(pca, newdata = Zsc)

# Isso retorna uma lista com vários elementos
str(Zsc_proj)
pca_sc <- Zsc_proj$coord[, 1:5]  # As 5 primeiras componentes principais
pca_sc <- as.data.frame(pca_sc)

# DOMICILIOS PRECARIOS
regressao_dm <- readRDS("regressao_dm.rds")
pred.sc.dm <- predict(regressao_dm, newdata = pca_sc)
summary(pred.sc.dm)
hist(pred.sc.dm, breaks=30, main="Histograma das predições da Regressão Linear + PCA (Domicílios Precários)")
boxplot(pred.sc.dm)

# COABITAÇÃO
regressao_cb <- readRDS("regressao_cb.rds")
pred.sc.cb <- predict(regressao_cb, newdata = pca_sc)
summary(pred.sc.cb)
hist(pred.sc.cb, breaks=30, main="Histograma das predições da Regressão Linear + PCA (Coabitação)")
boxplot(pred.sc.cb)

# ONUS EXCESSIVO
regressao_oe <- readRDS("regressao_oe.rds")
pred.sc.oe <- predict(regressao_oe, newdata = pca_sc)
summary(pred.sc.oe)
hist(pred.sc.oe, breaks=30, main="Histograma das predições da Regressão Linear + PCA (Ônus Excessivo)")
boxplot(pred.sc.oe)

# ADENSAMENTO
regressao_ad <- readRDS("regressao_ad.rds")
pred.sc.ad <- predict(regressao_ad, newdata = pca_sc)
summary(pred.sc.ad)
hist(pred.sc.ad, breaks=30, main="Histograma das predições da Regressão Linear + PCA (Adensamento)")
boxplot(pred.sc.ad)

