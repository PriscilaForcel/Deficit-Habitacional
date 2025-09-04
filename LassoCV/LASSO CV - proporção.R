library(readxl)
library(glmnet)

## BANCO DE DADOS TODO
dados1 <- read_excel("C:/Users/DLO/Downloads/BD_SP_DEFICIT.xlsx")
dados <- dados1[,-c(1:11)]

## BANCO DE DADOS DA CAPITAL
dados1 <- read_excel("C:/Users/DLO/Downloads/BANCO_SPCAPITAL.xlsx")
dados <- dados1[,-c(1:11)]

## BANCO DE DADOS EXCETO CAPITAL
dados1 <- read_excel("C:/Users/DLO/Downloads/BANCO_SP_EXCETOCAPITAL.xlsx")
dados <- dados1[,-c(1:11)]

# DIVISÃO DOMICÍLIOS
DPP <- c(`Domicílios particulares permanentes-DPP` = dados$`Domicílios particulares permanentes-DPP`)
DPP_divisor <- list(`DPP SEM banheiro de uso exclusivo dos moradores e NEM sanitário` = dados$`DPP SEM banheiro de uso exclusivo dos moradores e NEM sanitário`,
                 `DPP com lixo coletado em caçamba de serviço de limpeza` = dados$`DPP com lixo coletado em caçamba de serviço de limpeza`,
                 `DPP com lixo queimado na propriedade` = dados$`DPP com lixo queimado na propriedade`,
                 `DPP com lixo enterrado na propriedade` = dados$`DPP com lixo enterrado na propriedade`,
                 `DPP com lixo jogado em terreno baldio ou logradouro` = dados$`DPP com lixo jogado em terreno baldio ou logradouro`,
                 `DPP com lixo jogado em rio, lago ou mar` = dados$`DPP com lixo jogado em rio, lago ou mar`,
                 `DPP SEM energia elétrica` = dados$`DPP SEM energia elétrica`,
                 `DPP do tipo casa próprios e em aquisição` = dados$`DPP do tipo casa próprios e em aquisição`,
                 `DPP do tipo casa alugados` = dados$`DPP do tipo casa alugados`,
                 `DPP do tipo casa cedidos por empregador` = dados$`DPP do tipo casa cedidos por empregador`,
                 `DPP com outra forma de destino do lixo e outra forma de abastecimento de água` = dados$`DPP com outra forma de destino do lixo e outra forma de abastecimento de água`,
                 
                 `Total de domicílios particulares improvisados` = dados$`Total de domicílios particulares improvisados`,
                 `Domicílios particulares sem rendimento nominal mensal domiciliar per capita` = dados$`Domicílios particulares sem rendimento nominal mensal domiciliar per capita`,
                 `DPP alugados – Não existe iluminação pública` = dados$`DPP alugados – Não existe iluminação pública`,
                 `DPP alugados – Não existe pavimentação` = dados$`DPP alugados – Não existe pavimentação`,
                 `DPP próprios – Existe esgoto a céu aberto` = dados$`DPP próprios – Existe esgoto a céu aberto`,
                 `DPP alugados – Existe esgoto a céu aberto` = dados$`DPP alugados – Existe esgoto a céu aberto`,
                 `DPP que não tinham banheiro ou sanitário – Não existe iluminação pública` = dados$`DPP que não tinham banheiro ou sanitário – Não existe iluminação pública`,
                 `DPP que não tinham banheiro ou sanitário – Não existe pavimentação` = dados$`DPP que não tinham banheiro ou sanitário – Não existe pavimentação`,
                 `DPP que não tinham banheiro ou sanitário – Não existe calçada` = dados$`DPP que não tinham banheiro ou sanitário – Não existe calçada`,
                 `DPP que não tinham banheiro ou sanitário – Não existe meio-fio/guia` = dados$`DPP que não tinham banheiro ou sanitário – Não existe meio-fio/guia`,
                 `DPP que não tinham banheiro ou sanitário – Não existe bueiro/boca-de-lobo` = dados$`DPP que não tinham banheiro ou sanitário – Não existe bueiro/boca-de-lobo`,
                 `DPP que não tinham banheiro ou sanitário – Não existe rampa para cadeirante` = dados$`DPP que não tinham banheiro ou sanitário – Não existe rampa para cadeirante`,
                 `DPP que não tinham banheiro ou sanitário – Não existe arborização` = dados$`DPP que não tinham banheiro ou sanitário – Não existe arborização`,
                 `DPP que não tinham banheiro ou sanitário – Existe esgoto a céu aberto` = dados$`DPP que não tinham banheiro ou sanitário – Existe esgoto a céu aberto`,
                 `DPP que não tinham banheiro ou sanitário – Existe lixo acumulado nos logradouros` = dados$`DPP que não tinham banheiro ou sanitário – Existe lixo acumulado nos logradouros`,
                 `Moradia Adequada_V202eV203` = dados$`Moradia Adequada_V202eV203`,
                 `Moradia Semi-Adequada_V204eV205` = dados$`Moradia Semi-Adequada_V204eV205`,
                 `Moradia Inadequada_V206eV207` = dados$`Moradia Inadequada_V206eV207`)

a <- dados$`Total do rendimento nominal mensal dos domi. partic.`
a <- a/DPP
iqr_a <- quantile(a,prob=0.75) - quantile(a,prob=0.25)
Robust_a <- as.data.frame((a-median(a))/(iqr_a))
summary(Robust_a)

b <- dados$`Total do rendimento nominal mensal dos domi. partic. improvisados`
b <- b/DPP
iqr_b <- quantile(b,prob=0.75) - quantile(b,prob=0.25)
Robust_b <- as.data.frame((b-median(b))/(iqr_b))
summary(Robust_b)

dividido_DPP <- as.data.frame(sapply(DPP_divisor, function(col) col / DPP))

# PESSOAS RESIDENTES
Pessoas <- c(`Pessoas Residentes` = dados$`Pessoas Residentes`)
divisor_pessoas <- list(`Moradores em DPP próprios e em aquisição` = dados$`Moradores em DPP próprios e em aquisição`,
                     `Moradores em DPP alugados` = dados$`Moradores em DPP alugados`,
                     `Moradores em DPP cedidos por empregador` = dados$`Moradores em DPP cedidos por empregador`,
                     `Moradores em DPP com outra condição de ocupação (não são próprios, alugados, nem cedidos)` = dados$`Moradores em DPP com outra condição de ocupação (não são próprios, alugados, nem cedidos)`,
                     `Moradores em DPP SEM banheiro de uso exclusivo dos moradores e NEM sanitário` = dados$`Moradores em DPP SEM banheiro de uso exclusivo dos moradores e NEM sanitário`,
                     `Moradores em DPP SEM energia elétrica` = dados$`Moradores em DPP SEM energia elétrica`,
                     `Pessoas responsáveis, do sexo feminino` = dados$`Pessoas responsáveis, do sexo feminino`,
                     `Pessoas responsáveis, do sexo masculino` = dados$`Pessoas responsáveis, do sexo masculino`,
                     `Pessoas Residentes e cor ou raça - branca` = dados$`Pessoas Residentes e cor ou raça - branca`,
                     `Pessoas Residentes e cor ou raça - preta` = dados$`Pessoas Residentes e cor ou raça - preta`,
                     `Pessoas Residentes e cor ou raça - amarela` = dados$`Pessoas Residentes e cor ou raça - amarela`,
                     `Pessoas Residentes e cor ou raça - parda` = dados$`Pessoas Residentes e cor ou raça - parda`,
                     `Pessoas Residentes e cor ou raça - indígena` = dados$`Pessoas Residentes e cor ou raça - indígena`,
                     `Pessoas residentes em  DPP` = dados$`Pessoas residentes em  DPP`,
                     `Responsáveis pelos domicílios particulares` = dados$`Responsáveis pelos domicílios particulares`,
                     `Cônjuges ou companheiros(as) em domi. partic.` = dados$`Cônjuges ou companheiros(as) em domi. partic.`,
                     `Filhos(as) do responsável e do cônjuge em domi. partic.` = dados$`Filhos(as) do responsável e do cônjuge em domi. partic.`,
                     `Filhos(as) somente do responsável em domi. partic.` = dados$`Filhos(as) somente do responsável em domi. partic.`,
                     `Enteados(as) em domi. partic.` = dados$`Enteados(as) em domi. partic.`,
                     `Genros ou noras em domi. partic.` = dados$`Genros ou noras em domi. partic.`,
                     `Pais, mães, padrastos ou madrastas em domi. partic.` = dados$`Pais, mães, padrastos ou madrastas em domi. partic.`,
                     `Sogros (as) em domi. partic.` = dados$`Sogros (as) em domi. partic.`,
                     `Netos(as) em domi. partic.` = dados$`Netos(as) em domi. partic.`,
                     `Bisnetos(as) em domi. partic.` = dados$`Bisnetos(as) em domi. partic.`,
                     `Irmãos ou irmãs em domi. partic.` = dados$`Irmãos ou irmãs em domi. partic.`,
                     `Avôs ou avós em domi. partic.` = dados$`Avôs ou avós em domi. partic.`,
                     `Outros parentes em domi. partic.` = dados$`Outros parentes em domi. partic.`,
                     `Agregados(as) em  domi. partic.` = dados$`Agregados(as) em  domi. partic.`,
                     `Conviventes em domi. partic.` = dados$`Conviventes em domi. partic.`)
dividido_pessoas <- as.data.frame(sapply(divisor_pessoas, function(col) col / Pessoas))

which(is.na(dividido_pessoas))

X <- cbind(dividido_DPP,Robust_a, Robust_b,dividido_pessoas)
dim(X)

##X <- dados[, -((ncol(dados)-4):(ncol(dados)))]
# normalização
##X <- scale(X)

divisao_dados <- function(dados, porc=0.8) {
  set.seed(5)
  n <- nrow(dados)
  indices_treino <- sample(1:n, size = floor(porc * n), replace = FALSE)
  treino <- dados[indices_treino, ]
  teste <- dados[-indices_treino, ]
  return(list(treino = treino, teste = teste))
}

matriz_X <- divisao_dados(X,0.8)
treino <- matriz_X$treino
teste <- matriz_X$teste

modelo <- function(x,y){
  x <- as.matrix(x)
  set.seed(5)
  modelo_lasso_cv <- cv.glmnet(x,y,alpha=1,nfolds=5)
  cat("lambda.min = ",modelo_lasso_cv$lambda.min)
  cof <- coef(modelo_lasso_cv, s = "lambda.min")
  var <- rownames(cof)[cof[, 1] != 0]
  coef_values <- as.numeric(cof[cof[, 1] != 0])
  print(var)
  par(mfrow=c(1,1))
  par(mar = c(10, 4, 4, 2))  
  barplot(coef_values, names.arg = var, las = 2,
          cex.names = 0.6,
          main = "Contribuição das Variáveis Selecionadas pelo LASSO.CV",
          ylab = "Coeficientes", col = "steelblue")
  return(list(modelo = modelo_lasso_cv,
              variaveis = var, cof = cof,
              coef_filtrados = coef_values))
}

metricas <- function(lasso,dados,y){
  dados <- as.matrix(dados)
  predicao <- predict(lasso, newx = dados, s = "lambda.min")
  SSE <- sum((y - predicao)^2)
  SST <- sum((y - mean(y))^2)
  R2 <- 1 - SSE/SST
  RMSE <- sqrt(mean((y - predicao)^2))
  MAE <- mean(abs(y - predicao))
  cat("R2 =", R2, "| RMSE =", RMSE, "| MAE =", MAE, "\n")
  plot(y, predicao,
       xlab = "Valores observados",
       ylab = "Valores preditos",
       main = "Valores observados vs preditos", col = "darkblue")
  abline(0, 1, col = "red", lwd = 2)  # linha ideal (y = ŷ)
  par(mfrow = c(1, 2))
  residuos <- y - predicao
  plot(predicao, residuos,
       xlab = "Valores preditos",
       ylab = "Resíduos",
       main = "Resíduos vs preditos")
  abline(h = 0, col = "red", lwd = 2)
  qqnorm(residuos)
  qqline(residuos, col = "red", lwd = 2)
  print(shapiro.test(residuos))
}

# DM

dm <- as.matrix(dados1$`DOMICILIOS PRECARIOS`)
matriz_dm <- divisao_dados(dm,0.8)
treino_dm <- matriz_dm$treino
teste_dm <- matriz_dm$teste

lassodm <- modelo(treino,treino_dm)
lasso.dm <- lassodm$modelo
save(lasso.dm, file = "lasso.dm.RData")

coefs <-coef(lasso.dm, s = "lambda.min")
coefs_df <- as.data.frame(as.matrix(coefs))
coefs_df$variavel <- rownames(coefs_df)
colnames(coefs_df)[1] <- "coeficiente"
coefs_df_filtrado <- subset(coefs_df, coeficiente != 0)
coefs_ordenados <- coefs_df_filtrado[order(-abs(coefs_df_filtrado$coeficiente)), ]
dim(coefs_ordenados)
coefs_ordenados

pred.dm <- metricas(lasso.dm, X,dm)

# CB

cb <- as.matrix(dados1$COABITACAO)
matriz_cb <- divisao_dados(cb,0.8)
treino_cb <- matriz_cb$treino
teste_cb <- matriz_cb$teste

lassocb <- modelo(treino,treino_cb)
lasso.cb <- lassocb$modelo
save(lasso.cb, file = "lasso.cb.RData")

coefs <-coef(lasso.cb, s = "lambda.min")
coefs_df <- as.data.frame(as.matrix(coefs))
coefs_df$variavel <- rownames(coefs_df)
colnames(coefs_df)[1] <- "coeficiente"
coefs_df_filtrado <- subset(coefs_df, coeficiente != 0)
coefs_ordenados <- coefs_df_filtrado[order(-abs(coefs_df_filtrado$coeficiente)), ]
dim(coefs_ordenados)
coefs_ordenados

pred.cb <- metricas(lasso.cb, X,cb)

# OE

oe <- as.matrix(dados1$`ONUS EXCESSIVO`)
matriz_oe <- divisao_dados(oe,0.8)
treino_oe <- matriz_oe$treino
teste_oe <- matriz_oe$teste

lassooe <- modelo(treino,treino_oe)
lasso.oe <- lassooe$modelo
save(lasso.oe, file = "lasso.oe.RData")

coefs <-coef(lasso.oe, s = "lambda.min")
coefs_df <- as.data.frame(as.matrix(coefs))
coefs_df$variavel <- rownames(coefs_df)
colnames(coefs_df)[1] <- "coeficiente"
coefs_df_filtrado <- subset(coefs_df, coeficiente != 0)
coefs_ordenados <- coefs_df_filtrado[order(-abs(coefs_df_filtrado$coeficiente)), ]
dim(coefs_ordenados)
coefs_ordenados

pred.oe <- metricas(lasso.oe, X,oe)

# AD

ad <- as.matrix(dados1$ADENSAMENTO)
matriz_ad <- divisao_dados(ad,0.8)
treino_ad <- matriz_ad$treino
teste_ad <- matriz_ad$teste

lassoad <- modelo(treino,treino_ad)
lasso.ad <- lassoad$modelo
save(lasso.ad, file = "lasso.ad.RData")

coefs <-coef(lasso.ad, s = "lambda.min")
coefs_df <- as.data.frame(as.matrix(coefs))
coefs_df$variavel <- rownames(coefs_df)
colnames(coefs_df)[1] <- "coeficiente"
coefs_df_filtrado <- subset(coefs_df, coeficiente != 0)
coefs_ordenados <- coefs_df_filtrado[order(-abs(coefs_df_filtrado$coeficiente)), ]
dim(coefs_ordenados)
coefs_ordenados

pred.ad <- metricas(lasso.ad, X,ad)

