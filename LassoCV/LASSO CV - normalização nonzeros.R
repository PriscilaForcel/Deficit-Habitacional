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

X <- dados[, -((ncol(dados)-4):(ncol(dados)))]

normalize_nonzero <- function(col) {
  nonzero <- col[col != 0]
  if (length(nonzero) > 1) {
    m <- mean(nonzero)
    s <- sd(nonzero)
    col[col != 0] <- (col[col != 0] - m) / s
  }
  return(col)
}

X <- as.data.frame(lapply(X, normalize_nonzero))

zero_cols <- names(X)[apply(X, 2, function(col) all(col == 0))]
print(zero_cols)

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

