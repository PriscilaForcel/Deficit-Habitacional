{library(data.table)
library(gdata)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(writexl)
library(ggplot2)
library(reshape2)
library(readxl)
library(FactoMineR)
library(factoextra)
library(grDevices)
library(haven)
library(readxl)
  

options(scipen = 999)
options(datatable.integer64="numeric")}

#------------------------------------------------------------------------------#
#### Importanto dados ####
base = read_excel("C:\\Users\\julia\\Downloads\\AP-2010-SP-REDUZIDO-REV02.xlsx")



#### Tratamento dados ####
base$V011 = as.character(base$V011)
attach(base)
base_resumida <- data.table(AP0001,AP0002,AP0003,AP0004,AP0005,
                            AP0006,AP0007,AP0008,AP0009,AP0010,
                            AP0011,AP012,AP0013,AP0014,
                            AP015,AP0016,AP0019,AP0020,AP0062)

#Gráfico com as variáveis plotadas no primeiro plano
B1 = PCA(base_resumida, scale.unit=TRUE, graph=TRUE)

#gráfico de cotovelo para escolher o número de componentes ideal
scree_plot <- fviz_eig(B1, addlabels = TRUE, ylim = c(0, 55));scree_plot

#tabela com os autovalores e o quanto da variancia é explicada para cada componente
B1$eig

# tabela para entender a contribuicao de cada variavel em cada componente
B1$var$contrib

# tabela para entender a correlaçao de cada variavel em cada componente
B1$var$cor


#score de cada variavel dentro de cada componete
x = B1$ind$coord

x = as.data.frame(x)

base = subset(x,select = c(Dim.1,Dim.2,Dim.3,Dim.4,Dim.5)) 



library("writexl")
write_xlsx(base, path = "C:\\Users\\julia\\Downloads\\2024_02_29_Componentes.xlsx")














