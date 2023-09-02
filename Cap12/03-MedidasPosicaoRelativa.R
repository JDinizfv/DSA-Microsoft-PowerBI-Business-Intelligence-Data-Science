# Estatística Básica
#Parte 2 - Medidas de Dispersão

# Definiindo a pasta de trabalho
#  o caminho abaixo pela pasta do seu computador

setwd("C:/Jean/Arquivos/Educacional/Data Science Academy/Curso Power BI para Data Science/Cap12")

getwd()

#Carregando o dataset

vendas <- read.csv("Vendas.csv", fileEncoding = "windows-1252")

# Resumo dos dados

head(vendas)
tail(vendas)
View(vendas)

#Medidas de Tendencia Central

summary(vendas$Valor)
summary(vendas[c('Valor','Custo')])

#Explorando Variáveis numéricas

mean(vendas$Valor)
median(vendas$Valor)
quantile(vendas$Valor)
quantile(vendas$Valor, probs = c(0.01,0.99))
quantile(vendas$Valor, seq(from=0, to = 1, by= 0.20))
IQR(vendas$Valor) #Diferenças entre q3 e q1
range(vendas$Valor)
summary(vendas$Valor)
diff(range(vendas$Valor))

