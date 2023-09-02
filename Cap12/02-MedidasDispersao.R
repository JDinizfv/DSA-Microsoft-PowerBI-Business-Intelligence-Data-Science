# Estatística Básica
#Parte 2 - Medidas de Dispersão

# Definiindo a pasta de trabalho
#  o caminho abaixo pela pasta do seu computador

setwd("C:/Jean/Arquivos/Educacional/Data Science Academy/Curso Power BI para Data Science/Cap12")

getwd()

#Carregando o dataset

vendas <- read.csv("Vendas.csv", fileEncoding = "windows-1252")

#Resumo do dataset
View(vendas)
str(vendas)
summary(vendas$Valor)

#Variância

var(vendas$Valor)

#Desvio padrão

sd(vendas$Valor)



