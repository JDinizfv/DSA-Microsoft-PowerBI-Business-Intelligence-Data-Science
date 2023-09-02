# Estatística Básica
#Parte 1 - Medidas de Posição

# Definiindo a pasta de trabalho
#  o caminho abaixo pela pasta do seu computador

setwd("C:/Jean/Arquivos/Educacional/Data Science Academy/Curso Power BI para Data Science/Cap12")

getwd()

# Carregando o Dataset

vendas <- read.csv("Vendas.csv", fileEncoding = "windows-1252")

# Resumo do Dataset

View(vendas)
str(vendas)
summary(vendas$Valor)
summary(vendas$Custo)

#Média
?mean
mean(vendas$Valor)
mean(vendas$Custo)

#Media Ponderada
?weighted.mean

weighted.mean(vendas$Valor, w = vendas$Custo )

#Mediana
?median

median(vendas$Valor)
median(vendas$Custo)

# Moda

moda <- function(v){
  valor_unico <- unique(v)
  valor_unico[which.max(tabulate(match(v, valor_unico)))]
  }

# obtendo a moda

resultado <- moda(vendas$Valor)
print(resultado)

resultado_custo <- moda(vendas$Custo)
print(resultado_custo)

#Criando Gráfico de Média de Valor por Estado com ggplot2

install.packages("ggplot2")
library(ggplot2)

# cria o gráfico

ggplot(vendas) + stat_summary(aes(x= Estado, y= Valor),
                              fun=mean,
                              geom="bar",
                              fill="lightgreen",
                              col= "grey50") +
  labs(title="Média de Valor por Estado")
                              
                          






