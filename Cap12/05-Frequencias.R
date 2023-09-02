# Estatística Básica
#Parte 5 - Tabela de Frequência

# Definiindo a pasta de trabalho
#  o caminho abaixo pela pasta do seu computador

setwd("C:/Jean/Arquivos/Educacional/Data Science Academy/Curso Power BI para Data Science/Cap12")

getwd()

# Carregando o Dataset

dados <- read.table("Usuarios.csv",dec=".",
                    sep = ",",
                    h = T,
                    fileEncoding = "windows-1252")
#Vizualizando e sumarizando os dados

View(dados)
names(dados)
str(dados)
summary(dados$salario)
summary(dados$grau_instrucao)
mean(dados$salario)
mean(dados$grau_instrucao) # erro e aviso de nao possibilidade retornando NA

#Tabela de Frequências Absolutas

freq <- table(dados$grau_instrucao)
View(freq)

#Tabela de Frequências Relativas

freq_rel <- prop.table(freq)
View(freq_rel)

# Porcentagem(100 * freq_rel_table)

p_freq_rel <- 100 * prop.table(freq_rel)
View(p_freq_rel)

# Adiciona linhas de total

View(freq)
freq <- c(freq, sum(freq))
View(freq)
names(freq)[4] <- "Total"
View(freq)

#Tabela final com todos os valores

freq_rel <- c(freq_rel, sum(freq_rel))
p_freq_rel <- c(p_freq_rel, sum(p_freq_rel))

# Tabela final com todos os vetores

tabela_final <- cbind(freq, 
                      freq_rel = round(freq_rel ,digits = 2),
                      p_freq_rel = round (p_freq_rel, digits = 2))

View(tabela_final)






