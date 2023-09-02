# Microsoft Power BI Para Data Science, Versão 2.0

# ExercícioEstatística Básica na Linguagem R

setwd('C:/Jean/Arquivos/Educacional/Data Science Academy/Curso Power BI para Data Science/Cap12')
getwd()

#Carregando o Dataset

notas <- read.csv("Notas.csv" , fileEncoding = "windows-1252")

# Exercício 1: Apresente um resumo de tipos de dados e estatísticas do dataset.

View(notas)
str(notas)
summary(notas$TurmaA)
summary(notas$TurmaB)

# Exercício 2: Qual a média de cada turma?

mean(notas$TurmaA)
mean(notas$TurmaB)

# Exercício 3: Qual turma apresentou maior variabilidade de notas? Justifique sua resposta.



var(notas$TurmaA)
var(notas$TurmaB)
sd(notas$TurmaA)
sd(notas$TurmaB)

# A turmaA apresentar uma maior variabilidade que a turmaB, 
# devido ao valor da variância e desvio padrão serem maiores.

# Exercício 4 -Calcule o coeficiente de variação das 2 turmas.

media_ta <- mean(notas$TurmaA)
media_tb <- mean(notas$TurmaB)

sd_ta <- sd(notas$TurmaA)
sd_tb <- sd(notas$TurmaB)

cvA <- sd_ta / media_ta * 100
cvB <- sd_tb / media_ta * 100

cvA
cvB


# Exercício 5 -Qual nota apareceu mais vezes em cada turma?

moda <- function(v){
  valor_unico <- unique(v)
  valor_unico[which.max(tabulate(match(v, valor_unico)))]
}

modaturmaA <- moda(notas$TurmaA)
print(modaturmaA)

modaturmaB <- moda(notas$TurmaB)
print(modaturmaB)



