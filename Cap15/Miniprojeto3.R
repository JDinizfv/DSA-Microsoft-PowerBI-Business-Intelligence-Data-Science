#
#
#
#
# Mini projeto 3 - versão aluno

### Prevendo a Inadiplência de Clientes com Machine Learning e Power BI

# Definindo pasta de trabalho

setwd("C:/Jean/Arquivos/Educacional/Data Science Academy/Curso Power BI para Data Science/Cap15")
getwd()

# Definindo o problema
# Leia o manual em pdf do cap15 

#Instalando os pacotes para o projeto (basta uma vez)

install.packages("Amelia")
install.packages("caret")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape")
install.packages("randomForest")
install.packages("e1071")

## Carregando os pacotes

library(Amelia)
library(caret)
library(ggplot2)
library(dplyr)
library(reshape)
library(randomForest)
library(e1071)

#carregando o dataset (dados reais e com fontes no site)

dados_clientes <- read.csv("dados/dataset.csv")

# Visualizando os dados e a sua estrutura

View(dados_clientes)
str(dados_clientes)
summary(dados_clientes)
dim(dados_clientes)


######## Análise Exploratória , Limpeza e Transformação ##########

# Removendo a primeira coluna ID

dados_clientes$ID <- NULL
dim(dados_clientes)
View(dados_clientes)

# Renomeando a coluna de classe

colnames(dados_clientes)
colnames(dados_clientes)[24] <- "inadimplente"
colnames(dados_clientes)
View(dados_clientes)

# Verificando valores ausentes e removendo do dataset

sapply(dados_clientes, function(x) sum(is.na(x)))
?missmap

missmap(dados_clientes, main = "Valores Missing Observados")
dados_clientes <- na.omit(dados_clientes)

# Convertendo atributos de genero , escolaridade , estado civil e idade
# para fatores (categorias)

# Renomeando colunas categoricas

colnames(dados_clientes)
colnames(dados_clientes)[2] <- "Genero"
colnames(dados_clientes)[3] <- "Escolaridade"
colnames(dados_clientes)[4] <- "Estado_Civil"
colnames(dados_clientes)[5] <-"Idade"
colnames(dados_clientes)
View(dados_clientes)

str(dados_clientes)

# Genero

View(dados_clientes$Genero)
str(dados_clientes$Genero)
summary(dados_clientes$Genero)
?cut

dados_clientes$Genero <- cut(dados_clientes$Genero, c(0,1,2) ,labels= c("Masculino","Feminino"))

View(dados_clientes$Genero)
str(dados_clientes$Genero)
View(dados_clientes)
summary(dados_clientes$Genero)


# Escolaridade 

View(dados_clientes$Escolaridade)
str(dados_clientes$Escolaridade)
summary(dados_clientes$Escolaridade)
dados_clientes$Escolaridade <- cut(dados_clientes$Escolaridade, c(0,1,2,3,4) , labels=c("Pos Graduado" , "Graduado", "Ensino Medio", "Outros"))
str(dados_clientes$Escolaridade)
summary(dados_clientes$Escolaridade)

dados_clientes <- na.omit(dados_clientes)

# Estado Civil 

str(dados_clientes$Estado_Civil)
summary(dados_clientes$Estado_Civil)

dados_clientes$Estado_Civil <- cut(dados_clientes$Estado_Civil, c(-1,0,1,2,3), labels = c("Desconhecido" , "Casado", "Solteiro", "Outro" ))

View(dados_clientes$Estado_Civil)
str(dados_clientes$Estado_Civil)
summary(dados_clientes$Estado_Civil)

#Convertendo a variável para o tipo fator com faixa etária

str(dados_clientes$Idade)
summary(dados_clientes$Idade)
hist(dados_clientes$Idade)

dados_clientes$Idade <- cut(dados_clientes$Idade, c(0,30,50,100), labels=c("Jovem", "Adulto","Idoso"))

View(dados_clientes$Idade)
str(dados_clientes$Idade)
summary(dados_clientes$Idade)
hist(dados_clientes$Idade)


# Convertendo a variavel que indica pagamentos para o tipo fator

dados_clientes$PAY_0 <- as.factor(dados_clientes$PAY_0)
dados_clientes$PAY_2 <- as.factor(dados_clientes$PAY_2)
dados_clientes$PAY_3 <- as.factor(dados_clientes$PAY_3)
dados_clientes$PAY_4 <- as.factor(dados_clientes$PAY_4)
dados_clientes$PAY_5 <- as.factor(dados_clientes$PAY_5)
dados_clientes$PAY_6 <- as.factor(dados_clientes$PAY_6)

# Dataset após as conversões

str(dados_clientes)
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main="Valores Missing Observados")
dados_clientes <- na.omit(dados_clientes)
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main = " Valores Missing Observados")
dim(dados_clientes)

# Alterando a variável dependente para o tipo fator

str(dados_clientes$inadimplente)
colnames(dados_clientes)
View(dados_clientes)
dados_clientes$inadimplente <- as.factor(dados_clientes$inadimplente)
str(dados_clientes$inadimplente)
str(dados_clientes)

View(dados_clientes)

# Total de inadimplentes x não-inadimplentes
?table
table(dados_clientes$inadimplente)

# Vejamos as porcentagens entre as classes

prop.table(table(dados_clientes$inadimplente))

#plot da distribuição usando ggplot2

qplot(inadimplente, data = dados_clientes, geom = "bar") + theme(axis.text.x = element_text(angle =90 , hjust = 1 ) )

# Set seed 

set.seed(12345)


# amostragem estratificada 
# Seleciona as linhas de acordo com a variavel inadimplente como strata

?createDataPartition
indice <- createDataPartition(dados_clientes$inadimplente, p=0.75, list = FALSE)
dim(indice)


# Definimos os dados de treinamento como subconjunto do conjunto de dados original 
# Com números de indice de linha (conforme identificado acima) e todas as colunas

dados_treino <- dados_clientes[indice,]
dim(dados_treino)
table(dados_treino$inadimplente)

# Veja porcentagem entre as classes 
prop.table(table(dados_treino$inadimplente))

# Número de registros no dataset de treinamento 
dim(dados_treino)

# Comparamos as porcentagens entre as classes de treinamento e dados originais 

compara_dados <- cbind(prop.table(table(dados_treino$inadimplente)), prop.table(table(dados_clientes$inadimplente)))

colnames(compara_dados) <- c("Treinamento","Original")
compara_dados

# Melt Data - Converte colunas em linhas

?reshape2::melt
melt_compara_dados <- melt(compara_dados)
melt_compara_dados

# Plot para ver a distribuição do treinamento vs original 

ggplot(melt_compara_dados, aes(x = X1, y = value)) + geom_bar(aes(fill = X2), stat = "identity" , position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90 , hjust =1))

# Tudo o qu não está no dataset de treinamento está no dataset de teste . Observe o sinal - (menci)

dados_teste <- dados_clientes[-indice,]
dim(dados_teste)
dim(dados_treino)



###### Modelo de Machine Learning ##########


# Construindo a primeira versão do modelo

?randomForest
modelo_v1 <- randomForest(inadimplente ~., data = dados_treino)
modelo_v1

# Avaliando o modelo

plot(modelo_v1)

# Previsões com dados de teste

previsões_v1 <- predict(modelo_v1,dados_teste)

# Confusion Matrix

?caret::confusionMatrix

cm_v1 <- caret::confusionMatrix(previsões_v1, dados_teste$inadimplente, positive="1")
cm_v1

# Calculando Precision, Recall e F1-Score , métricas de avaliação do modelo preditivo

y <- dados_teste$inadimplente
y_pred_v1 <- previsões_v1

precision <- posPredValue(y_pred_v1, y)
precision

recall <- sensitivity(y_pred_v1,y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1

# Balanceamento de classe

install.packages(c("zoo","xts","quantmod")) ## and perhaps mode

install.packages( "C:/Jean/Arquivos/Educacional/Data Science Academy/Curso Power BI para Data Science/Cap15/DMwR", repos=NULL, type="source" )
install.packages("DMwR_0.4.1.tar.gz")

install.packages("performanceEstimation")
library(performanceEstimation)

?performanceEstimation

# Aplicando o Smote - smote : synthetic Minority Over-Sampling  Technique

table(dados_treino$inadimplente)
prop.table(table(dados_treino$inadimplente))
set.seed(9560)

dados_treino_bal <- smote(inadimplente ~.,data = dados_treino)
table(dados_treino_bal$inadimplente)
prop.table(table(dados_treino_bal$inadimplente))

# Construindo a segunda versão do modelo

modelo_v2 <- randomForest(inadimplente ~., data = dados_treino_bal)
modelo_v2


# Avaliação da segunda versão

plot(modelo_v2)

# Previsões com dados de teste

previsões_v2 <- predict(modelo_v2,dados_teste)

# Confusion Matrix

?caret::confusionMatrix

cm_v2 <- caret::confusionMatrix(previsões_v2, dados_teste$inadimplente, positive="1")
cm_v2

# Calculando Precision, Recall e F1-Score , métricas de avaliação do modelo preditivo

y <- dados_teste$inadimplente
y_pred_v2 <- previsões_v2

precision <- posPredValue(y_pred_v2, y)
precision

recall <- sensitivity(y_pred_v2,y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1


# Importância das Variaveis preditoras para as previsões

View(dados_treino_bal)
varImpPlot(modelo_v2)

#Obtendo as variaveis mais importantes

imp_var <- importance(modelo_v2)
varImportance <- data.frame(variables = row.names(imp_var), Importance = round(imp_var[ , 'MeanDecreaseGini'],2))

# Criando o rank de variaveis baseado na importancia 

rankImportance <- varImportance %>%
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))

# Usando o ggplot2 para visualizar a importância relativa das variaveis

ggplot(rankImportance, aes(x = reorder(variables, Importance),
                           y=Importance,
                           fill = Importance)) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(x= variables, y= 0.5, label = Rank),
            hjust= 0,
            vjust = 0.55,
            size = 4,
            colour= 'red') + 
  labs(x = 'variables') + coord_flip()

##### não to conseguindo fazer esse gráfico -> Error in reorder(Variables, Importance) : object 'Variables' not found

# Construindo a terceira versão do modelo apenas com as variaveis mais importantes

colnames(dados_treino_bal)

modelo_v3 <- randomForest(inadimplente ~ PAY_0+ PAY_2 + PAY_3 + PAY_AMT1 + PAY_AMT2 + PAY_5 + BILL_AMT1, data = dados_treino_bal )
modelo_v3 
        
# Avaliando o modelo

plot(modelo_v3)

# Previsões com dados de teste

previsoes_v3 <- predict(modelo_v3, dados_teste)

# Confusion matrix

?caret::confusionMatrix

cm_v3 <- caret::confusionMatrix(previsoes_v3 , dados_teste$inadimplente ,positive = "1" )
cm_v3

# Calculando Precision , Recall e F1-score, métricas de avaliação do modelo preditivo

y <- dados_teste$inadimplente

y_pred_v3 <- previsoes_v3

precision <- posPredValue(y_pred_v3, y)
precision

recall <- sensitivity(y_pred_v3, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1


# Salvar o modelo em disco
?saveRDS
saveRDS(modelo_v3, file = "C:Jean/Arquivos/Educacional/Data Science Academy/Curso Power BI para Data Science/Cap15/modelo_v3.rds" )
saveRDS(modelo_v3 , file = "modelo/modelo_v3.rds")


# Carregando o modelo
?readRDS
modelo_final <- readRDS("C:Jean/Arquivos/Educacional/Data Science Academy/Curso Power BI para Data Science/Cap15/modelo_v3.rds" )

modelo_final <- readRDS("modelo/modelo_v3.rds")

###### Não conseguir salvar e carregar



# Dados dos Clientes

PAY_0 <- c(0,0,0)
PAY_2 <- c(0,0,0)
PAY_3 <- c(1,0,0)
PAY_AMT1 <- c(1100,1000,1200)
PAY_AMT2 <- c(1500, 1300, 1150)
PAY_5 <- c(0,0,0)
BILL_AMT1 <- c(350,420,280)

# Concatena em um dataframe

novos_clientes <- data.frame(PAY_0, PAY_2, PAY_3, PAY_AMT1,PAY_AMT2, PAY_5, BILL_AMT1)
View(novos_clientes)

# Previsões 

previsoes_novos_clientes <- predict(modelo_final, novos_clientes)

# Checando os tipos de dados

str(dados_treino_bal)
str(novos_clientes)

# Convertendo os tipos de dados

novos_clientes$PAY_0 <- factor(novos_clientes$PAY_0, levels = levels(dados_treino_bal$PAY_0))
novos_clientes$PAY_2 <- factor(novos_clientes$PAY_2, levels = levels(dados_treino_bal$PAY_2))
novos_clientes$PAY_3 <- factor(novos_clientes$PAY_3, levels = levels(dados_treino_bal$PAY_3))
novos_clientes$PAY_5 <- factor(novos_clientes$PAY_5, levels = levels(dados_treino_bal$PAY_5))
str(novos_clientes)

# previsoes

previsoes_novo_cliente <- predict(modelo_final, novos_clientes)
View(previsoes_novo_cliente)