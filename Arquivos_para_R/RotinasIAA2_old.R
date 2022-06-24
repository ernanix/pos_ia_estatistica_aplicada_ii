############# ROTINAS DE ESTATISTICA APLICADA II #########

########################## PARTE 1 #######################

install.packages("plyr")
install.packages("readr")
install.packages("dplyr")
install.packages("caret")
install.packages("ggplot2")
install.packages("repr")
install.packages("glmnet")

library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(glmnet)

load("D:/Cursos/Pos_IA/Estatistica_aplicada_II/Arquivos_para_R/wage.RData")

dat <- wage
glimpse(dat)
gc()
#### Data partioning #######

set.seed(302) 

index = sample(1:nrow(dat), 0.8*nrow(dat)) 

train = dat[index,] # Create the training data 
test = dat[-index,] # Create the test data

dim(train)
dim(test)

#### Scaling variables ########

cols = c('husage', 'husearns', 'huseduc', 'hushrs', 'earns',
         'age', 'educ', 'hrwage')

pre_proc_val <- preProcess(train[,cols], method = c("center", "scale"))

train[,cols] = predict(pre_proc_val, train[,cols])
test[,cols] = predict(pre_proc_val, test[,cols])

summary(train)
summary(test)

###### Regressco Ridge  ###########
## Regressco ridge reduz os coeficientes

cols_reg = c('husage', 'husearns', 'huseduc', 'hushrs', 
             'earns', 'age', 'educ', 'hrwage','husblck',
             'hushisp', 'kidge6', 'black', 'hispanic',
             'union', 'kidlt6')

dummies <- dummyVars(hrwage ~ husage+husearns+huseduc+hushrs+ 
                     earns+age+educ+husblck+hushisp+kidge6+
                     black+hispanic+union+kidlt6, 
                     data = dat[,cols_reg])

train_dummies = predict(dummies, newdata = train[,cols_reg])

test_dummies = predict(dummies, newdata = test[,cols_reg])

print(dim(train_dummies)); print(dim(test_dummies))

# A regressco Ridge i uma extensco da regressco linear em 
# que a fungco de perda i modificada para minimizar a 
# complexidade do modelo. Essa modificagco i feita 
# adicionando um parbmetro de penalidade equivalente ao 
# quadrado da magnitude dos coeficientes.

# Uma das principais diferengas entre os modelos de regressco
# linear e regularizada i que o zltimo envolve o ajuste de
# um hiperparbmetro, lambda. O csdigo executa o modelo 
# glmnet() varias vezes para diferentes valores de lambda. 
# Podemos automatizar essa tarefa de encontrar o valor lambda
# ideal usando a fungco cv.glmnet(). Isso i feito usando as
# linhas de csdigo abaixo.

# A fungco perda i dada por:
# Loss function = OLS+lambda*summation(squared coefficient
# values)
# Lambda i o parbmetro de penalidade que selecionamos

# The data for model

x = as.matrix(train_dummies)
y_train = train$hrwage

x_test = as.matrix(test_dummies)
y_test = test$hrwage

### The optimal lambda value ########
lambdas <- 10^seq(2, -3, by = -.1)
ridge_lamb <- cv.glmnet(x, y_train, alpha = 0, 
                      lambda = lambdas)
best_lambda_ridge <- ridge_lamb$lambda.min
best_lambda_ridge

ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, 
                   family = 'gaussian', 
                   lambda = best_lambda_ridge)

summary(ridge_reg)

# Obtendo os valores dos parbmetros

ridge_reg[["beta"]]

# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

# Prediction and evaluation on train data
predictions_train <- predict(ridge_reg, s = best_lambda_ridge,
                             newx = x)
eval_results(y_train, predictions_train, train)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s = best_lambda_ridge, 
                            newx = x_test)
eval_results(y_test, predictions_test, test)

# Prediction for our example

# Como os valores de entrada no modelo estco normalizados,
# temos de normalizar tambim os dados que queremos prever
# Observe que as variaveis dummies nco sofrem normalizagco

#Para:
##Usadas as medianas no teste
# husage = 40 anos
husage = (40-pre_proc_val[["mean"]][["husage"]])/
                          pre_proc_val[["std"]][["husage"]]
# husearns = 551
husearns = (551-pre_proc_val[["mean"]][["husearns"]])/
                          pre_proc_val[["std"]][["husearns"]]

# huseduc = 13
huseduc = (13-pre_proc_val[["mean"]][["huseduc"]])/
                          pre_proc_val[["std"]][["huseduc"]]

# husblck = 0
husblck = 0

# hushisp = 0
hushisp = 0

# hushrs = 40
hushrs = (40-pre_proc_val[["mean"]][["hushrs"]])/
                          pre_proc_val[["std"]][["hushrs"]]

# kidge6 = 0
kidge6 = 0

# earns = 355.5
earns = (355.5-pre_proc_val[["mean"]][["earns"]])/
                          pre_proc_val[["std"]][["earns"]]

# age = 37 anos 
age = (37-pre_proc_val[["mean"]][["age"]])/
                          pre_proc_val[["std"]][["age"]]

# black = 0
black = 0

# educ = 13
educ = (13-pre_proc_val[["mean"]][["educ"]])/
                          pre_proc_val[["std"]][["educ"]]

# hispanic = 0
hispanic = 0

# union = 0
union = 0

# kidlt6 = 0
kidlt6 = 0

# Construindo uma matriz com os dados para predigco

our_pred = as.matrix(data.frame(husage=husage, 
                                husearns=husearns,
                                huseduc=huseduc,
                                husblck=husblck,
                                hushisp=hushisp,
                                hushrs=hushrs,
                                kidge6=kidge6,
                                earns=earns,
                                age=age,
                                black=black,
                                educ=educ,
                                hispanic=hispanic,
                                union=union,
                                kidlt6=kidlt6))
# Fazendo a predigco

predict_our_ridge <- predict(ridge_reg, s = best_lambda_ridge, 
                     newx = our_pred)
predict_our_ridge

# O resultado i uma informagco normalizada, vamos 
# convertj-la em valor nominal, compatmvel com a base de 
# dados original
 
wage_pred_ridge=(predict_our_ridge*
                   pre_proc_val[["std"]][["hrwage"]])+
                   pre_proc_val[["mean"]][["hrwage"]]
wage_pred_ridge

# Confident intervals for our example

n <- nrow(train)
m <- wage_pred_ridge
s <- pre_proc_val[["std"]][["hrwage"]]
dam <- s/sqrt(n)
CIlwr_ridge <- m + (qnorm(0.025))*dam
CIupr_ridge <- m - (qnorm(0.025))*dam 

# Sal??rio varia entre um e outro valor
CIlwr_ridge
CIupr_ridge

##### Regressco Lasso ####
## Leva a zero os coeficientes nco significativos

# A regressco Lasso, ou o Operador de Encolhimento Absoluto
# Mmnimo e Selegco, tambim i uma modificagco da regressco
# linear. No lasso, a fungco de perda i modificada para
# minimizar a complexidade do modelo, limitando a soma dos
# valores absolutos dos coeficientes do modelo 
# (tambim chamado de l1-norm).
# O uso de uma restrigco l1-norm forga alguns valores de
# peso a zero para permitir que outros coeficientes assumam
# valores diferentes de zero.

# A fungco perda i dada por:
# Loss function = OLS+lambda*summation(absolute values of
# the magnitude of the coefficients)

# Escolhendo  melhor lambda como parbmetro

lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_lamb <- cv.glmnet(x, y_train, alpha = 1, 
                       lambda = lambdas, 
                       standardize = TRUE, nfolds = 5)

# Best 
best_lambda_lasso <- lasso_lamb$lambda.min 
best_lambda_lasso

lasso_model <- glmnet(x, y_train, alpha = 1, 
                      lambda = best_lambda_lasso, 
                      standardize = TRUE)

# Visualizando os parbmetros calculados

lasso_model[["beta"]]

# Fazendo as predigues a avaliando o modelo lasso nas bases 
# de treino e teste

predictions_train <- predict(lasso_model, s = best_lambda_lasso,
                             newx = x)
eval_results(y_train, predictions_train, train)

predictions_test <- predict(lasso_model, s = best_lambda_lasso, 
                            newx = x_test)
eval_results(y_test, predictions_test, test)

## Prediction for our example

# Fazendo a predigco baseada nos mesmos parbmetros da 
# regressco ridge

predict_our_lasso <- predict(lasso_model, s = best_lambda_lasso, 
                          newx = our_pred)
predict_our_lasso

# Novamente, a informagco que retorna i normalizada, temos
# de convertj-la em valor compatmvel com a base de dados 
# original

wage_pred_lasso=(predict_our_lasso*
                   pre_proc_val[["std"]][["hrwage"]])+
                   pre_proc_val[["mean"]][["hrwage"]]
wage_pred_lasso

# Confident intervals for our example

n <- nrow(train)
m <- wage_pred_lasso
s <- pre_proc_val[["std"]][["hrwage"]]
dam <- s/sqrt(n)
CIlwr_lasso <- m + (qnorm(0.025))*dam
CIupr_lasso <- m - (qnorm(0.025))*dam 

CIlwr_lasso
CIupr_lasso

########### Regressco ElasticNet ###########################

#A regressco ElasticNet combina as propriedades de regressco
# Ridge e lasso. Ele funciona penalizando o modelo usando 
# tanto a l2-norm quanto a l1-norm. O modelo pode ser 
# facilmente construmdo usando o pacote caret, que seleciona
# automaticamente o valor ideal dos parbmetros.

# Set training control
train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)

# Train the model

# Note que aqui nco temos o parbmetro alpha, porque a
# regressco elasticnet vai seleciona-lo automaticamente com
# valor entre 0 e 1; Para a regressco ridge alpha=0 e 
# lasso alpha=1
# Na elasticnet o parbmetro lambda tambim i encontrado por
# cross-validation

elastic_reg <- train(hrwage ~ husage+husearns+huseduc+hushrs+ 
                       earns+age+educ+husblck+hushisp+kidge6+
                       black+hispanic+union+kidlt6,
                     data = train,
                     method = "glmnet",
                     tuneLength = 10,
                     trControl = train_cont)

# Best tuning parameter
elastic_reg$bestTune

# And the parameters are:

elastic_reg[["finalModel"]][["beta"]]

# Fazendo as predigues nas bases de treino e teste e
# avaliando o modelo

# Make predictions on training set
predictions_train <- predict(elastic_reg, x)
eval_results(y_train, predictions_train, train) 

# Make predictions on test set
predictions_test <- predict(elastic_reg, x_test)
eval_results(y_test, predictions_test, test)

## Prediction for our example

predict_our_elastic <- predict(elastic_reg,our_pred)
predict_our_elastic

# Novamente, a informagco que retorna i normalizada, temos
# de convertj-la em valor compatmvel com a base de dados 
# original

wage_pred_elastic=(predict_our_elastic*
                     pre_proc_val[["std"]][["hrwage"]])+
                     pre_proc_val[["mean"]][["hrwage"]]
wage_pred_elastic

# Confident intervals for our example

n <- nrow(train)
m <- wage_pred_elastic
s <- pre_proc_val[["std"]][["hrwage"]]
dam <- s/sqrt(n)
CIlwr_elastic <- m + (qnorm(0.025))*dam
CIupr_elastic <- m - (qnorm(0.025))*dam 

CIlwr_elastic
CIupr_elastic

###################### FIM DA PARTE 1 #######################

################# Testes nco Paramitricos ###################

###### One-Sample Wilcoxon Signed Rank Test #################

# Preparando os dados

# Usaremos um conjunto de dados de exemplo contendo o peso
# de 10 ratos. Queremos saber se o peso midio dos camundongos
# difere de 25g?

load("D:/Cursos/Pos_IA/Estatistica_aplicada_II/Arquivos_para_R/data_rats.Rdata")

# Sumario estatmstico do peso

summary(data_rats$weight)


# Visualize seus dados usando box plots

library(ggpubr)
ggboxplot(data_rats$weight, 
          ylab = "Weight (g)", xlab = FALSE,
          ggtheme = theme_minimal())

mean(data_rats$weight)

# Calcular teste de Wilcoxon de uma amostra

# Queremos saber se o peso midio dos ratos difere de 25g
# (teste bicaudal)

# One-sample wilcoxon test
res <- wilcox.test(data_rats$weight, mu = 25)
res 

# H0: peso dos ratos i igual estatisticamente a 25g
# HA: peso dos ratos i estisticamente diferente de 25g

# O p-value do teste i 0,001953, que i menor que o nmvel de 
# significbncia alfa = 0.05. Podemos rejeitar a hipstese 
# nula e concluir que o peso midio dos camundongos i 
# significativamente diferente de 25g com um p-value de
# p = 0,001953.

# se vocj deseja testar se o peso midio dos camundongos i 
# inferior a 25g (teste unicaudal), use a seguinte fungco:

wilcox.test(data_rats$weight, mu = 25,
            alternative = "less")

# H0: o peso midio dos ratos i maior que 25g
# HA: o peso midio dos ratos i menor que 25g

# data:  data_rats$weight
# V = 0, p-value = 0.0009766
# alternative hypothesis: true location is less than 25

# Como p-value < 0.05, rejeita-se H0, o peso midio dos ratos
# i menor que 25g

###### se vocj quiser testar se o peso midio dos camundongos 
# i maior que 25g (teste unicaudal), use a seguinte fungco:

wilcox.test(data_rats$weight, mu = 25,
            alternative = "greater")

# H0: o peso midio dos ratos i menor que 25g
# HA: o peso midio dos ratos i maior que 25g

# data:  data_rats$weight
# V = 0, p-value = 1
# alternative hypothesis: true location is greater than 25

# Como p-value > 0.05 aceita-se H0, o peso midio dos ratos
# i menor que 25g



###### Unpaired Two-Samples Wilcoxon Test para ############# 
#### independjncia de grupos/amostras desemparelhadas ###### 
###############(ou nco emparelhadas)########################


# Preparando os dados

# Dados em dois vetores numiricos

women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4,
                  48.8, 48.5)

men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3,
                62.4) 

# Criar um data frame

weight <- data.frame( 
  group = rep(c("Woman", "Man"), each = 9),
  weight = c(women_weight,  men_weight)
)


# Queremos saber se o peso mediano das mulheres difere do
# peso mediano dos homens.

# Sumario estatmstico

library(dplyr)

group_by(weight, group) %>%
  summarise(
    count = n(),
    median = median(weight, na.rm = TRUE),
    IQR = IQR(weight, na.rm = TRUE)
  )

#Visualize seus dados usando box plots

# Plote "weight" por groupo

library("ggpubr")
ggboxplot(weight, x = "group", y = "weight", 
          color = "group", palette=c("#00AFBB", "#E7B800"),
          ylab = "Weight", xlab = "Groups")

# Teste se o peso mediano dos homens i igual ao peso mediano
# das mulheres
# O teste i sempre feito com relagco ao disposto no vetor de
# teste, no caso do zltimo para o primeiro - No vetor: 
# Man contra Woman

res <- wilcox.test(weight ~ group, data = weight,
                   exact = FALSE)
res

# O p-value do teste i 0,02712, que i menor que o nmvel de
# significbncia alfa = 0,05. Podemos concluir que o peso 
# mediano dos homens i significativamente diferente do peso
# mediano das mulheres.

# Observe que:
# se vocj quiser testar se o peso mediano dos homens i menor
# que o peso mediano das mulheres, use a seguinte fungco:

wilcox.test(weight ~ group, data = weight, 
            exact = FALSE, alternative = "less")

# Como p-value > 0.05 o peso mediano dos homens nco i menor
# que o peso mediano das mulheres

# se vocj quiser testar se o peso mediano dos homens i maior
# que o peso mediano das mulheres, use a seguinte fungco:

wilcox.test(weight ~ group, data = weight,
            exact = FALSE, alternative = "greater")

# como p-value < 0.05 o peso mediano dos homens i maior que
# o peso mediano das mulheres

# Entco conclui-se que o peso mediano dos homens i maior que
# o peso mediano das mulheres


############################################################
########Teste de Wilcoxon para amostras pareadas ###########

# Carregando os dados

# Usaremos um conjunto de dados de exemplo, que contim o
# peso de 10 ratos antes e depois do tratamento.

load ("D:/Cursos/Pos_IA/Estatistica_aplicada_II/Arquivos_para_R/paired_weight.Rdata")

# Queremos saber se existe alguma diferenga significativa
# nos pesos medianos antes e depois do tratamento?

# Calcule estatmsticas resumidas por grupos:

library("dplyr")
group_by(paired_weight, group) %>%
  summarise(
    count = n(),
    median = median(weight, na.rm = TRUE),
    IQR = IQR(weight, na.rm = TRUE)
  )

# Visualize seus dados usando box plots
# Plote os pesos por grupo

library("ggpubr")

ggboxplot(paired_weight, x = "group", y = "weight", 
          color = "group", palette=c("blue", "red"),
          order = c("before", "after"),
          ylab = "Weight", xlab = "Groups")


# Plote os dados emparelhados:

# Subconjunto de dados de peso antes do tratamento

before <- subset(paired_weight,  group == "before", weight,
                 drop = TRUE)

# Subconjunto de dados de peso depois do tratamento

after <- subset(paired_weight,  group == "after", weight,
                drop = TRUE)

# Plote os dados emparelhados

library(PairedData)

pd <- paired(before, after)
pd
plot(pd, type = "profile") + theme_bw()

# Calcule o teste

res <- wilcox.test(weight ~ group, data = paired_weight, 
                   paired = TRUE)
res

# O p-value do teste i 0.001953, que i menor que o nmvel
# de significbncia alfa = 0,05. Podemos concluir que o 
# peso midio dos camundongos antes do tratamento i 
# significativamente diferente do peso midio apss o 
# tratamento

# se vocj quiser testar se o peso mediano antes do 
# tratamento i menor do que o peso mediano apss o 
# tratamento, use a seguinte fungco:

wilcox.test(weight ~ group, data = paired_weight, 
            paired = TRUE,alternative = "less")

# Como o p-value > 0.05 conclui-se que o peso mediano antes
# do tratamento i menor que o peso mediano apss tratamento

# se vocj quiser testar se o peso mediano antes do 
# tratamento i maior do que o peso mediano apss o
# tratamento, use a seguinte fungco:

wilcox.test(weight ~ group, data = paired_weight, 
            paired = TRUE,alternative = "greater")

# Como o p-value < 0.05 conclui-se que o peso mediano antes
# do tratamento nco i maior do que o peso mediano apss o
# tratamento.

######## Teste de Kruskal-Wallis para Comparar #############
#### dois ou mais grupos ou amostras independentes #########

# Carregar dados de pesos de plantas em 3 condigues
# experimentais

my_data <- PlantGrowth

# Na terminologia R, a coluna "group" i chamada de fator e 
# as diferentes categorias ("ctr", "trt1", "trt2") sco 
# chamadas de nmveis de fator. Os nmveis sco ordenados
# alfabeticamente.

# Mostrar os grupos

levels(my_data$group)

# Se os nmveis nco estiverem automaticamente na ordem 
# correta, reordene-os da seguinte forma:

my_data$group <- ordered(my_data$group,
                         levels = c("ctrl", "trt1", 
                                    "trt2"))

# Calcule estatmsticas resumidas por grupos:

library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE),
    median = median(weight, na.rm = TRUE),
    IQR = IQR(weight, na.rm = TRUE)
  )

# Visualize os dados usando box-plots

library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800",
                                       "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")

# Grafico do peso por grupo
# Adicionando barras de erro: mean_se (erro quadrado midio)

library("ggpubr")
ggline(my_data, x = "group", y = "weight", 
       add = c("mean_se", "jitter"), 
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")


# Calculo do teste de Kruskal-Wallis

# Queremos saber se existe alguma diferenga significativa
# entre os pesos midios das plantas nas 3 condigues 
# experimentais.

kruskal.test(weight ~ group, data = my_data)

qchisq(0.95,2)
# Como o valor de p (0.01842) i inferior ao nmvel de 
# significbncia 0,05, podemos concluir que existem 
# diferengas significativas entre os grupos de tratamento.

### Comparagco mzltipla de pares entre grupos

# A partir do resultado do teste de Kruskal-Wallis, sabemos
# que ha uma diferenga significativa entre os grupos, mas 
# nco sabemos quais pares de grupos sco diferentes.

# I possmvel usar a fungco pairwise.wilcox.test () para 
# calcular comparagues de pares entre os nmveis do grupo
# com corregues para testes mzltiplos.

pairwise.wilcox.test(PlantGrowth$weight, PlantGrowth$group,
                     p.adjust.method = "BH")

# BH i a ticnica de ajuste de Benjamini & Hochberg (1995)

# A comparagco entre pares mostra que, apenas trt1 e trt2
# sco significativamente diferentes pois p < 0.05

# Teste de Dunn com a metodologia de "BY", 
# Benjamini & Yekutieli (2001)

library (dunn.test)
library (PMCMR)
library (PMCMRplus)

dunn.test(my_data$weight, my_data$group, method="by", 
          list=TRUE)

# O teste de Dunn corroborou o teste pela ticnica "BH", 
# apenas trt1 e trt2 sco significativamente diferentes pois
# p < 0.05

### Teste de Nemenyi com ajustamento de Tukey

posthoc.kruskal.nemenyi.test(weight ~ group, data = my_data,
                             dist="Tukey")

# O teste apresentou ligagues entre valores e nco assegura
# a veracidade dos p-values, mas os demais testes sco 
# suficientes. Para outra base de dados este teste pode ser
# conveniente

##### Teste de Friedman para comparar dois ou mais ########
############# grupos/amostras pareadas #################### 

library(tidyverse)
library(ggpubr)
library(rstatix)

# Preparando os dados

data("selfesteem", package = "datarium") 
# escore de autoestima para 10 individuos, em 3 momentos de
# uma dieta

head(selfesteem, 3) # mostra as 3 primeiras linhas

# Rezna as colunas t1, t2 e t3 no formato long. Converta 
# variaveis id e de tempo em variaveis de fator 
# (ou agrupamento):

selfesteem <- selfesteem %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)
head(selfesteem, 3)

# Estatmsticas de resumo

selfesteem %>%
  group_by(time) %>%
  get_summary_stats(score, type = "common")

# Visualizagco

ggboxplot(selfesteem, x="time", y="score", add="jitter")

# Calculo da estatmstica

# Usaremos a fungco friedman_test() compatmvel com 
# [pacote rstatix].

res.fried <- selfesteem %>% friedman_test(score ~ time |id)
res.fried

# O escore de autoestima foi diferente estatisticamente
# significante, p-value < 0.05, nos diferentes momentos
# durante a dieta, X2 (2)= qui-quad 2 gl = 18.2, 
# p = 0,000112.

# Tamanho do efeito

selfesteem %>% friedman_effsize(score ~ time |id)

# Um grande tamanho do efeito foi detectado ("large" nos
# resultados), W = 0.91.

# Mzltiplas comparagues de pares
# A partir do resultado do teste de Friedman, sabemos que
# ha uma diferenga significativa entre os grupos, mas nco
# sabemos quais pares de grupos sco diferentes.

# Um teste de Friedman significativo pode ser seguido de
# testes de classificagco sinalizada de Wilcoxon aos pares
# para identificar quais grupos sco diferentes.

# Observe que os dados devem ser ordenados corretamente
# pela variavel (id) para que a primeira observagco para o
# tempo t1 seja pareada com a primeira observagco para o 
# tempo t2 e assim por diante.

#### Comparagues de pares usando teste de postos sinalizados
# de Wilcoxon pareado
# Os p-values sco ajustados usando o mitodo de corregco de 
# teste mzltiplo de Bonferroni.

# Comparagues por pares

pwc <- selfesteem %>%
  wilcox_test(score ~ time, paired = TRUE, 
              p.adjust.method = "bonferroni")
pwc

# Todas as diferengas entre pares sco estatisticamente
# significativas Todos tem "estrelas" = p-value < 0.05.

# Observe que tambim i possmvel realizar comparagues de 
# pares usando o Teste de Sinal, que pode nco ter forga 
# para detectar diferengas em conjuntos de dados 
# emparelhados. No entanto, i ztil porque tem poucas 
# suposigues sobre as distribuigues dos dados a serem 
# comparados.

# Comparagues de pares usando teste de sinal:

pwc2 <- selfesteem %>%
  sign_test(score ~ time, p.adjust.method = "bonferroni")
pwc2

## O teste corroborou o teste de wilcoxon com excegco entre 
# t2 e t3 nco foi significativo

# Relatsrio final dos testes 

# O escore de autoestima foi estatisticamente significativo 
# nos diferentes momentos usando o teste de Friedman, 
# X2 (2) = 18,2, p = 0,00011.

# O teste de classificagco sinalizada de Wilcoxon pairwise 
# entre os grupos revelou diferengas estatisticamente 
# significativas no escore de autoestima entre t1 e t2
# (p = 0,006); t1 e t3 (0,006); t2 e t3 (0,012).

# Visualizagco: boxplots com p-values

pwc <- pwc %>% add_xy_position(x = "time")
ggboxplot(selfesteem, x="time", y="score", add="point")+
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

############### TESTES PARAMITRICOS ######################## 

#### Teste uma amostra (one-sample t test) #################


# Usaremos um conjunto de dados de exemplo contendo o peso 
# de 10 ratos.
# Queremos saber se o peso midio dos ratos difere de 25g?

# install.packages("ggpubr")

library(ggpubr)

load("C:/iaa/data_rats.Rdata" )

####Teste preliminar para verificar as suposigues do teste t
# de uma amostra
# Esta i uma amostra grande? - Nco, porque n <30.
# Visto que o tamanho da amostra nco i grande o suficiente 
# (menor de 30, teorema do limite central),precisamos 
# verificar se os dados seguem uma distribuigco normal.

# Teste de Shapiro-Wilk:
# Hipstese nula: os dados sco normalmente distribumdos
# Hipstese alternativa: os dados nco sco normalmente distrib.

shapiro.test(data_rats$weight) # => p-value = 0.7846

# A partir do resultado, o p-value i maior do que o nmvel 
# de significbncia 0.05, o que implica que a distribuigco
# dos dados nco i significativamente diferente da distrib.
# normal. Em outras palavras, podemos assumir a normalidade.

### Teste de t para uma amostra para verificar se a midia 
# dos pesos dos ratos da amostra i 25g

res <- t.test(data_rats$weight, mu = 25)
res

# H0: O peso dos ratos i 25g
# HA: 0 peso dos ratos i diferente de 25g

# No resultado acima:
# t i o valor estatmstico do teste t (t = -13,788),
# df sco os graus de liberdade (df = 9),
# o p-value i o nmvel de significbncia do teste t (valor de 
# p = 2.34^{-7}). conf.int i o intervalo de confianga da 
# midia a 95% (conf.int = [18,27169, 20,16831]);
# a estimativa i o valor midio da amostra (midia = 19,22).

# Interpretagco do resultado
# O p-value do teste i 2,34^{-7}, que i menor que o nmvel
# de significbncia alfa = 0.05. Pode-se concluir que o peso
# midio dos camundongos i estatisticamente diferente de 25g.


############################################################
# Teste de duas amostras independentes - Unpaired t test  ##


load ("C:/iaa/mw_weight.Rdata")

# Teste preliminar para verificar as suposigues do 
# teste t independente

# Premissa 1: as duas amostras sco independentes?
# Sim, pois as amostras de homens e mulheres nco estco 
# relacionadas.
# Premissa 2: os dados de cada um dos 2 grupos seguem uma
#             distribuigco normal?
# Use o teste de normalidade Shapiro-Wilk 
# - Hipstese nula: os dados sco normalmente distribumdos 
# - Hipstese alternativa: os dados nco estco normalmente
#                         distribumdos

# Usaremos shapiro.test() para calcular o teste Shapiro-Wilk
# para cada grupo de amostras.

# Teste de normalidade Shapiro-Wilk para os pesos masculinos

with(mw_weight, shapiro.test(weight[group == "Man"]))     
# p-value>0.05 (0.1066), logo a amostra possui distribuigco
# normal

# Teste de normalidade Shapiro-Wilk para os pesos femininos

with(mw_weight, shapiro.test(weight[group == "Woman"]))     
# p-value>0.05 (0.6101), logo a amostra possui distribuigco
# normal

# Pelos resultados dos testes, os dois valores de p sco 
# maiores do que o nmvel de significbncia 0.05, o que implica
# que a distribuigco dos dados nco i significativamente 
# diferente da distribuigco normal. Em outras palavras, 
# podemos assumir que as amostras tem distribuigco normal.

# Observe que se os dados nco forem distribumdos normalmente,
# i recomendavel usar outro teste de duas amostras nco 
# paramitrico.

# Premissa 3. As duas populagues tjm as mesmas varibncias?
# Usaremos o teste F para testar a homogeneidade nas 
# varibncias. Isso pode ser executado com a fungco var.test()
# da seguinte maneira:

res.ftest <- var.test(weight ~ group, data = mw_weight)
res.ftest

# H0: As varibncias sco iguais 
# HA: As varibncias nco sco iguais

# O p-value do teste F i p = 0.1714. I maior do que o nmvel
# de significbncia alfa = 0.05. Em conclusco, nco ha 
# diferenga significativa entre as varibncias dos dois 
# conjuntos de dados. Portanto, podemos usar o teste t 
# classico que assume a igualdade das duas varibncias.

# Pergunta: Existe alguma diferenga significativa entre os 
# pesos das mulheres e dos homens?

res <- t.test(weight ~ group, data = mw_weight, 
              var.equal = TRUE)
res

# H0: O peso dos homens nco i diferente estatisticamente do 
#     peso das mulheres
# HA: O peso dos homens i diferente estatisticamente do peso
#     das mulheres

# No resultado:
# t i o valor estatmstico do teste t (t = 2.784),  df sco 
# os graus de liberdade (df = 16),  O p-value i o nmvel de 
# significbncia do teste t (valor de p = 0.01327). conf.int 
# i o intervalo de confianga da midia a 95% 
# (conf.int = [4.0298 , 29.748]);
# A estimativa da amostra i o valor midio da amostra 
# (midia = 68.98889, 52.1).

# O valor de p do teste i 0.01327, que i menor que o nmvel 
# de significbncia alfa = 0,05. Pode-se concluir que o peso
# midio dos homens i significativamente diferente do peso 
# midio das mulheres.


############################################################
######### Teste de t para amostras emparelhadas ############


load ("C:/iaa/paired_weight.Rdata")

# Queremos saber se existe alguma diferenga significativa nos
# pesos midios dos ratos apss o tratamento?

# Estatmsticas descritivas

library("dplyr")

group_by(paired_weight, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

# Teste preliminar para verificar as suposigues do teste t
# pareado

# Premissa 1: as duas amostras estco emparelhadas?
# Sim, uma vez que os dados foram coletados medindo o peso 
# dos mesmos ratos.

# Premissa 2: esta i uma amostra grande?
# Nco, n<30. Como o tamanho da amostra nco i grande o 
# suficiente (menos de 30), precisamos verificar se as
# diferengas dos pares seguem uma distribuigco normal.
# Use o teste de normalidade Shapiro-Wilk

# Hipstese nula: os dados sco normalmente distribumdos
# Hipstese alternativa: os dados nco sco normalmente 
#                       distribu?dos

# calcular a diferenga

d <- with(paired_weight, 
          weight[group == "before"] - weight[group == "after"])
d

# Shapiro-Wilk normality test for the differences

shapiro.test(d)             # => p-value = 0.6141

# no resultado, o p-value i maior do que o nmvel de 
# significbncia 0.05, o que implica que a distribuigco
# das diferengas (d) nco i significativamente diferente 
# da distribuigco normal.
# Em outras palavras, podemos assumir a normalidade.

# Observe que se os dados nco forem distribumdos normalmente,
# i recomendavel usar um teste de duas amostras emparelhadas 
# nco paramitrico.

# Pergunta: Existe alguma mudanga significativa no peso dos
# ratos apss o tratamento?

res <- t.test(weight ~ group, data = paired_weight, 
              paired = TRUE)
res

# H0: O peso dos ratos i estatisticamente igual
# HA: O peso dos ratos i estatisticamente diferente

# No resultado:
# t i o valor estatmstico do teste t (t = 20.883),
# df sco os graus de liberdade (df = 9),
# O p-value i o nmvel de significbncia do teste t
# (p-value = 6.20^{-9}).
# conf.int i o intervalo de confianga das diferengas das 
# midias com 95%, e tambim i mostrado 
# (conf.int = [173,42, 215,56])
# A estimativa da amostra para as diferengas das midias 
# entre pares i (midia = 194,49).

# O p-value do teste i 6.2^{-9}, que i menor que o nmvel 
# de significbncia alfa = 0.05. Podemos entco rejeitar a 
# hipstese nula e concluir que o peso midio dos camundongos
# antes do tratamento i significativamente diferente 
# do peso midio apss o tratamento.


############################################################
############# Teste One-way ANOVA ##########################

# Aqui, usaremos o conjunto de dados integrado ao R nominado
# PlantGrowth. Ele contim o peso das plantas obtidas sob 
# controle e duas condigues de tratamento diferentes.

my_data <- PlantGrowth

# Para ter uma ideia de como sco os dados, usamos a fungco 
# sample_n() [no pacote dplyr]. A fungco sample_n() escolhe
# aleatoriamente algumas das observagues no quadro de dados
# para imprimir:

set.seed(1234)
dplyr::sample_n(my_data, 10)

# Na terminologia R, a coluna "group" i chamada de fator e 
# as diferentes categorias ("ctr", "trt1", "trt2") sco 
# chamadas de nmveis de fator. Os nmveis sco ordenados
# alfabeticamente.

levels(my_data$group)

# Calcule algumas estatmsticas por grupo-contagem, midia e sd:

library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

### Calculo do teste ANOVA unilateral ##

# Queremos saber se existe alguma diferenga significativa 
# entre os pesos midios das plantas nas 3 condigues 
# experimentais.

# A fungco aov() pode ser usada para responder a esta 
# pergunta. A fungco summary.aov() i usada para resumir o 
# modelo de analise de varibncia.

# Calculo da analise da varibncia

res.aov <- aov(weight ~ group, data = my_data)

# Sumario estatmstico da analise

summary(res.aov)

# O resultado inclui as colunas F-value e Pr(>F) 
# correspondentes ao p-value do teste. Como o p-value i 
# menor que o nmvel de significbncia 0.05, pode-se 
# concluir que existem diferengas significativas entre os
# grupos, isso i destacado com "*" no sumario do modelo.

### Comparagco mzltipla de pares entre as midias dos grupos
# No teste ANOVA unilateral, um valor p significativo indica 
# que algumas das midias do grupo sco diferentes, mas nco 
# sabemos quais pares de grupos sco diferentes. I possmvel 
# realizar mzltiplas comparagues de pares, para determinar 
# se a diferenga midia entre pares especmficos do grupo i 
# estatisticamente significativa.

### Comparagues de pares mzltiplos de Tukey-um teste post-hoc, 
# pode ser usado para mais de 3 grupos
# Como o teste ANOVA i significativo, podemos calcular o 
# teste de Tukey HSD (Tukey Honest Significant Differences,
# fungco R: TukeyHSD()) 
# para realizar mzltiplas comparagues de pares entre as 
# midias dos grupos. A fungco TukeyHD() usa a ANOVA ajustada 
# como argumento.

TukeyHSD(res.aov)

# Parbmetros do teste:
# diff: estatmstica da diferenga entre midias dos dois grupos
# lwr, upr: pontos inferior e superior do intervalo de 
#           confianga 95%
# p adj: valor p apss o ajuste para as comparagues mzltiplas.
# Pode-se ver no resultado, que apenas a diferenga entre 
# trt2 e trt1 i significativa com valor p ajustado de 0.012,
# ou seja, trt2 e trt1 sco diferentes.

### Mzltiplas comparagues usando pacote multcomp
# I possmvel usar a fungco glht() [do pacote multcomp] para 
# realizar varios procedimentos de comparagco para uma ANOVA.
# "glht" significa testes de hipsteses lineares gerais. 
# Use glht() para realizar varias comparagues de pares para 
# uma ANOVA unilateral:

library(multcomp)
summary(glht(res.aov, linfct = mcp(group = "Tukey")))

# Novamente foi significativa a diferenga entre trt2 e trt1
# com p-value = 0.0121


### Teste t pareado
# A fungco pairwise.t.test() tambim pode ser usada para 
# calcular comparagues de pares entre nmveis de grupo com
# corregues para testes mzltiplos.

pairwise.t.test(my_data$weight, my_data$group,
                p.adjust.method = "BH")

# O resultado i uma tabela de p-values para as comparagues 
# entre pares. Aqui, os p-values foram ajustados pelo mitodo
# de Benjamini-Hochberg. Novamente o teste mostra que existe
# diferenga significativa entre trt2 e trt1, as demais nco.

# Verifique as suposigues da ANOVA: testar a validade!!
# O teste ANOVA assume que os dados sco normalmente 
# distribumdos e a variagco entre os grupos i homogjnea. 
# Podemos verificar isso com alguns graficos de diagnsstico.
# Tambim i possmvel usar o teste de Bartlett ou teste de 
# Levene para verificar a homogeneidade das varibncias.
# Recomenda-se o teste de Levene, que i menos sensmvel a 
# desvios da distribuigco normal. A fungco leveneTest() 
# [no pacote do car] sera usada:

library(car)
leveneTest(weight ~ group, data = my_data)

# No resultado, pode-se ver que o p-value nco i menor que
# o nmvel de significbncia de 0.05. Isso significa que nco
# existe evidjncias de que a varibncia entre os grupos seja
# diferente. Portanto, podemos supor a homogeneidade das 
# varibncias nos diferentes grupos de tratamento.

### Relaxando a suposigco da homogeneidade da varibncia
# O teste classico da ANOVA de um fator classico requer uma
# suposigco de varibncias simlares para todos os grupos. 
# Em nosso exemplo, a suposigco de homogeneidade da varibncia
# se confirmou: pois o teste de Levene nco i significativo.
# Mas como fazer na situagco em que a suposigco de 
# homogeneidade da varibncia i violada? Usa-se um procedimento
# alternativo que nco exige essa suposigco.

## Teste de t pareados sem suposigco de varibncias iguais

pairwise.t.test(my_data$weight, my_data$group,
                p.adjust.method = "BH", pool.sd = FALSE)

# O teste mostrou os mesmos resultados com p-value<0.05 para 
# trt1 e trt2, ou seja as medianas dos 2 grupos de tratamento
# sco diferentes.

### Proximo passo: Teste Shapiro-Wilk para normalidade 
# dos 3 grupos

# Extramndo os resmduos

aov_residuals <- residuals(object = res.aov )

# Execuntando o teste Shapiro-Wilk

shapiro.test(x = aov_residuals )

# A conclusco do teste de Shapiro-Wilk nos resmduos da ANOVA 
# (W = 0,96, p = 0,43) i de que nco se encontra indmcios de 
# violagco da normalidade. Se a suposigco de normalidade 
# fosse rompida, uma opgco i usar o teste nco-paramitrico
# da soma de ranking de Kruskal-Wallis

#############################################################

############ Testes Repeated-measures ANOVA #################

# I um conjunto de 3 testes:One-way repeated measures ANOVA;
# Two-way repeated measures ANOVA ; e Three-way repeated 
# measures ANOVA. 

### Mesmos indivmduos sco medidos mais de uma vez.

########## One-way repeated measures ANOVA ###################

library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)


# A base de dados

data("selfesteem", package = "datarium")
head(selfesteem, 3)

# Reunir as colunas t1, t2 e t3 em formato longo
# Converter id e tempo em variaveis de fator

selfesteem <- selfesteem %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)
head(selfesteem, 3)

# A ANOVA de medidas repetidas unilaterais pode ser usada
# para determinar se as midias dos escores de autoestima
# sco significativamente diferentes entre os trjs momentos.

# Sumario estatmstico

selfesteem %>%
  group_by(time) %>%
  get_summary_stats(score, type = "mean_sd")


# Criar um grafico "box plot" e adicionar pontos 
# correspondentes aos valores individuais:

bxp <- ggboxplot(selfesteem, x = "time", y = "score", 
                 add = "point")
bxp

## Checando as premissas

# Outliers podem ser facilmente identificados usando mitodos 
# de box plot, implementados pela fungco R identify_outliers()
# [pacote rstatix].

selfesteem %>%
  group_by(time) %>%
  identify_outliers(score)

# observando o resultado do teste, nco existem outliers 
# extremos, se houvesse is.extreme seria "TRUE"(se houvesse
# teriamos de deletar). Observe que, na sitgco em que vocj tem
# outliers extremos, isso pode ser devido a: erros de entrada 
# de dados; erros de medigco; ou valores incomuns. Vocj pode 
# incluir o outlier na analise de qualquer maneira se nco 
# acreditar que o resultado sera substancialmente afetado. 
# Isso pode ser avaliado comparando o resultado da ANOVA com  
# e sem o outlier.Tambim i possivel manter os outliers nos 
# dados e realizar um teste ANOVA robusto usando o pacote 
# WRS2.

## A premissa da normalidade

selfesteem %>%
  group_by(time) %>%
  shapiro_test(score)

# O escore de autoestima apresentou distribuigco normal em 
# cada tempo, conforme avaliado pelo teste de Shapiro-Wilk
# (p> 0,05).

# Observe que, se o tamanho da sua amostra for maior que 50, 
# o grafico QQ normal i preferido porque em tamanhos de 
# amostra maiores, o teste de Shapiro-Wilk se torna muito 
# sensmvel ati mesmo a um pequeno desvio da normalidade.

# O grafico QQ desenha a correlagco entre uma variavel e a 
# distribuigco normal.
# Crie graficos QQ para cada ponto de tempo:

ggqqplot(selfesteem, "score", facet.by = "time")

# No grafico, como todos os pontos caem aproximadamente ao
# longo da linha de referjncia, podemos assumir que a 
# variavel tem distribuigco normal.

### Suposigco de esfericidade ou homogeneidade da amostra

# A suposigco de esfericidade i verificada automaticamente
# durante o calculo do teste ANOVA usando a fungco 
# anova_test() [pacote rstatix]. O teste de Mauchly i usado
# internamente para avaliar a suposigco de esfericidade.
# Usando a fungco get_anova_table() [pacote rstatix] para 
# extrair a tabela ANOVA, a corregco de esfericidade de 
# Greenhouse-Geisser i aplicada automaticamente aos
# fatores que violam a suposigco de esfericidade.

res.aov <- anova_test(data = selfesteem, dv = score, 
                      wid = id, within = time)
res.aov
get_anova_table(res.aov)

# O escore de autoestima foi estatisticamente significativo 
# para diferenga nos diferentes momentos durante a dieta, 
# F (2, 18) = 55,469, p<0,0001, eta2 [g] = 0,829. A 
# estatmstica F Indica que estamos comparando a uma 
# distribuigco F (teste F); (2, 18) indica os graus de 
# liberdade no numerador (DFn) e no denominador (DFd), 
# respectivamente; 55,469 indica o valor obtido da 
# estatmstica F, p especifica o p-value, "ges" i o tamanho 
# do efeito generalizado 82.9% (quantidade de variabilidade 
# devido ao fator within-subjects)

## Testes Post-hoc
# Vocj pode realizar varios testes t pareados por pares 
# entre os nmveis dos fatores within-subjects (time). Os 
# p-values sco ajustados usando o mitodo de corregco
# do teste mzltiplo de Bonferroni.

# Comparagues por pares

pwc <- selfesteem %>%
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

# Todas as diferengas entre pares sco estatisticamente 
# significativas.

# Resultados
# Pode-se analisar o resultado da seguinte maneira:
# O escore de autoestima foi diferente e estatisticamente 
# significativo nos diferentes tempos. As analises post-hoc 
# com ajuste de Bonferroni revelaram que todas as diferengas
# aos pares, entre os pontos no tempo, foram diferentes e 
# estatisticamente significativas (p < 0.05).

# Visualizagco: box plots com p-values
pwc <- pwc %>% add_xy_position(x = "time")
bxp + stat_pvalue_manual(pwc) +
  labs(subtitle = get_test_label(res.aov,
                                 detailed = TRUE),
       caption = get_pwc_label(pwc)
  )

########### Two-way repeated measures ANOVA ###############

## Preparando a base de dados

set.seed(123)
data("selfesteem2", package = "datarium")
selfesteem2 %>% sample_n_by(treatment, size = 1)

# Rezna as colunas t1, t2 e t3 no formato longo.
# Converta id e tempo em variaveis de fator

selfesteem2 <- selfesteem2 %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)

# Inspecione algumas linhas aleatsrias dos dados por grupos

set.seed(123)
selfesteem2 %>% sample_n_by(treatment, time, size = 1)


# Neste exemplo, o efeito do "tempo" na pontuagco da 
# autoestima i nossa variavel focal, essa i nossa principal
# preocupagco. Porim, pensa-se que o efeito do "tempo" sera
# diferente se o tratamento for realizado ou nco. Nesse 
# cenario, a variavel "tratamento" i considerada como 
# variavel moderadora.

# Estatmsticas de resumo
# Agrupe os dados por tratamento e tempo e, em seguida, 
# calcule algumas estatmsticas resumidas da variavel de 
# pontuagco: midia e desvio padrco.

selfesteem2 %>%
  group_by(treatment, time) %>%
  get_summary_stats(score, type = "mean_sd")


## Visualizagco
# Crie box plots coloridos do score por grupos de 
# tratamento:

bxp <- ggboxplot(
  selfesteem2, x = "time", y = "score",
  color = "treatment", palette = "jco"
)
bxp


### Checagem de premissas
# Outliers

selfesteem2 %>%
  group_by(treatment, time) %>%
  identify_outliers(score)


# Nco existem outliers extremos.

### Suposigco de normalidade
# Calculo do Teste de Shapiro-Wilk para cada combinagco de
# nmveis de fator:

selfesteem2 %>%
  group_by(treatment, time) %>%
  shapiro_test(score)

# O escore de autoestima apresentou distribuigco normal em 
# cada momento (p> 0.05), exceto para o fator ctr em t1.

# Criar o grafico QQ para cada grupo:

ggqqplot(selfesteem2, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ treatment, labeller = "label_both")

# No grafico, como todos os pontos caem aproximadamente ao 
# longo da linha de referjncia, podemos assumir normalidade.

### Calculo do teste Two-way repeated measures ANOVA 

res.aov <- anova_test(
  data = selfesteem2, dv = score, wid = id,
  within = c(treatment, time)
)
res.aov

get_anova_table(res.aov)

# Existe uma interagco bidirecional estatisticamente 
# significativa entre o tratamento e o tempo, 
# F (2, 22) = 30,424, p < 0,05.


### Testes Post-hoc
# Uma interagco bidirecional significativa indica que o 
# impacto que um fator (por exemplo, tratamento) tem sobre
# a variavel de resultado (no exemplo, pontuagco de 
# autoestima) depende do nmvel do outro fator (por exemplo,
# tempo) (e vice-versa). Portanto, vocj pode decompor uma 
# interagco bidirecional significativa em:

# Efeito principal simples: execute o modelo unilateral da 
# primeira variavel (fator A) em cada nmvel da segunda 
# variavel (fator B),

# Comparagues de pares simples: se o efeito principal 
# simples for significativo, execute varias comparagues de
# pares para determinar quais grupos sco diferentes.

# Para uma interagco bidirecional nco significativa, vocj
# precisa determinar se existe algum efeito principal 
# estatisticamente significativo no resultado da ANOVA.

### Procedimento para uma interagco bidirecional significativa 
# Efeito do tratamento. Em nosso exemplo, analisaremos o
# efeito do tratamento na pontuagco da autoestima em cada 
# momento.

# Observe que a variavel do fator de tratamento possui 
# apenas dois nmveis ("ctr" e "Diet"); assim, o teste ANOVA 
# e o teste t pareado fornecerco os mesmos p-values.

# Efeito do tratamento em cada ponto no tempo

one.way <- selfesteem2 %>%
  group_by(time) %>%
  anova_test(dv = score, wid = id, within = treatment) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Resultado: O efeito do tratamento i significativo nos 
# tempos t2 e t3 Considerando o p-value ajustado de 
# Bonferroni (p.adj), verifica-se que o simples efeito 
# principal do tratamento nco foi significativo no momento 
# t1 (p = 1). Torna-se significativo em t2 (p = 0,036) e 
# t3 (p = 0,00051).

# Comparagues de pares entre grupos de tratamento

pwc <- selfesteem2 %>%
  group_by(time) %>%
  pairwise_t_test(
    score ~ treatment, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

# As comparagues pareadas mostram que a pontuagco midia de 
# autoestima foi significativamente diferente entre ctr e 
# o grupo Dieta em t2 (p = 0,12) e t3 (p = 0,00017), mas 
# nco em t1 (p = 0,552).

# Efeito do tempo. 
# Observe que tambim i possmvel realizar a mesma analise
# para a variavel tempo em cada nmvel de tratamento. Vocj 
# nco precisa necessariamente fazer essa analise.

# Efeito do tempo em cada nmvel de tratamento

one.way2 <- selfesteem2 %>%
  group_by(treatment) %>%
  anova_test(dv = score, wid = id, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2

# vocj pode ver que o efeito do tempo i significativo apenas
# para o controle, F (2, 22) = 39,7, p <0,05. 

# Comparagues pareadas entre pontos no tempo

pwc2 <- selfesteem2 %>%
  group_by(treatment) %>%
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2

# As comparagues entre pares mostram que todas as 
# comparagues entre os pontos de tempo foram 
# estatisticamente significativas para o grupo de controle,
# mas nco para o grupo de tratamento (dieta).

### Procedimento para interagco bidirecional nco significat.
# Se a interagco nco for significativa, vocj precisa 
# interpretar os efeitos principais de cada uma das duas 
# variaveis: tratamento e tempo. Um efeito principal 
# significativo pode ser conseguido com comparagues de pares.

# Comparagues de teste t pareado:

# comparagues para a variavel tratamento

selfesteem2 %>%
  pairwise_t_test(
    score ~ treatment, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

# comparagues para a variavel tempo

selfesteem2 %>%
  pairwise_t_test(
    score ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

# Todas as comparagues pareadas sco significantes.

## Relatsrio de resultados
# Podemos relatar o resultado da seguinte maneira:
# Uma ANOVA de medidas repetidas de duas vias foi realizada
# para avaliar o efeito de diferentes tratamentos de dieta 
# ao longo do tempo no escore de autoestima.

# Houve uma interagco estatisticamente significativa entre
# o tratamento e o tempo no escore de autoestima, 
# F (2, 22) = 30,424, p<0,05. 
# Com isso, o efeito da variavel de tratamento foi analisado
# em cada momento. Os p-values foram ajustados usando o 
# mitodo de corregco pelo teste mzltiplo de Bonferroni. 
# O efeito do tratamento foi significativo em t2 (p = 0,036)
# e t3 (p = 0,00051), mas nco no momento t1 (p = 1).

# As comparagues pareadas, usando o teste t pareado, mostram
# que a pontuagco midia da autoestima foi significativamente
# diferente entre ctr e dieta nos momentos t2 (p = 0,012) e 
# t3 (p = 0,00017), mas nco em t1 (p = 0.55).

# Visualizagco: box plots com p-values
pwc <- pwc %>% add_xy_position(x = "time")
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


######### Three-way repeated measures ANOVA ################

## Preparagco dos dados

# Usaremos o conjunto de dados de emagrecimento 
# [pacote datarium]. Neste estudo, um pesquisador avaliou
# os efeitos da dieta e dos exercmcios na perda de peso em
# 10 indivmduos sedentarios. Os participantes foram inclumdos
# em quatro ensaios: (1) sem dieta e sem exercmcios; 
# (2) dieta apenas; (3) exercmcios apenas; e (4) dieta e
# exercmcios combinados.
# Cada participante realizou todas as quatro tentativas. A 
# ordem dos testes foi contrabalangada e foi concedido tempo
# suficiente entre os testes para permitir que quaisquer 
# efeitos dos testes anteriores se dissipassem.
# Cada tentativa durou nove semanas e a pontuagco da perda
# de peso foi medida no inmcio (t1), no ponto midio (t2) e 
# no final (t3) de cada tentativa.
# A ANOVA de trjs medidas repetidas pode ser realizada para
# determinar se existe interagco significativa entre dieta,
# exercmcios e tempo, no escore de perda de peso.

# Preparando os dados

library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)


set.seed(123)
data("weightloss", package = "datarium")
weightloss %>% sample_n_by(diet, exercises, size = 1)

# Rezna as colunas t1, t2 e t3 no formato longo.
# Converta id e tempo em variaveis de fator

weightloss <- weightloss %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)

# Inspecione algumas linhas dos dados por grupos 
# aleatoriamente

set.seed(123)
weightloss %>% sample_n_by(diet, exercises, time, size = 1)


# Neste exemplo, o efeito "tempo" i nossa variavel focal, 
# essa i nossa principal preocupagco.
# Pensa-se que o efeito "tempo" no escore de perda de peso 
# dependera de dois outros fatores, "dieta" e "exercmcios", 
# chamados de variaveis moderadoras.

### Estatmsticas de resumo

# Agrupe os dados por dieta, exercmcios e tempo e em seguida
# calcule algumas estatmsticas de resumo da variavel de 
# pontuagco: midia e sd (desvio padrco)

weightloss %>%
  group_by(diet, exercises, time) %>%
  get_summary_stats(score, type = "mean_sd")

### Visualizagco

# Criar box plots:

bxp <- ggboxplot(
  weightloss, x = "exercises", y = "score",
  color = "time", palette = "jco",
  facet.by = "diet", short.panel.labs = FALSE
)
bxp

# Checando as premissas

# Outliers

weightloss %>%
  group_by(diet, exercises, time) %>%
  identify_outliers(score)

# Nco existem outliers extremos.


### Suposigco de normalidade

# Calculo do teste de Shapiro-Wilk para cada combinagco 
# de nmveis dos fatores:

weightloss %>%
  group_by(diet, exercises, time) %>%
  shapiro_test(score)

# O escore de perda de peso i distribumdo normalmente, 
# conforme avaliado pelo teste de normalidade de 
# Shapiro-Wilk (p> 0,05).

# Criar QQ plot para cada grupo:

ggqqplot(weightloss, "score", ggtheme = theme_bw()) +
  facet_grid(diet+exercises~time, labeller="label_both")

# No grafico, como todos os pontos caem aproximadamente ao
# longo da linha de referjncia, podemos assumir normalidade.


#### Calculo do teste para homogeneidade da amostra

res.aov <- anova_test(
  data = weightloss, dv = score, wid = id,
  within = c(diet, exercises, time)
)
res.aov
get_anova_table(res.aov)

# A partir do resultado, pode-se ver que existem interagues
# tripartidas estatisticamente significativas entre dieta,
# exercmcios e tempo,F (2, 22) = 14,246, p<0,05.
# Observe que, se a interagco de trjs vias nco for 
# estatisticamente significativa, vocj precisa consultar o
# resultado das interagues de duas vias.

# Em nosso exemplo, existe iteragco bidirecional da dieta 
# estatisticamente significativa: interagco com exercmcios
# (p <0,0001); 
# Alim disso, existe iteragco bidirecional de exercmcios:
# iteragco com tempo (p <0,0001). 
# E interagco de duas vias da dieta: iteragco com tempo "nco"
# foi estatisticamente significativa (p = 0.5).

#### Testes Post-hoc

# Se houver efeito significativo de interagco de trjs vias 
# vocj pode decompor em:

# 1) Interagco bidirecional simples: execute a interagco 
#    bidirecional em cada nmvel da terceira variavel,
# 2) Efeito principal simples: execute o modelo unilateral
#    em cada nmvel da segunda variavel, e
# 3) Comparagues pareadas simples: execute comparagues 
#    pareadas ou outras comparagues post-hoc, se necessario.

### Calculo da interagco bidirecional simples

# Vocj i livre para decidir quais duas variaveis formarco 
# as interagues bidirecionais simples e qual variavel atuara
# como a terceira variavel (moderadora). No csdigo R a 
# seguir, consideramos a interagco simples de duas vias de 
# exercmcios * tempo em cada nmvel da dieta.

# Agrupe os dados por dieta e analise a interagco simples 
# de duas vias entre exercmcios e tempo:

# Two-way ANOVA em cada nmvel de dieta

two.way <- weightloss %>%
  group_by(diet) %>%
  anova_test(dv = score, wid = id, 
             within = c(exercises, time))

# Extrair Tabela anova

get_anova_table(two.way)

# Houve uma interagco bidirecional simples estatisticamente
# significativa entre exercmcios e tempo para o ensaio 
# "dieta nco", F (2, 22) = 28,9, p <0,0001, mas nco entre 
# exercmcios e tempo para o ensaio "dieta sim",
# F (2, 22) = 2,57, p = 0,099.

# I recomendado ajustar o p-value. Uma abordagem comum
# i aplicar um ajuste de Bonferroni para ajustar o 
# nmvel no qual vocj declara a significbncia estatmstica.

# Isso pode ser feito dividindo o nmvel atual em que vocj
# declara a significbncia estatmstica (ou seja, p <0,05) 
# pelo nzmero de interagues bidirecionais simples que vocj
# esta computando (ou seja, 2).

# Assim, vocj ss declara uma interagco de duas vias como
# estatisticamente significativa quando p <0,025 
# (ou seja, p <0,05 / 2). Aplicando isso ao nosso exemplo
# atual, ainda tirarmamos as mesmas conclusues.

### Calcular efeito principal simples simples

# Uma interagco bidirecional simples estatisticamente 
# significativa pode ser seguida de efeitos principais
# simples.

# Em nosso exemplo, vocj poderia, portanto, investigar o
# efeito do tempo na pontuagco de perda de peso em todos
# os nmveis de exercmcios ou investigar o efeito dos 
# exercmcios em todos os nmveis de tempo.

# Vocj ss precisara considerar o resultado das analises
# simples do efeito principal para o estudo "dieta nco",
# pois essa foi a znica interagco simples de duas vias
# que foi estatisticamente significativa.

# Agrupe os dados por dieta e exercmcios e analise o efeito
# principal simples do tempo. O ajuste de Bonferroni deve  
# ser considerado e a significbncia estatmstica deve ser
# aceita no nmvel de p <0,025 (ou seja, 0,05 dividido pelo
# nzmero de testes (aqui 2) considerados para "dieta: nco".

# Efeito do tempo em cada grupo dieta X exercmcios

time.effect <- weightloss %>%
  group_by(diet, exercises) %>%
  anova_test(dv = score, wid = id, within = time)

# Extraindo a tabela anova

get_anova_table(time.effect) %>%
  filter(diet == "no")

# Houve um efeito principal simples e estatisticamente 
# significativo do tempo no escore de perda de peso para o
# grupo "dieta: nco, exercmcios: sim" (p <0,0001), mas nco
# para quando nem dieta nem exercmcios foram realizados 
# (p = 0,286).

### Calcule comparagues simples

# Um efeito principal simples estatisticamente significativo
# pode ser seguido por mzltiplas comparagues de pares para
# determinar quais midias de grupo sco diferentes.

# Agrupe os dados por dieta e exercmcios e faga comparagues
# aos pares entre os pontos no tempo com o ajuste de 
# Bonferroni:

# Comparagues por pares

pwc <- weightloss %>%
  group_by(diet, exercises) %>%
  pairwise_t_test(score ~ time, paired = TRUE, 
                  p.adjust.method = "bonferroni") %>%
  select(-df, -statistic) # Remove alguns detalhes

# Mostrar resultados de comparagco para grupos "dieta: nco,
# exercmcios: sim"

pwc %>% filter(diet == "no", exercises == "yes") %>%
  select(-p)     # remove coluna p, ss interessa p. adj.


# Na tabela de comparagues de pares, estamos interessados 
# apenas nas comparagues simples para grupos "dieta: nco,
# exercmcios: sim". Em nosso exemplo, existem trjs 
# combinagues possmveis de diferengas de grupo. Podermamos
# relatar os resultados da comparagco entre pares como segue.

# Todas as comparagues pareadas simples foram feitas entre
# os diferentes pontos de tempo para o ensaio "dieta: nco,
# exercmcios: sim". O ajuste de Bonferroni foi aplicado. 
# A pontuagco midia de perda de peso foi significativamente
# diferente em todas as comparagues nos pontos de tempo
# quando os exercmcios sco realizados (p <0,05).

#### Relatsrio do teste

# Uma ANOVA de trjs medidas repetidas foi realizada para 
# avaliar os efeitos da dieta, exercmcios e tempo na perda
# de peso. Houve uma interagco de trjs vias estatisticamente
# significativa entre dieta, exercmcios e tempo, 
# F (2, 22) = 14,2, p = 0,00011.

# Para as interagues bidirecionais simples e analises de 
# efeitos principais simples, um ajuste de Bonferroni foi
# aplicado levando a significbncia estatmstica aceita no
# nmvel de p<0,025.

# Houve uma interagco bidirecional simples estatisticamente
# significativa entre exercmcios e tempo para o ensaio 
# "dieta nco", F (2, 22) = 28,9, p <0,0001, mas nco para o 
# ensaio "dieta sim" ", F (2, 22) = 2,6, p = 0,099.

# Houve um efeito principal simples e estatisticamente 
# significativo do tempo no escore de perda de peso para o
# ensaio "dieta: nco, exercmcios: sim" (p <0,0001),
# mas nco para quando nem dieta nem exercmcios foram 
# realizados (p = 0,286).

# Todas as comparagues pareadas simples foram feitas entre
# os diferentes pontos de tempo para o ensaio "dieta: nco,
# exercmcios: sim" com um ajuste de Bonferroni aplicado. 
# A pontuagco midia de perda de peso foi significativamente
# diferente em todas as comparagues nos pontos de tempo 
# quando os exercmcios sco realizados (p <0,05).

# Visualizagco: box plots com p-values

pwc <- pwc %>% add_xy_position(x = "exercises")
pwc.filtered <- pwc %>% 
  filter(diet == "no", exercises == "yes")
bxp + 
  stat_pvalue_manual(pwc.filtered, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


################### Fim da Aula 1 ###################################
