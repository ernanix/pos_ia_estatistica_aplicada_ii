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

load("C:/Users/escneto/Documents/Estudos/Pos_IA_UFPR/Estatistica_Aplicada_II/Arquivos_para_R/wage.RData")

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

###### Regressão Ridge  ###########
## Regressão ridge reduz os coeficientes

cols_reg = c('husage', 'husearns', 'huseduc', 'hushrs', 
             'earns', 'age', 'educ', 'hrwage','husblck',
             'hushisp', 'kidge6', 'black', 'hispanic',
             'union', 'kidlt6')

dummies <- dummyVars(hrwage ~ husage+husearns+huseduc+hushrs+ 
                     earns+age+educ+husblck+hushisp+kidge6+
                     black+hispanic+union+kidlt6, 
                     data = wage[,cols_reg])

train_dummies = predict(dummies, newdata = train[,cols_reg])

test_dummies = predict(dummies, newdata = test[,cols_reg])

print(dim(train_dummies)); print(dim(test_dummies))

# A regressão Ridge é uma extensão da regressão linear em 
# que a função de perda é modificada para minimizar a 
# complexidade do modelo. Essa modificação é feita 
# adicionando um parâmetro de penalidade equivalente ao 
# quadrado da magnitude dos coeficientes.

# Uma das principais diferenças entre os modelos de regressão
# linear e regularizada é que o último envolve o ajuste de
# um hiperparâmetro, lambda. O código executa o modelo 
# glmnet() várias vezes para diferentes valores de lambda. 
# Podemos automatizar essa tarefa de encontrar o valor lambda
# ideal usando a função cv.glmnet(). Isso é feito usando as
# linhas de código abaixo.

# A função perda é dada por:
# Loss function = OLS+lambda*summation(squared coefficient
# values)
# Lambda é o parâmetro de penalidade que selecionamos

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

# Obtendo os valores dos parâmetros

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

# Como os valores de entrada no modelo estão normalizados,
# temos de normalizar também os dados que queremos prever
# Observe que as variáveis dummies não sofrem normalização

#Para:
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

# Construindo uma matriz com os dados para predi??o

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
# Fazendo a predi??o

predict_our_ridge <- predict(ridge_reg, s = best_lambda_ridge, 
                     newx = our_pred)
predict_our_ridge

# O resultado ? uma informa??o normalizada, vamos 
# convert?-la em valor nominal, compat?vel com a base de 
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

CIlwr_ridge
CIupr_ridge

##### Regress?o Lasso ####
## Leva a zero os coeficientes n?o significativos

# A regress?o Lasso, ou o Operador de Encolhimento Absoluto
# M?nimo e Sele??o, tamb?m ? uma modifica??o da regress?o
# linear. No lasso, a fun??o de perda ? modificada para
# minimizar a complexidade do modelo, limitando a soma dos
# valores absolutos dos coeficientes do modelo 
# (tamb?m chamado de l1-norm).
# O uso de uma restri??o l1-norm for?a alguns valores de
# peso a zero para permitir que outros coeficientes assumam
# valores diferentes de zero.

# A fun??o perda ? dada por:
# Loss function = OLS+lambda*summation(absolute values of
# the magnitude of the coefficients)

# Escolhendo  melhor lambda como par?metro

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

# Visualizando os par?metros calculados

lasso_model[["beta"]]

# Fazendo as predi??es a avaliando o modelo lasso nas bases 
# de treino e teste

predictions_train <- predict(lasso_model, s = best_lambda_lasso,
                             newx = x)
eval_results(y_train, predictions_train, train)

predictions_test <- predict(lasso_model, s = best_lambda_lasso, 
                            newx = x_test)
eval_results(y_test, predictions_test, test)

## Prediction for our example

# Fazendo a predi??o baseada nos mesmos par?metros da 
# regress?o ridge

predict_our_lasso <- predict(lasso_model, s = best_lambda_lasso, 
                          newx = our_pred)
predict_our_lasso

# Novamente, a informa??o que retorna ? normalizada, temos
# de convert?-la em valor compat?vel com a base de dados 
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

########### Regress?o ElasticNet ###########################

#A regress?o ElasticNet combina as propriedades de regress?o
# Ridge e lasso. Ele funciona penalizando o modelo usando 
# tanto a l2-norm quanto a l1-norm. O modelo pode ser 
# facilmente constru?do usando o pacote caret, que seleciona
# automaticamente o valor ideal dos par?metros.

# Set training control
train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)

# Train the model

# Note que aqui n?o temos o par?metro alpha, porque a
# regress?o elasticnet vai selecion?-lo automaticamente com
# valor entre 0 e 1; Para a regress?o ridge alpha=0 e 
# lasso alpha=1
# Na elasticnet o par?metro lambda tamb?m ? encontrado por
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

# Fazendo as predi??es nas bases de treino e teste e
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

# Novamente, a informa??o que retorna ? normalizada, temos
# de convert?-la em valor compat?vel com a base de dados 
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

################# Testes não Paramétricos ###################

###### One-Sample Wilcoxon Signed Rank Test #################

# Preparando os dados

# Usaremos um conjunto de dados de exemplo contendo o peso
# de 10 ratos. Queremos saber se o peso m?dio dos camundongos
# difere de 25g?

load("C:/iaa/data_rats.Rdata")

# Sum?rio estat?stico do peso

summary(data_rats$weight)


# Visualize seus dados usando box plots

library(ggpubr)
ggboxplot(data_rats$weight, 
          ylab = "Weight (g)", xlab = FALSE,
          ggtheme = theme_minimal())

mean(data_rats$weight)

# Calcular teste de Wilcoxon de uma amostra

# Queremos saber se o peso m?dio dos ratos difere de 25g
# (teste bicaudal)

# One-sample wilcoxon test
res <- wilcox.test(data_rats$weight, mu = 25)
res 

# H0: peso dos ratos ? igual estatisticamente a 25g
# HA: peso dos ratos ? estisticamente diferente de 25g

# O p-value do teste ? 0,001953, que ? menor que o n?vel de 
# signific?ncia alfa = 0.05. Podemos rejeitar a hip?tese 
# nula e concluir que o peso m?dio dos camundongos ? 
# significativamente diferente de 25g com um p-value de
# p = 0,001953.

# se voc? deseja testar se o peso m?dio dos camundongos ? 
# inferior a 25g (teste unicaudal), use a seguinte fun??o:

wilcox.test(data_rats$weight, mu = 25,
            alternative = "less")

# H0: o peso m?dio dos ratos ? maior que 25g
# HA: o peso m?dio dos ratos ? menor que 25g

# data:  data_rats$weight
# V = 0, p-value = 0.0009766
# alternative hypothesis: true location is less than 25

# Como p-value < 0.05, rejeita-se H0, o peso m?dio dos ratos
# ? menor que 25g

###### se voc? quiser testar se o peso m?dio dos camundongos 
# ? maior que 25g (teste unicaudal), use a seguinte fun??o:

wilcox.test(data_rats$weight, mu = 25,
            alternative = "greater")

# H0: o peso m?dio dos ratos ? menor que 25g
# HA: o peso m?dio dos ratos ? maior que 25g

# data:  data_rats$weight
# V = 0, p-value = 1
# alternative hypothesis: true location is greater than 25

# Como p-value > 0.05 aceita-se H0, o peso m?dio dos ratos
# ? menor que 25g



###### Unpaired Two-Samples Wilcoxon Test para ############# 
#### independ?ncia de grupos/amostras desemparelhadas ###### 
###############(ou n?o emparelhadas)########################


# Preparando os dados

# Dados em dois vetores num?ricos

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

# Sum?rio estat?stico

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

# Teste se o peso mediano dos homens ? igual ao peso mediano
# das mulheres
# O teste ? sempre feito com rela??o ao disposto no vetor de
# teste, no caso do ?ltimo para o primeiro - No vetor: 
# Man contra Woman

res <- wilcox.test(weight ~ group, data = weight,
                   exact = FALSE)
res

# O p-value do teste ? 0,02712, que ? menor que o n?vel de
# signific?ncia alfa = 0,05. Podemos concluir que o peso 
# mediano dos homens ? significativamente diferente do peso
# mediano das mulheres.

# Observe que:
# se voc? quiser testar se o peso mediano dos homens ? menor
# que o peso mediano das mulheres, use a seguinte fun??o:

wilcox.test(weight ~ group, data = weight, 
            exact = FALSE, alternative = "less")

# Como p-value > 0.05 o peso mediano dos homens n?o ? menor
# que o peso mediano das mulheres

# se voc? quiser testar se o peso mediano dos homens ? maior
# que o peso mediano das mulheres, use a seguinte fun??o:

wilcox.test(weight ~ group, data = weight,
            exact = FALSE, alternative = "greater")

# como p-value < 0.05 o peso mediano dos homens ? maior que
# o peso mediano das mulheres

# Ent?o conclui-se que o peso mediano dos homens ? maior que
# o peso mediano das mulheres


############################################################
########Teste de Wilcoxon para amostras pareadas ###########

# Carregando os dados

# Usaremos um conjunto de dados de exemplo, que cont?m o
# peso de 10 ratos antes e depois do tratamento.

load ("C:/iaa/paired_weight.Rdata")

# Queremos saber se existe alguma diferen?a significativa
# nos pesos medianos antes e depois do tratamento?

# Calcule estat?sticas resumidas por grupos:

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

# O p-value do teste ? 0.001953, que ? menor que o n?vel
# de signific?ncia alfa = 0,05. Podemos concluir que o 
# peso m?dio dos camundongos antes do tratamento ? 
# significativamente diferente do peso m?dio ap?s o 
# tratamento

# se voc? quiser testar se o peso mediano antes do 
# tratamento ? menor do que o peso mediano ap?s o 
# tratamento, use a seguinte fun??o:

wilcox.test(weight ~ group, data = paired_weight, 
            paired = TRUE,alternative = "less")

# Como o p-value > 0.05 conclui-se que o peso mediano antes
# do tratamento ? menor que o peso mediano ap?s tratamento

# se voc? quiser testar se o peso mediano antes do 
# tratamento ? maior do que o peso mediano ap?s o
# tratamento, use a seguinte fun??o:

wilcox.test(weight ~ group, data = paired_weight, 
            paired = TRUE,alternative = "greater")

# Como o p-value < 0.05 conclui-se que o peso mediano antes
# do tratamento n?o ? maior do que o peso mediano ap?s o
# tratamento.

######## Teste de Kruskal-Wallis para Comparar #############
#### dois ou mais grupos ou amostras independentes #########

# Carregar dados de pesos de plantas em 3 condi??es
# experimentais

my_data <- PlantGrowth

# Na terminologia R, a coluna "group" ? chamada de fator e 
# as diferentes categorias ("ctr", "trt1", "trt2") s?o 
# chamadas de n?veis de fator. Os n?veis s?o ordenados
# alfabeticamente.

# Mostrar os grupos

levels(my_data$group)

# Se os n?veis n?o estiverem automaticamente na ordem 
# correta, reordene-os da seguinte forma:

my_data$group <- ordered(my_data$group,
                         levels = c("ctrl", "trt1", 
                                    "trt2"))

# Calcule estat?sticas resumidas por grupos:

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

# Gr?fico do peso por grupo
# Adicionando barras de erro: mean_se (erro quadrado m?dio)

library("ggpubr")
ggline(my_data, x = "group", y = "weight", 
       add = c("mean_se", "jitter"), 
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")


# C?lculo do teste de Kruskal-Wallis

# Queremos saber se existe alguma diferen?a significativa
# entre os pesos m?dios das plantas nas 3 condi??es 
# experimentais.

kruskal.test(weight ~ group, data = my_data)

# Como o valor de p (0.01842) ? inferior ao n?vel de 
# signific?ncia 0,05, podemos concluir que existem 
# diferen?as significativas entre os grupos de tratamento.

### Compara??o m?ltipla de pares entre grupos

# A partir do resultado do teste de Kruskal-Wallis, sabemos
# que h? uma diferen?a significativa entre os grupos, mas 
# n?o sabemos quais pares de grupos s?o diferentes.

# ? poss?vel usar a fun??o pairwise.wilcox.test () para 
# calcular compara??es de pares entre os n?veis do grupo
# com corre??es para testes m?ltiplos.

pairwise.wilcox.test(PlantGrowth$weight, PlantGrowth$group,
                     p.adjust.method = "BH")

# BH ? a t?cnica de ajuste de Benjamini & Hochberg (1995)

# A compara??o entre pares mostra que, apenas trt1 e trt2
# s?o significativamente diferentes pois p < 0.05

# Teste de Dunn com a metodologia de "BY", 
# Benjamini & Yekutieli (2001)

library (dunn.test)
library (PMCMR)
library (PMCMRplus)

dunn.test(my_data$weight, my_data$group, method="by", 
          list=TRUE)

# O teste de Dunn corroborou o teste pela t?cnica "BH", 
# apenas trt1 e trt2 s?o significativamente diferentes pois
# p < 0.05

### Teste de Nemenyi com ajustamento de Tukey

posthoc.kruskal.nemenyi.test(weight ~ group, data = my_data,
                             dist="Tukey")

# O teste apresentou liga??es entre valores e n?o assegura
# a veracidade dos p-values, mas os demais testes s?o 
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

# Re?na as colunas t1, t2 e t3 no formato long. Converta 
# vari?veis id e de tempo em vari?veis de fator 
# (ou agrupamento):

selfesteem <- selfesteem %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)
head(selfesteem, 3)

# Estat?sticas de resumo

selfesteem %>%
  group_by(time) %>%
  get_summary_stats(score, type = "common")

# Visualiza??o

ggboxplot(selfesteem, x="time", y="score", add="jitter")

# Calculo da estat?stica

# Usaremos a fun??o friedman_test() compat?vel com 
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

# M?ltiplas compara??es de pares
# A partir do resultado do teste de Friedman, sabemos que
# h? uma diferen?a significativa entre os grupos, mas n?o
# sabemos quais pares de grupos s?o diferentes.

# Um teste de Friedman significativo pode ser seguido de
# testes de classifica??o sinalizada de Wilcoxon aos pares
# para identificar quais grupos s?o diferentes.

# Observe que os dados devem ser ordenados corretamente
# pela vari?vel (id) para que a primeira observa??o para o
# tempo t1 seja pareada com a primeira observa??o para o 
# tempo t2 e assim por diante.

#### Compara??es de pares usando teste de postos sinalizados
# de Wilcoxon pareado
# Os p-values s?o ajustados usando o m?todo de corre??o de 
# teste m?ltiplo de Bonferroni.

# Compara??es por pares

pwc <- selfesteem %>%
  wilcox_test(score ~ time, paired = TRUE, 
              p.adjust.method = "bonferroni")
pwc

# Todas as diferen?as entre pares s?o estatisticamente
# significativas Todos tem "estrelas" = p-value < 0.05.

# Observe que tamb?m ? poss?vel realizar compara??es de 
# pares usando o Teste de Sinal, que pode n?o ter for?a 
# para detectar diferen?as em conjuntos de dados 
# emparelhados. No entanto, ? ?til porque tem poucas 
# suposi??es sobre as distribui??es dos dados a serem 
# comparados.

# Compara??es de pares usando teste de sinal:

pwc2 <- selfesteem %>%
  sign_test(score ~ time, p.adjust.method = "bonferroni")
pwc2

## O teste corroborou o teste de wilcoxon com exce??o entre 
# t2 e t3 n?o foi significativo

# Relat?rio final dos testes 

# O escore de autoestima foi estatisticamente significativo 
# nos diferentes momentos usando o teste de Friedman, 
# X2 (2) = 18,2, p = 0,00011.

# O teste de classifica??o sinalizada de Wilcoxon pairwise 
# entre os grupos revelou diferen?as estatisticamente 
# significativas no escore de autoestima entre t1 e t2
# (p = 0,006); t1 e t3 (0,006); t2 e t3 (0,012).

# Visualiza??o: boxplots com p-values

pwc <- pwc %>% add_xy_position(x = "time")
ggboxplot(selfesteem, x="time", y="score", add="point")+
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

############### TESTES PARAM?TRICOS ######################## 

#### Teste uma amostra (one-sample t test) #################


# Usaremos um conjunto de dados de exemplo contendo o peso 
# de 10 ratos.
# Queremos saber se o peso m?dio dos ratos difere de 25g?

# install.packages("ggpubr")

library(ggpubr)



load("D:/Cursos/Pos_IA/Estatistica_aplicada_II/Arquivos_para_R/data_rats.Rdata" )

####Teste preliminar para verificar as suposi??es do teste t
# de uma amostra
# Esta ? uma amostra grande? - N?o, porque n <30.
# Visto que o tamanho da amostra n?o ? grande o suficiente 
# (menor de 30, teorema do limite central),precisamos 
# verificar se os dados seguem uma distribui??o normal.

# Teste de Shapiro-Wilk:
# Hip?tese nula: os dados s?o normalmente distribu?dos
# Hip?tese alternativa: os dados n?o s?o normalmente distrib.

shapiro.test(data_rats$weight) # => p-value = 0.7846

# A partir do resultado, o p-value é maior do que o nível 
# de significância 0.05, o que implica que a distribuição
# dos dados não é significativamente diferente da distrib.
# normal. Em outras palavras, podemos assumir a normalidade.

### Teste de t para uma amostra para verificar se a m?dia 
# dos pesos dos ratos da amostra ? 25g

res <- t.test(data_rats$weight, mu = 25)
res

#t.test(data_rats$weight,y=NULL,"less",mu=25)
#t.test(data_rats$weight,y=NULL,"greater",mu=25)

qt(0.025,9)

# H0: O peso dos ratos é 25g
# HA: 0 peso dos ratos é diferente de 25g

# No resultado acima:
# t ? o valor estat?stico do teste t (t = -13,788),
# df s?o os graus de liberdade (df = 9),
# o p-value ? o n?vel de signific?ncia do teste t (valor de 
# p = 2.34^{-7}). conf.int ? o intervalo de confian?a da 
# m?dia a 95% (conf.int = [18,27169, 20,16831]);
# a estimativa ? o valor m?dio da amostra (m?dia = 19,22).

# Interpreta??o do resultado
# O p-value do teste ? 2,34^{-7}, que ? menor que o n?vel
# de signific?ncia alfa = 0.05. Pode-se concluir que o peso
# m?dio dos camundongos ? estatisticamente diferente de 25g.


############################################################
# Teste de duas amostras independentes - Unpaired t test  ##
# Amostras não pareadas - Não tem relação entre elas
# pareadas: ex. peso de pessoas antes e depois de um treino

load ("D:/Cursos/Pos_IA/Estatistica_aplicada_II/Arquivos_para_R/mw_weight.Rdata")

# Teste preliminar para verificar as suposições do 
# teste t independente

# Premissa 1: as duas amostras são independentes?
# Sim, pois as amostras de homens e mulheres não estão 
# relacionadas.
# Premissa 2: os dados de cada um dos 2 grupos seguem uma
#             distribuição normal?
# Use o teste de normalidade Shapiro-Wilk 
# - Hipótese nula: os dados são normalmente distribuídos 
# - Hipótese alternativa: os dados não estão normalmente
#                         distribuídos

# Usaremos shapiro.test() para calcular o teste Shapiro-Wilk
# para cada grupo de amostras.

# Teste de normalidade Shapiro-Wilk para os pesos masculinos

with(mw_weight, shapiro.test(weight[group == "Man"]))     
# p-value>0.05 (0.1066), logo a amostra possui distribuição
# normal

# Teste de normalidade Shapiro-Wilk para os pesos femininos

with(mw_weight, shapiro.test(weight[group == "Woman"]))     
# p-value>0.05 (0.6101), logo a amostra possui distribuição
# normal - Quanto mais longe de de 0.05, mais normal é

# Pelos resultados dos testes, os dois valores de p são 
# maiores do que o nível de significância 0.05, o que implica
# que a distribuição dos dados não é significativamente 
# diferente da distribuição normal. Em outras palavras, 
# podemos assumir que as amostras tem distribuição normal.

# Observe que se os dados não forem distribuídos normalmente,
# é recomendável usar outro teste de duas amostras não 
# paramétrico.

# Premissa 3. As duas populaçõees têm as mesmas variâncias?
# Usaremos o teste F para testar a homogeneidade nas 
# variâncias. Isso pode ser executado com a função var.test()
# da seguinte maneira:

res.ftest <- var.test(weight ~ group, data = mw_weight)
res.ftest

# H0: As variâncias são iguais 
# HA: As variâncias não são iguais

# O p-value do teste é p = 0.1714. É maior do que o nível
# de significância alfa = 0.05. Em conclusão, não há 
# diferença significativa entre as variâncias dos dois 
# conjuntos de dados. Portanto, podemos usar o teste t 
# clássico que assume a igualdade das duas variâncias.

# Pergunta: Existe alguma diferença significativa entre os 
# pesos das mulheres e dos homens?

res <- t.test(weight ~ group, data = mw_weight, 
              var.equal = TRUE)
res

# H0: O peso dos homens não é diferente estatisticamente do 
#     peso das mulheres
# HA: O peso dos homens é diferente estatisticamente do peso
#     das mulheres

# No resultado:
# t é o valor estatístico do teste t (t = 2.784),  df são 
# os graus de liberdade (df = 16),  O p-value é o nível de 
# significância do teste t (valor de p = 0.01327). conf.int 
# é o intervalo de confianca da média a 95% 
# (conf.int = [4.0298 , 29.748]);
# A estimativa da amostra é o valor médio da amostra 
# (média = 68.98889, 52.1).

# O valor de p do teste é 0.01327, que é menor que o nível 
# de significância alfa = 0,05. Pode-se concluir que o peso
# médio dos homens é significativamente diferente do peso 
# médio das mulheres.


############################################################
######### Teste de t para amostras emparelhadas ############


load ("D:/Cursos/Pos_IA/Estatistica_aplicada_II/Arquivos_para_R/paired_weight.Rdata")

# Queremos saber se existe alguma diferença significativa nos
# pesos médios dos ratos após o tratamento?

# Estatísticas descritivas

library("dplyr")

# %>% - Pipe (faz o group_by, depois o summarise)
group_by(paired_weight, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

# Teste preliminar para verificar as suposições do teste t
# pareado

# Premissa 1: as duas amostras estão emparelhadas?
# Sim, uma vez que os dados foram coletados medindo o peso 
# dos mesmos ratos.

# Premissa 2: esta é uma amostra grande?
# Não, n<30. Como o tamanho da amostra não é grande o 
# suficiente (menos de 30), precisamos verificar se as
# diferenças dos pares seguem uma distribuição normal.
# Use o teste de normalidade Shapiro-Wilk

# Hipótese nula: os dados são normalmente distribuídos
# Hipótese alternativa: os dados não são normalmente 
#                       distribuídos

# calcular a diferença

d <- with(paired_weight, 
          weight[group == "before"] - weight[group == "after"])
d

# Shapiro-Wilk normality test for the differences

shapiro.test(d)             # => p-value = 0.6141

# no resultado, o p-value é maior do que o nivel de 
# significância 0.05, o que implica que a distribuição
# das diferenças (d) não é significativamente diferente 
# da distribuição normal.
# Em outras palavras, podemos assumir a normalidade.

# Observe que se os dados não forem distribuídos normalmente,
# é recomendável usar um teste de duas amostras emparelhadas 
# não paramétrico.

# Pergunta: Existe alguma mudança significativa no peso dos
# ratos após o tratamento?

res <- t.test(weight ~ group, data = paired_weight, 
              paired = TRUE)
res

# H0: O peso dos ratos é estatisticamente igual
# HA: O peso dos ratos é estatisticamente diferente

# No resultado:
# t é o valor estatístico do teste t (t = 20.883),
# df são os graus de liberdade (df = 9),
# O p-value é o nível de significância do teste t
# (p-value = 6.20^{-9}).
# conf.int ? o intervalo de confiança das diferenças das 
# médias com 95%, e também é mostrado 
# (conf.int = [173,42, 215,56])
# A estimativa da amostra para as diferenças das médias 
# entre pares é (média = 194,49).

# O p-value do teste é 6.2^{-9}, que é menor que o nível 
# de significância alfa = 0.05. Podemos então rejeitar a 
# hipótese nula e concluir que o peso médio dos camundongos
# antes do tratamento é significativamente diferente 
# do peso médio após o tratamento.


############################################################
############# Teste One-way ANOVA ##########################

# Aqui, usaremos o conjunto de dados integrado ao R nominado
# PlantGrowth. Ele contém o peso das plantas obtidas sob 
# controle e duas condições de tratamento diferentes.

my_data <- PlantGrowth

# Para ter uma ideia de como são os dados, usamos a função 
# sample_n() [no pacote dplyr]. A função sample_n() escolhe
# aleatoriamente algumas das observações no quadro de dados
# para imprimir:

set.seed(1234)
#usar função sample_n do pacote dplyr
dplyr::sample_n(my_data, 10)

# Na terminologia R, a coluna "group" é chamada de fator e 
# as diferentes categorias ("ctr", "trt1", "trt2") são 
# chamadas de níveis de fator. Os níveis são ordenados
# alfabeticamente.

levels(my_data$group)

# Calcule algumas estatísticas por grupo-contagem, média e sd:

library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

### Cálculo do teste ANOVA unilateral ##

# Queremos saber se existe alguma diferença significativa 
# entre os pesos médios das plantas nas 3 condições 
# experimentais.

# A função aov() pode ser usada para responder a esta 
# pergunta. A função summary.aov() é usada para resumir o 
# modelo de análise de variância.

# Cálculo da análise da variância

res.aov <- aov(weight ~ group, data = my_data)

# Sumário estatístico da análise

summary(res.aov)

#qf(0.95,2,27)

# O resultado inclui as colunas F-value e Pr(>F) 
# correspondentes ao p-value do teste. Como o p-value é 
# menor que o nível de significância 0.05, pode-se 
# concluir que existem diferenças significativas entre os
# grupos, isso é destacado com "*" no sumário do modelo.

### Comparação múltipla de pares entre as médias dos grupos
# No teste ANOVA unilateral, um valor p significativo indica 
# que algumas das médias do grupo são diferentes, mas não 
# sabemos quais pares de grupos são diferentes. É possível 
# realizar múltiplas comparações de pares, para determinar 
# se a diferença média entre pares específicos do grupo é 
# estatisticamente significativa.

### Comparações de pares múltiplos de Tukey-um teste post-hoc, 
# pode ser usado para mais de 3 grupos
# Como o teste ANOVA é significativo, podemos calcular o 
# teste de Tukey HSD (Tukey Honest Significant Differences,
# função R: TukeyHSD()) 
# para realizar m?ltiplas comparações de pares entre as 
# médias dos grupos. A função TukeyHD() usa a ANOVA ajustada 
# como argumento.

TukeyHSD(res.aov)

# Parâmetros do teste:
# diff: estatística da diferença entre médias dos dois grupos
# lwr, upr: pontos inferior e superior do intervalo de 
#           confiança 95%
# p adj: valor p após o ajuste para as comparações múltiplas.
# Pode-se ver no resultado, que apenas a diferença entre 
# trt2 e trt1 é significativa com valor p ajustado de 0.012,
# ou seja, trt2 e trt1 são diferentes.

### Múltiplas comparações usando pacote multcomp
# É possível usar a função glht() [do pacote multcomp] para 
# realizar v?rios procedimentos de compara??o para uma ANOVA.
# "glht" significa testes de hipóteses lineares gerais. 
# Use glht() para realizar várias comparações de pares para 
# uma ANOVA unilateral:

library(multcomp)
summary(glht(res.aov, linfct = mcp(group = "Tukey")))

# Novamente foi significativa a diferença entre trt2 e trt1
# com p-value = 0.0121


### Teste t pareado
# A fun??o pairwise.t.test() tamb?m pode ser usada para 
# calcular compara??es de pares entre n?veis de grupo com
# corre??es para testes m?ltiplos.

pairwise.t.test(my_data$weight, my_data$group,
                p.adjust.method = "BH")

# O resultado ? uma tabela de p-values para as compara??es 
# entre pares. Aqui, os p-values foram ajustados pelo m?todo
# de Benjamini-Hochberg. Novamente o teste mostra que existe
# diferen?a significativa entre trt2 e trt1, as demais n?o.

# Verifique as suposi??es da ANOVA: testar a validade!!
# O teste ANOVA assume que os dados s?o normalmente 
# distribu?dos e a varia??o entre os grupos ? homog?nea. 
# Podemos verificar isso com alguns gr?ficos de diagn?stico.
# Tamb?m ? poss?vel usar o teste de Bartlett ou teste de 
# Levene para verificar a homogeneidade das vari?ncias.
# Recomenda-se o teste de Levene, que ? menos sens?vel a 
# desvios da distribui??o normal. A fun??o leveneTest() 
# [no pacote do car] ser? usada:

library(car)
leveneTest(weight ~ group, data = my_data)

# No resultado, pode-se ver que o p-value n?o ? menor que
# o n?vel de signific?ncia de 0.05. Isso significa que n?o
# existe evid?ncias de que a vari?ncia entre os grupos seja
# diferente. Portanto, podemos supor a homogeneidade das 
# vari?ncias nos diferentes grupos de tratamento.

### Relaxando a suposi??o da homogeneidade da vari?ncia
# O teste cl?ssico da ANOVA de um fator cl?ssico requer uma
# suposi??o de vari?ncias simlares para todos os grupos. 
# Em nosso exemplo, a suposi??o de homogeneidade da vari?ncia
# se confirmou: pois o teste de Levene n?o ? significativo.
# Mas como fazer na situa??o em que a suposi??o de 
# homogeneidade da vari?ncia ? violada? Usa-se um procedimento
# alternativo que n?o exige essa suposi??o.

## Teste de t pareados sem suposi??o de vari?ncias iguais

pairwise.t.test(my_data$weight, my_data$group,
                p.adjust.method = "BH", pool.sd = FALSE)

# O teste mostrou os mesmos resultados com p-value<0.05 para 
# trt1 e trt2, ou seja as medianas dos 2 grupos de tratamento
# s?o diferentes.

### Proximo passo: Teste Shapiro-Wilk para normalidade 
# dos 3 grupos

# Extra?ndo os res?duos

aov_residuals <- residuals(object = res.aov )

# Execuntando o teste Shapiro-Wilk

shapiro.test(x = aov_residuals )

# A conclus?o do teste de Shapiro-Wilk nos res?duos da ANOVA 
# (W = 0,96, p = 0,43) ? de que n?o se encontra ind?cios de 
# viola??o da normalidade. Se a suposi??o de normalidade 
# fosse rompida, uma op??o ? usar o teste n?o-param?trico
# da soma de ranking de Kruskal-Wallis

#############################################################

############ Testes Repeated-measures ANOVA #################

# ? um conjunto de 3 testes:One-way repeated measures ANOVA;
# Two-way repeated measures ANOVA ; e Three-way repeated 
# measures ANOVA. 

### Mesmos indiv?duos s?o medidos mais de uma vez.

########## One-way repeated measures ANOVA ###################

library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)


# A base de dados

data("selfesteem", package = "datarium")
head(selfesteem, 3)

# Reunir as colunas t1, t2 e t3 em formato longo
# Converter id e tempo em vari?veis de fator

selfesteem <- selfesteem %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)
head(selfesteem, 3)

# A ANOVA de medidas repetidas unilaterais pode ser usada
# para determinar se as m?dias dos escores de autoestima
# s?o significativamente diferentes entre os tr?s momentos.

# Sum?rio estat?stico

selfesteem %>%
  group_by(time) %>%
  get_summary_stats(score, type = "mean_sd")


# Criar um gr?fico "box plot" e adicionar pontos 
# correspondentes aos valores individuais:

bxp <- ggboxplot(selfesteem, x = "time", y = "score", 
                 add = "point")
bxp

## Checando as premissas

# Outliers podem ser facilmente identificados usando m?todos 
# de box plot, implementados pela fun??o R identify_outliers()
# [pacote rstatix].

selfesteem %>%
  group_by(time) %>%
  identify_outliers(score)

# observando o resultado do teste, n?o existem outliers 
# extremos, se houvesse is.extreme seria "TRUE"(se houvesse
# teriamos de deletar). Observe que, na sit??o em que voc? tem
# outliers extremos, isso pode ser devido a: erros de entrada 
# de dados; erros de medi??o; ou valores incomuns. Voc? pode 
# incluir o outlier na an?lise de qualquer maneira se n?o 
# acreditar que o resultado ser? substancialmente afetado. 
# Isso pode ser avaliado comparando o resultado da ANOVA com  
# e sem o outlier.Tamb?m ? poss?vel manter os outliers nos 
# dados e realizar um teste ANOVA robusto usando o pacote 
# WRS2.

## A premissa da normalidade

selfesteem %>%
  group_by(time) %>%
  shapiro_test(score)

# O escore de autoestima apresentou distribui??o normal em 
# cada tempo, conforme avaliado pelo teste de Shapiro-Wilk
# (p> 0,05).

# Observe que, se o tamanho da sua amostra for maior que 50, 
# o gr?fico QQ normal ? preferido porque em tamanhos de 
# amostra maiores, o teste de Shapiro-Wilk se torna muito 
# sens?vel at? mesmo a um pequeno desvio da normalidade.

# O gr?fico QQ desenha a correla??o entre uma vari?vel e a 
# distribui??o normal.
# Crie gr?ficos QQ para cada ponto de tempo:

ggqqplot(selfesteem, "score", facet.by = "time")

# No gr?fico, como todos os pontos caem aproximadamente ao
# longo da linha de refer?ncia, podemos assumir que a 
# vari?vel tem distribui??o normal.

### Suposi??o de esfericidade ou homogeneidade da amostra

# A suposi??o de esfericidade ? verificada automaticamente
# durante o c?lculo do teste ANOVA usando a fun??o 
# anova_test() [pacote rstatix]. O teste de Mauchly ? usado
# internamente para avaliar a suposi??o de esfericidade.
# Usando a fun??o get_anova_table() [pacote rstatix] para 
# extrair a tabela ANOVA, a corre??o de esfericidade de 
# Greenhouse-Geisser ? aplicada automaticamente aos
# fatores que violam a suposi??o de esfericidade.

res.aov <- anova_test(data = selfesteem, dv = score, 
                      wid = id, within = time)
res.aov
get_anova_table(res.aov)

# O escore de autoestima foi estatisticamente significativo 
# para diferen?a nos diferentes momentos durante a dieta, 
# F (2, 18) = 55,469, p<0,0001, eta2 [g] = 0,829. A 
# estat?stica F Indica que estamos comparando a uma 
# distribui??o F (teste F); (2, 18) indica os graus de 
# liberdade no numerador (DFn) e no denominador (DFd), 
# respectivamente; 55,469 indica o valor obtido da 
# estat?stica F, p especifica o p-value, "ges" ? o tamanho 
# do efeito generalizado 82.9% (quantidade de variabilidade 
# devido ao fator within-subjects)

## Testes Post-hoc
# Voc? pode realizar v?rios testes t pareados por pares 
# entre os n?veis dos fatores within-subjects (time). Os 
# p-values s?o ajustados usando o m?todo de corre??o
# do teste m?ltiplo de Bonferroni.

# Compara??es por pares

pwc <- selfesteem %>%
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

# Todas as diferen?as entre pares s?o estatisticamente 
# significativas.

# Resultados
# Pode-se analisar o resultado da seguinte maneira:
# O escore de autoestima foi diferente e estatisticamente 
# significativo nos diferentes tempos. As an?lises post-hoc 
# com ajuste de Bonferroni revelaram que todas as diferen?as
# aos pares, entre os pontos no tempo, foram diferentes e 
# estatisticamente significativas (p < 0.05).

# Visualiza??o: box plots com p-values
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

# Re?na as colunas t1, t2 e t3 no formato longo.
# Converta id e tempo em vari?veis de fator

selfesteem2 <- selfesteem2 %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)

# Inspecione algumas linhas aleat?rias dos dados por grupos

set.seed(123)
selfesteem2 %>% sample_n_by(treatment, time, size = 1)


# Neste exemplo, o efeito do "tempo" na pontua??o da 
# autoestima ? nossa vari?vel focal, essa ? nossa principal
# preocupa??o. Por?m, pensa-se que o efeito do "tempo" ser?
# diferente se o tratamento for realizado ou n?o. Nesse 
# cen?rio, a vari?vel "tratamento" ? considerada como 
# vari?vel moderadora.

# Estat?sticas de resumo
# Agrupe os dados por tratamento e tempo e, em seguida, 
# calcule algumas estat?sticas resumidas da vari?vel de 
# pontua??o: m?dia e desvio padr?o.

selfesteem2 %>%
  group_by(treatment, time) %>%
  get_summary_stats(score, type = "mean_sd")


## Visualiza??o
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


# N?o existem outliers extremos.

### Suposi??o de normalidade
# C?lculo do Teste de Shapiro-Wilk para cada combina??o de
# n?veis de fator:

selfesteem2 %>%
  group_by(treatment, time) %>%
  shapiro_test(score)

# O escore de autoestima apresentou distribui??o normal em 
# cada momento (p> 0.05), exceto para o fator ctr em t1.

# Criar o gr?fico QQ para cada grupo:

ggqqplot(selfesteem2, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ treatment, labeller = "label_both")

# No gr?fico, como todos os pontos caem aproximadamente ao 
# longo da linha de refer?ncia, podemos assumir normalidade.

### C?lculo do teste Two-way repeated measures ANOVA 

res.aov <- anova_test(
  data = selfesteem2, dv = score, wid = id,
  within = c(treatment, time)
)
res.aov

get_anova_table(res.aov)

# Existe uma intera??o bidirecional estatisticamente 
# significativa entre o tratamento e o tempo, 
# F (2, 22) = 30,424, p < 0,05.


### Testes Post-hoc
# Uma intera??o bidirecional significativa indica que o 
# impacto que um fator (por exemplo, tratamento) tem sobre
# a vari?vel de resultado (no exemplo, pontua??o de 
# autoestima) depende do n?vel do outro fator (por exemplo,
# tempo) (e vice-versa). Portanto, voc? pode decompor uma 
# intera??o bidirecional significativa em:

# Efeito principal simples: execute o modelo unilateral da 
# primeira vari?vel (fator A) em cada n?vel da segunda 
# vari?vel (fator B),

# Compara??es de pares simples: se o efeito principal 
# simples for significativo, execute v?rias compara??es de
# pares para determinar quais grupos s?o diferentes.

# Para uma intera??o bidirecional n?o significativa, voc?
# precisa determinar se existe algum efeito principal 
# estatisticamente significativo no resultado da ANOVA.

### Procedimento para uma intera??o bidirecional significativa 
# Efeito do tratamento. Em nosso exemplo, analisaremos o
# efeito do tratamento na pontua??o da autoestima em cada 
# momento.

# Observe que a vari?vel do fator de tratamento possui 
# apenas dois n?veis ("ctr" e "Diet"); assim, o teste ANOVA 
# e o teste t pareado fornecer?o os mesmos p-values.

# Efeito do tratamento em cada ponto no tempo

one.way <- selfesteem2 %>%
  group_by(time) %>%
  anova_test(dv = score, wid = id, within = treatment) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Resultado: O efeito do tratamento ? significativo nos 
# tempos t2 e t3 Considerando o p-value ajustado de 
# Bonferroni (p.adj), verifica-se que o simples efeito 
# principal do tratamento n?o foi significativo no momento 
# t1 (p = 1). Torna-se significativo em t2 (p = 0,036) e 
# t3 (p = 0,00051).

# Compara??es de pares entre grupos de tratamento

pwc <- selfesteem2 %>%
  group_by(time) %>%
  pairwise_t_test(
    score ~ treatment, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

# As compara??es pareadas mostram que a pontua??o m?dia de 
# autoestima foi significativamente diferente entre ctr e 
# o grupo Dieta em t2 (p = 0,12) e t3 (p = 0,00017), mas 
# n?o em t1 (p = 0,552).

# Efeito do tempo. 
# Observe que tamb?m ? poss?vel realizar a mesma an?lise
# para a vari?vel tempo em cada n?vel de tratamento. Voc? 
# n?o precisa necessariamente fazer essa an?lise.

# Efeito do tempo em cada n?vel de tratamento

one.way2 <- selfesteem2 %>%
  group_by(treatment) %>%
  anova_test(dv = score, wid = id, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2

# voc? pode ver que o efeito do tempo ? significativo apenas
# para o controle, F (2, 22) = 39,7, p <0,05. 

# Compara??es pareadas entre pontos no tempo

pwc2 <- selfesteem2 %>%
  group_by(treatment) %>%
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2

# As compara??es entre pares mostram que todas as 
# compara??es entre os pontos de tempo foram 
# estatisticamente significativas para o grupo de controle,
# mas n?o para o grupo de tratamento (dieta).

### Procedimento para intera??o bidirecional n?o significat.
# Se a intera??o n?o for significativa, voc? precisa 
# interpretar os efeitos principais de cada uma das duas 
# vari?veis: tratamento e tempo. Um efeito principal 
# significativo pode ser conseguido com compara??es de pares.

# Compara??es de teste t pareado:

# compara??es para a vari?vel tratamento

selfesteem2 %>%
  pairwise_t_test(
    score ~ treatment, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

# compara??es para a vari?vel tempo

selfesteem2 %>%
  pairwise_t_test(
    score ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

# Todas as compara??es pareadas s?o significantes.

## Relat?rio de resultados
# Podemos relatar o resultado da seguinte maneira:
# Uma ANOVA de medidas repetidas de duas vias foi realizada
# para avaliar o efeito de diferentes tratamentos de dieta 
# ao longo do tempo no escore de autoestima.

# Houve uma intera??o estatisticamente significativa entre
# o tratamento e o tempo no escore de autoestima, 
# F (2, 22) = 30,424, p<0,05. 
# Com isso, o efeito da vari?vel de tratamento foi analisado
# em cada momento. Os p-values foram ajustados usando o 
# m?todo de corre??o pelo teste m?ltiplo de Bonferroni. 
# O efeito do tratamento foi significativo em t2 (p = 0,036)
# e t3 (p = 0,00051), mas n?o no momento t1 (p = 1).

# As compara??es pareadas, usando o teste t pareado, mostram
# que a pontua??o m?dia da autoestima foi significativamente
# diferente entre ctr e dieta nos momentos t2 (p = 0,012) e 
# t3 (p = 0,00017), mas n?o em t1 (p = 0.55).

# Visualiza??o: box plots com p-values
pwc <- pwc %>% add_xy_position(x = "time")
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


######### Three-way repeated measures ANOVA ################

## Prepara??o dos dados

# Usaremos o conjunto de dados de emagrecimento 
# [pacote datarium]. Neste estudo, um pesquisador avaliou
# os efeitos da dieta e dos exerc?cios na perda de peso em
# 10 indiv?duos sedent?rios. Os participantes foram inclu?dos
# em quatro ensaios: (1) sem dieta e sem exerc?cios; 
# (2) dieta apenas; (3) exerc?cios apenas; e (4) dieta e
# exerc?cios combinados.
# Cada participante realizou todas as quatro tentativas. A 
# ordem dos testes foi contrabalan?ada e foi concedido tempo
# suficiente entre os testes para permitir que quaisquer 
# efeitos dos testes anteriores se dissipassem.
# Cada tentativa durou nove semanas e a pontua??o da perda
# de peso foi medida no in?cio (t1), no ponto m?dio (t2) e 
# no final (t3) de cada tentativa.
# A ANOVA de tr?s medidas repetidas pode ser realizada para
# determinar se existe intera??o significativa entre dieta,
# exerc?cios e tempo, no escore de perda de peso.

# Preparando os dados

library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)


set.seed(123)
data("weightloss", package = "datarium")
weightloss %>% sample_n_by(diet, exercises, size = 1)

# Re?na as colunas t1, t2 e t3 no formato longo.
# Converta id e tempo em vari?veis de fator

weightloss <- weightloss %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)

# Inspecione algumas linhas dos dados por grupos 
# aleatoriamente

set.seed(123)
weightloss %>% sample_n_by(diet, exercises, time, size = 1)


# Neste exemplo, o efeito "tempo" ? nossa vari?vel focal, 
# essa ? nossa principal preocupa??o.
# Pensa-se que o efeito "tempo" no escore de perda de peso 
# depender? de dois outros fatores, "dieta" e "exerc?cios", 
# chamados de vari?veis moderadoras.

### Estat?sticas de resumo

# Agrupe os dados por dieta, exerc?cios e tempo e em seguida
# calcule algumas estat?sticas de resumo da vari?vel de 
# pontua??o: m?dia e sd (desvio padr?o)

weightloss %>%
  group_by(diet, exercises, time) %>%
  get_summary_stats(score, type = "mean_sd")

### Visualiza??o

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

# N?o existem outliers extremos.


### Suposi??o de normalidade

# C?lculo do teste de Shapiro-Wilk para cada combina??o 
# de n?veis dos fatores:

weightloss %>%
  group_by(diet, exercises, time) %>%
  shapiro_test(score)

# O escore de perda de peso ? distribu?do normalmente, 
# conforme avaliado pelo teste de normalidade de 
# Shapiro-Wilk (p> 0,05).

# Criar QQ plot para cada grupo:

ggqqplot(weightloss, "score", ggtheme = theme_bw()) +
  facet_grid(diet+exercises~time, labeller="label_both")

# No gr?fico, como todos os pontos caem aproximadamente ao
# longo da linha de refer?ncia, podemos assumir normalidade.


#### C?lculo do teste para homogeneidade da amostra

res.aov <- anova_test(
  data = weightloss, dv = score, wid = id,
  within = c(diet, exercises, time)
)
res.aov
get_anova_table(res.aov)

# A partir do resultado, pode-se ver que existem intera??es
# tripartidas estatisticamente significativas entre dieta,
# exerc?cios e tempo,F (2, 22) = 14,246, p<0,05.
# Observe que, se a intera??o de tr?s vias n?o for 
# estatisticamente significativa, voc? precisa consultar o
# resultado das intera??es de duas vias.

# Em nosso exemplo, existe itera??o bidirecional da dieta 
# estatisticamente significativa: intera??o com exerc?cios
# (p <0,0001); 
# Al?m disso, existe itera??o bidirecional de exerc?cios:
# itera??o com tempo (p <0,0001). 
# E intera??o de duas vias da dieta: itera??o com tempo "n?o"
# foi estatisticamente significativa (p = 0.5).

#### Testes Post-hoc

# Se houver efeito significativo de intera??o de tr?s vias 
# voc? pode decompor em:

# 1) Intera??o bidirecional simples: execute a intera??o 
#    bidirecional em cada n?vel da terceira vari?vel,
# 2) Efeito principal simples: execute o modelo unilateral
#    em cada n?vel da segunda vari?vel, e
# 3) Compara??es pareadas simples: execute compara??es 
#    pareadas ou outras compara??es post-hoc, se necess?rio.

### C?lculo da intera??o bidirecional simples

# Voc? ? livre para decidir quais duas vari?veis formar?o 
# as intera??es bidirecionais simples e qual vari?vel atuar?
# como a terceira vari?vel (moderadora). No c?digo R a 
# seguir, consideramos a intera??o simples de duas vias de 
# exerc?cios * tempo em cada n?vel da dieta.

# Agrupe os dados por dieta e analise a intera??o simples 
# de duas vias entre exerc?cios e tempo:

# Two-way ANOVA em cada n?vel de dieta

two.way <- weightloss %>%
  group_by(diet) %>%
  anova_test(dv = score, wid = id, 
             within = c(exercises, time))

# Extrair Tabela anova

get_anova_table(two.way)

# Houve uma intera??o bidirecional simples estatisticamente
# significativa entre exerc?cios e tempo para o ensaio 
# "dieta n?o", F (2, 22) = 28,9, p <0,0001, mas n?o entre 
# exerc?cios e tempo para o ensaio "dieta sim",
# F (2, 22) = 2,57, p = 0,099.

# ? recomendado ajustar o p-value. Uma abordagem comum
# ? aplicar um ajuste de Bonferroni para ajustar o 
# n?vel no qual voc? declara a signific?ncia estat?stica.

# Isso pode ser feito dividindo o n?vel atual em que voc?
# declara a signific?ncia estat?stica (ou seja, p <0,05) 
# pelo n?mero de intera??es bidirecionais simples que voc?
# est? computando (ou seja, 2).

# Assim, voc? s? declara uma intera??o de duas vias como
# estatisticamente significativa quando p <0,025 
# (ou seja, p <0,05 / 2). Aplicando isso ao nosso exemplo
# atual, ainda tirar?amos as mesmas conclus?es.

### Calcular efeito principal simples simples

# Uma intera??o bidirecional simples estatisticamente 
# significativa pode ser seguida de efeitos principais
# simples.

# Em nosso exemplo, voc? poderia, portanto, investigar o
# efeito do tempo na pontua??o de perda de peso em todos
# os n?veis de exerc?cios ou investigar o efeito dos 
# exerc?cios em todos os n?veis de tempo.

# Voc? s? precisar? considerar o resultado das an?lises
# simples do efeito principal para o estudo "dieta n?o",
# pois essa foi a ?nica intera??o simples de duas vias
# que foi estatisticamente significativa.

# Agrupe os dados por dieta e exerc?cios e analise o efeito
# principal simples do tempo. O ajuste de Bonferroni deve  
# ser considerado e a signific?ncia estat?stica deve ser
# aceita no n?vel de p <0,025 (ou seja, 0,05 dividido pelo
# n?mero de testes (aqui 2) considerados para "dieta: n?o".

# Efeito do tempo em cada grupo dieta X exerc?cios

time.effect <- weightloss %>%
  group_by(diet, exercises) %>%
  anova_test(dv = score, wid = id, within = time)

# Extraindo a tabela anova

get_anova_table(time.effect) %>%
  filter(diet == "no")

# Houve um efeito principal simples e estatisticamente 
# significativo do tempo no escore de perda de peso para o
# grupo "dieta: n?o, exerc?cios: sim" (p <0,0001), mas n?o
# para quando nem dieta nem exerc?cios foram realizados 
# (p = 0,286).

### Calcule compara??es simples

# Um efeito principal simples estatisticamente significativo
# pode ser seguido por m?ltiplas compara??es de pares para
# determinar quais m?dias de grupo s?o diferentes.

# Agrupe os dados por dieta e exerc?cios e fa?a compara??es
# aos pares entre os pontos no tempo com o ajuste de 
# Bonferroni:

# Compara??es por pares

pwc <- weightloss %>%
  group_by(diet, exercises) %>%
  pairwise_t_test(score ~ time, paired = TRUE, 
                  p.adjust.method = "bonferroni") %>%
  select(-df, -statistic) # Remove alguns detalhes

# Mostrar resultados de compara??o para grupos "dieta: n?o,
# exerc?cios: sim"

pwc %>% filter(diet == "no", exercises == "yes") %>%
  select(-p)     # remove coluna p, s? interessa p. adj.


# Na tabela de compara??es de pares, estamos interessados 
# apenas nas compara??es simples para grupos "dieta: n?o,
# exerc?cios: sim". Em nosso exemplo, existem tr?s 
# combina??es poss?veis de diferen?as de grupo. Poder?amos
# relatar os resultados da compara??o entre pares como segue.

# Todas as compara??es pareadas simples foram feitas entre
# os diferentes pontos de tempo para o ensaio "dieta: n?o,
# exerc?cios: sim". O ajuste de Bonferroni foi aplicado. 
# A pontua??o m?dia de perda de peso foi significativamente
# diferente em todas as compara??es nos pontos de tempo
# quando os exerc?cios s?o realizados (p <0,05).

#### Relat?rio do teste

# Uma ANOVA de tr?s medidas repetidas foi realizada para 
# avaliar os efeitos da dieta, exerc?cios e tempo na perda
# de peso. Houve uma intera??o de tr?s vias estatisticamente
# significativa entre dieta, exerc?cios e tempo, 
# F (2, 22) = 14,2, p = 0,00011.

# Para as intera??es bidirecionais simples e an?lises de 
# efeitos principais simples, um ajuste de Bonferroni foi
# aplicado levando a signific?ncia estat?stica aceita no
# n?vel de p<0,025.

# Houve uma intera??o bidirecional simples estatisticamente
# significativa entre exerc?cios e tempo para o ensaio 
# "dieta n?o", F (2, 22) = 28,9, p <0,0001, mas n?o para o 
# ensaio "dieta sim" ", F (2, 22) = 2,6, p = 0,099.

# Houve um efeito principal simples e estatisticamente 
# significativo do tempo no escore de perda de peso para o
# ensaio "dieta: n?o, exerc?cios: sim" (p <0,0001),
# mas n?o para quando nem dieta nem exerc?cios foram 
# realizados (p = 0,286).

# Todas as compara??es pareadas simples foram feitas entre
# os diferentes pontos de tempo para o ensaio "dieta: n?o,
# exerc?cios: sim" com um ajuste de Bonferroni aplicado. 
# A pontua??o m?dia de perda de peso foi significativamente
# diferente em todas as compara??es nos pontos de tempo 
# quando os exerc?cios s?o realizados (p <0,05).

# Visualiza??o: box plots com p-values

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
