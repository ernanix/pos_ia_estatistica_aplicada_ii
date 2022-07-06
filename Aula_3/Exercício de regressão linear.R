#################### AULA 3 ######################## 
# REGRESSÃO POR MQO (MÍNIMOS QUADRADOS ORDINÁRIOS) #
#               EXERCÍCIO PROPOSTO                 #
####################################################
# Carregando o arquivo de dados

load("D:/Cursos/Pos_IA/Estatistica_aplicada_I/exercicio2/imoveiscwbav.RData")
gc()
##### Estimando o modelo preliminar ################

resultados <- lm(price~age+parea+tarea+bath+
                   ensuit+garag+plaz+park+trans+
                   kidca+school+health+bike+barb+
                   balc+elev+fitg+party+categ,
                 data=imoveiscwbav)
summary (resultados)

## Pela teoria (artigos científicos) neste tipo de 
# estudo, devemos utilizar um modelo log-linear

imoveiscwbav$lnprice <- log(imoveiscwbav$price)

resultados <- lm(lnprice~age+parea+tarea+bath+
                   ensuit+garag+plaz+park+trans+
                   kidca+school+health+bike+barb+
                   balc+elev+fitg+party+categ,
                 data=imoveiscwbav)
summary (resultados)

## Verificando a presença de outliers pelo teste de 
#  Bonferroni

library(carData)
library(car)

outlierTest(resultados)

# por hora não faremos nada com os outliers

######## Teste Reset de especificação do modelo

# install.packages("zoo")

library (zoo)
library (lmtest)

resettest(lnprice~age+parea+tarea+bath+
            ensuit+garag+plaz+park+trans+
            kidca+school+health+bike+barb+
            balc+elev+fitg+party+categ,power=2:3, 
          type="regressor", 
          data=imoveiscwbav)


# H0 = o modelo está corretamente especificado; 
# HA = o modelo está incorretamente especificado;

# Resultado do teste

# RESET test
# data:  log(price) ~ .
# RESET=2.5388,df1=38, df2=483, p-value = 2.917e-06

# F tabelado:

qf(0.95, df1=38, df2=483)

# Como o F calculado (2.5388) é maior que o F tabelado
# (1,429987), existe erro de especificação do modelo


# Vamos testar agora o modelo log-log para variáveis
# com maior variância

summary(imoveiscwbav)
sd(imoveiscwbav$age)
sd(imoveiscwbav$parea)
sd(imoveiscwbav$tarea)
sd(imoveiscwbav$bath) # baixo desv. padrão, as demais 
# variáveis também são assim

imoveiscwbav$lnage <- log(imoveiscwbav$age)
imoveiscwbav$lnparea <- log(imoveiscwbav$parea)
imoveiscwbav$lntarea <- log(imoveiscwbav$tarea)

resettest(lnprice~lnage+lnparea+lntarea+bath+
            ensuit+garag+plaz+park+trans+
            kidca+school+health+bike+barb+
            balc+elev+fitg+party+categ,
            power=2:3, type="regressor", 
          data=imoveiscwbav)

# RESET test
# data:log(price)~lnage+lnparea+lntarea+bath+ensuit+
#        garag+plaz+park+trans+kidca+school+health+ 
#        bike+barb+balc+elev+fitg+party+categ
# RESET = 3.6012, df1 = 38, df2 = 483, p-value = 3.489e-11


# Ficou pior, vamos testar a especificação pelo 
# pacote PAJEN

##############Testando a especificação do modelo

#install.packages("PanJen")

library("PanJen")

formBase<-formula(lnprice~parea+tarea+bath+
                    ensuit+garag+plaz+park+trans+
                    kidca+school+health+bike+barb+
                    balc+elev+fitg+party+categ)
summary(lm(formBase, data=imoveiscwbav))

PanJenage<-fform(imoveiscwbav,"age",formBase)
# X^2 foi a forma vencedora, vamos elevar a variável
# "age" ao quadrado

imoveiscwbav$age2 <- imoveiscwbav$age^2

formBase<-formula(lnprice~age+tarea+bath+
                    ensuit+garag+plaz+park+trans+
                    kidca+school+health+bike+barb+
                    balc+elev+fitg+party+categ)
summary(lm(formBase, data=imoveiscwbav))

PanJenparea<-fform(imoveiscwbav,"parea",formBase)
# log(x) foi vencedora, então vamos criar uma variável
# com o log da variável "parea", mas ela já existe, 
# criamos ao tentar o modelo log-log

formBase<-formula(lnprice~age+parea+bath+
                    ensuit+garag+plaz+park+trans+
                    kidca+school+health+bike+barb+
                    balc+elev+fitg+party+categ)
summary(lm(formBase, data=imoveiscwbav))

PanJentarea<-fform(imoveiscwbav,"tarea",formBase)
# X foi vencedora, então devemos incluir a variável
# "tarea" como ela está

formBase<-formula(lnprice~age+parea+tarea+
                    ensuit+garag+plaz+park+trans+
                    kidca+school+health+bike+barb+
                    balc+elev+fitg+party+categ)
summary(lm(formBase, data=imoveiscwbav))

PanJenbath<-fform(imoveiscwbav,"bath",formBase)
# a variável tem valores com zeros na base de dados,
# falhou a estimativa, como a variância é pequena, 
# vamos deixar essa variável assim mesmo

formBase<-formula(lnprice~age+parea+tarea+bath+
                    garag+plaz+park+trans+
                    kidca+school+health+bike+barb+
                    balc+elev+fitg+party+categ)
summary(lm(formBase, data=imoveiscwbav))

PanJenensuit<-fform(imoveiscwbav,"ensuit",formBase)

# a variável tem valores com zeros na base de 
# dados, falhou a estimativa, como a variância é 
# pequena, vamos deixar essa variável assim mesmo

formBase<-formula(lnprice~age+parea+tarea+bath+
                    ensuit+plaz+park+trans+
                    kidca+school+health+bike+barb+
                    balc+elev+fitg+party+categ)
summary(lm(formBase, data=imoveiscwbav))

PanJengarag<-fform(imoveiscwbav,"garag",formBase)
# a variável tem valores com zeros na base de dados,
# falhou a estimativa, como a variância é pequena, 
# vamos deixar essa variável assim mesmo

formBase<-formula(lnprice~age+parea+tarea+bath+
                    ensuit+garag+park+trans+
                    kidca+school+health+bike+barb+
                    balc+elev+fitg+party+categ)
summary(lm(formBase, data=imoveiscwbav))

PanJenplaz<-fform(imoveiscwbav,"plaz",formBase)
# log(x) foi a vencedora então vamos criar uma 
# variável log para a variável "plaz"

imoveiscwbav$lnplaz <- log(imoveiscwbav$plaz)

formBase<-formula(lnprice~age+parea+tarea+bath+
                    ensuit+garag+plaz+trans+
                    kidca+school+health+bike+barb+
                    balc+elev+fitg+party+categ)
summary(lm(formBase, data=imoveiscwbav))

PanJenpark<-fform(imoveiscwbav,"park",formBase)
# log(x) foi a vencedora então vamos criar uma 
# variável log para a variável "park"

imoveiscwbav$lnpark <- log(imoveiscwbav$park)

formBase<-formula(lnprice~age+parea+tarea+bath+
                    ensuit+garag+plaz+park+
                    kidca+school+health+bike+barb+
                    balc+elev+fitg+party+categ)
summary(lm(formBase, data=imoveiscwbav))

PanJentrans<-fform(imoveiscwbav,"trans",formBase)
# 1/x foi a vencedora então vamos criar uma variável 
# 1/trans

imoveiscwbav$transinv <- 1/(imoveiscwbav$trans)

formBase<-formula(lnprice~age+parea+tarea+bath+
                    ensuit+garag+plaz+park+trans+
                    school+health+bike+barb+
                    balc+elev+fitg+party+categ)
summary(lm(formBase, data=imoveiscwbav))

PanJenkidca<-fform(imoveiscwbav,"kidca",formBase)
# base foi a vencedora então não vamos incluir essa
# variável no modelo

formBase<-formula(lnprice~age+parea+tarea+bath+
                    ensuit+garag+plaz+park+trans+
                    kidca+health+bike+barb+
                    balc+elev+fitg+party+categ)
summary(lm(formBase, data=imoveiscwbav))

PanJenschool<-fform(imoveiscwbav,"school",formBase)
# base foi a vencedora então não vamos incluir essa 
# variável no modelo

formBase<-formula(lnprice~age+parea+tarea+bath+
                    ensuit+garag+plaz+park+trans+
                    kidca+school+bike+barb+
                    balc+elev+fitg+party+categ)
summary(lm(formBase, data=imoveiscwbav))

PanJenhealth<-fform(imoveiscwbav,"health",formBase)
# base foi a vencedora então não vamos incluir essa 
# variável no modelo

formBase<-formula(lnprice~age+parea+tarea+bath+
                    ensuit+garag+plaz+park+trans+
                    kidca+school+health+barb+
                    balc+elev+fitg+party+categ)
summary(lm(formBase, data=imoveiscwbav))

PanJenbike<-fform(imoveiscwbav,"bike",formBase)

# base foi a vencedora então não vamos incluir essa
# variável no modelo

# As demais variáveis são binárias, não podem ser
# transformadas então não vamos testar elas transformadas,
# se tivermos que retirá-las do modelo a rotina stepwise
# apresentará esse resultado

# Vamos refazer o teste resset com as mudanças nas
# variáveis, mas por hora não vamos excluir nenhuma do
# modelo, somente se o stepwise confirmar

resettest(lnprice~age2+lnparea+tarea+bath+
            ensuit+garag+lnplaz+lnpark+transinv+
            kidca+school+health+bike+barb+
            balc+elev+fitg+party+categ,
          power=2:3, type="regressor", 
          data=imoveiscwbav)

# O resultado foi pior, vamos ver o que o stepwise 
# apresenta

resultados <- lm(lnprice~age2+lnparea+tarea+bath+
                   ensuit+garag+lnplaz+lnpark+transinv+
                   kidca+school+health+bike+barb+
                   balc+elev+fitg+party+categ,
                 data=imoveiscwbav)
summary (resultados)

library(RcmdrMisc)
step<-stepwise(resultados,direction='backward/forward',
               criterion ='AIC')
step

# agora vamos testar o modelo indicado pelo stepwise

resettest(lnprice~age2+lnparea+tarea+bath+
            ensuit+garag+lnplaz+lnpark+transinv+
            bike+balc+elev+fitg+party+categ,
          power=2:3, type="regressor", data=imoveiscwbav)

# Ficou pior, então vamos escolher o modelo com menor
# valor reset para aplicar o stepwise

resultados <- lm(lnprice~age+parea+tarea+bath+
                   ensuit+garag+plaz+park+trans+
                   kidca+school+health+bike+barb+
                   balc+elev+fitg+party+categ,
                 data=imoveiscwbav)
summary (resultados)

step<-stepwise(resultados,direction='backward/forward',
               criterion ='AIC')
step

# O melhor modelo escolhido foi

resultados <- lm(lnprice~age+parea+tarea+bath+
                   ensuit+garag+plaz+park+trans+
                   balc+elev+fitg+party+categ,
                 data=imoveiscwbav)
summary (resultados)

# Vamos ver como ficou o teste reset

resettest(lnprice~age+parea+tarea+bath+
            ensuit+garag+plaz+park+trans+
            balc+elev+fitg+party+categ,
          power=2:3, type="regressor", 
          data=imoveiscwbav)

# ainda contém algum erro de especificação

#################### Verificando a Multicolinearidade

# Correlação (Matriz de correlação)
# As variáveis binárias não entram na análise
cor(imoveiscwbav[,c("age","parea","tarea", "bath",
                    "ensuit","garag","plaz","park",
                    "trans","balc","elev","fitg",
                    "party","categ")],use="complete")

#Parece que parea e tarea, e talvez bath com 
# ensuit/tarea/parea, garag com tarea são 
# correlacionadas

# Vamos ver o VIF - Valor de Inflação da Variância

library(car)

vif(lm(lnprice~age+parea+tarea+bath+
         ensuit+garag+plaz+park+trans+
         balc+elev+fitg+party+categ,data=imoveiscwbav), 
    type="high-order")

# Pelo VIF que é uma medida mais acurada da 
# multicolinearidade percebe-se que parea está mais 
# próxima do escore 5 e pode haver correlação com 
# tarea, mas vimos pelar correlações que tarea pode
# estar correlacionada com outras variáveis "garag"
# por exemplo, neste caso vamos excluir tarea e 
# refazer o VIF

vif(lm(lnprice~age+parea+bath+
         ensuit+garag+plaz+park+trans+
         balc+elev+fitg+party+categ,data=imoveiscwbav), 
    type="high-order")

# Melhorou muito o VIF, vamos trocar parea por tarea

vif(lm(lnprice~age+tarea+bath+
         ensuit+garag+plaz+park+trans+
         balc+elev+fitg+party+categ,data=imoveiscwbav), 
    type="high-order")

# A diferença foi muito pequena, mas em princípio 
# "parea" é mais importante para o comprador de imóveis
# que "tarea", então vamos manter "parea"

# Refazendo o teste reset

resettest(lnprice~age+parea+bath+
            ensuit+garag+plaz+park+trans+
            balc+elev+fitg+party+categ,
          power=2:3, type="regressor", 
          data=imoveiscwbav)

# piorou um pouco

################### HOMOCEDASTICIDADE ##################
# Verificando a homocedasticidade = variância constante 
# (amostra homogênea?) para grandes amostras - mais de 
# 100 obs - utilizar o teste de Breusch-Pagan, caso 
# contrário utilizar o teste de Goldfeld-Quandt (gqtest)
# descrito na seção 11.5 do livro de Gujarati e Porter

bptest(lnprice~age+parea+bath+
         ensuit+garag+plaz+park+trans+
         balc+elev+fitg+party+categ,studentize=FALSE, 
       data=imoveiscwbav)

# H0: homocedático (variâncias constantes); 
# HA: Heterocedastico (variâncias não constantes)

# O resultado do teste é:

# Breusch-Pagan test
# data:lnprice~age+parea+bath+ensuit+garag+plaz+ 
#      park+trans+balc+elev+fitg+party+categ
# BP = 63.119, df = 13, p-value = 1.445e-08

# O valor chiquadrado tabelado é:

qchisq(0.95, df=13, lower.tail = TRUE)

# O resultado deve ser confrontado em um teste 
# qui-quadrado com k-1 graus de liberdade.
# Como o resultado do teste BP (63.119) é maior que o
# tabelado (22.36203) rejeita-se a hipótese de
# homocedasticidade 

# Vamos verificar quais variáveis tem alta variância

sd(imoveiscwbav$age)
sd(imoveiscwbav$parea)
sd(imoveiscwbav$bath)
sd(imoveiscwbav$ensuit)
sd(imoveiscwbav$garag)
sd(imoveiscwbav$plaz)
sd(imoveiscwbav$park)
sd(imoveiscwbav$trans)

#As variáveis age e parea tem maior variância

# Vamos reduzir a variância das variáveis por meio 
# do log das variáveis, já temos essas variáveis em
# log no dataset

library(lmtest)
bptest(lnprice~lnage+lnparea+bath+
         ensuit+garag+plaz+park+trans+
         balc+elev+fitg+party+categ,studentize=FALSE, 
       data=imoveiscwbav)

# Agora o resultado do teste é:
# Breusch-Pagan test
# data:lnprice~lnage + lnparea + bath + ensuit + garag + 
#            plaz + park + trans + balc + elev + fitg +
#            party + categ
# BP = 69.403, df = 13, p-value = 1.034e-09

# Não mudou muito, até piorou, persiste a 
# heterocedasticidade, vamos utilizar a regressão
# robusta sobre o modelo menos heterocedástico

# Correção da variância não constante por regressão
# robusta com o modelo com menor valor BP, ou seja, 
# o modelo anterior sem as variáveis em log

# A regressão normal é:

resultados <- lm(lnprice~age+parea+bath+
                   ensuit+garag+plaz+park+trans+
                   balc+elev+fitg+party+categ, 
                 data = imoveiscwbav)
summary(resultados)

# Agora a regressão robusta:

# install.packages("robust")
library(robust)

resultrob <- lmRob(lnprice~age+parea+bath+
                     ensuit+garag+plaz+park+trans+
                     balc+elev+fitg+party+categ,
                   data=imoveiscwbav)
summary(resultrob)

# Eliminando a variável "trans" que não foi 
# significativa, o modelo é:

resultados <- lm(lnprice~age+parea+bath+
                   ensuit+garag+plaz+park+
                   balc+elev+fitg+party+categ, 
                 data = imoveiscwbav)
summary(resultados)


resultrob <- lmRob(lnprice~age+parea+bath+
                     ensuit+garag+plaz+park+
                     balc+elev+fitg+party+categ,
                   data=imoveiscwbav)
summary(resultrob)

AIC(resultados)
BIC(resultados)

#install.packages("AICcmodavg")
library(AICcmodavg)

# AICc é usado para pequenas amostras
AICc(resultados)

#install.packages("performace")
library(performance)

# Menores valores de AIC, BIC, Sigma e RMSE são
# desejáveis. Maiores valores de R2 e R2(adj) são
# melhores. Sigma é o Erro Padrão dos Resíduos
# RMSE é a raiz quadrada média dos erros 
# (menor é melhor)

model_performance(resultados)
model_performance(resultrob)


# Calculando os intervalos de confiança - 
# Para regressão linear "normal"

confint(resultados, level = 0.95)

# Calculando os intervalos de confiança - 
# Para a regressão robusta (dos parâmetros)

confint(resultrob, level=0.95)

### Mas onde estão os resíduos??
resultrob$residuals
imoveiscwbav <- within(imoveiscwbav, 
                {residuos <- residuals(resultrob)})

# Amostra é grande não precisa testar normalidade 
# dos resíduos, mas se quisermos testar podemos usar

shapiro.test(imoveiscwbav$residuos)

# Os resíduos não são normalmente distribuídos
# se quisermos ser rigorosos poderemos transformar 
# com box-cox a variável Y e refazer o processo

hist(imoveiscwbav$price)
hist(imoveiscwbav$lnprice)

shapiro.test(imoveiscwbav$price)
shapiro.test(imoveiscwbav$lnprice)

### Mas onde estão os valores preditos (Y chapéu)
resultrob$fitted.values
imoveiscwbav <- within(imoveiscwbav, 
                {preditos<-fitted.values(resultrob)})

################ Fazendo predições no modelos OLS 

#Para:
#age = 5 anos
#parea = 80 metros quadrados
#bath = 1 banheiros
#ensuit = 1 suite
#garag = 1 garagem
#plaz = 0,5 km de distância da praça mais próxima
#park = 2 km de distância do parque mais próximo
#balc = 1 (imóvel possui sacada)
#elev = 1 (prédio possui elevador)
#fitg = 1 (prédio possui academia)
#party = 1 (prédio possui área de festas)
#categ = 1 (o imóvel é um apartamento - não é uma casa
#           em condomínio ou de rua)

propvalue<-predict(object = resultrob,
        data.frame(age=5, parea=80, bath=1, ensuit=1,
                   garag=1, plaz=0.5, park=2, balc=1,
                   elev=1, fitg=1, party=1, categ=1))

#o preço do imóvel é:
predantilog<-exp(propvalue)
predantilog
# R$626.519,30

# estimando os valores do intervalo de confiança

confintrob <- as.matrix(confint(resultrob, level=0.95))
newdata<- as.matrix(data.frame(age=5, parea=80, bath=1, 
                               ensuit=1,garag=1,plaz=0.5,
                               park=2, balc=1, elev=1, 
                               fitg=1, party=1,categ=1))

confintroblower <- confintrob[1,1]+(sum(newdata[1,]*
                                    confintrob[2:13,1]))
confintroblower
predantiloglower<-exp(confintroblower)
predantiloglower
# R$304.185,90

confintrobupper <- confintrob[1,2]+(sum(newdata[1,]*
                                    confintrob[2:13,2]))
confintrobupper
predantilogupper<-exp(confintrobupper)
predantilogupper
# R$1.290.416,00

# Então, o valor médio do preço do apartamento para os 
# dados de predição é de R$626.519,30 e pode variar entre 
# R$304.185,90 e R$1.290.416,00

## Os intervalos de confiança são muito amplos, isso devido 
# em parte à heterocedasticidade, apesar da correção, como
# OLS é uma regressão pela média, podemos alternativamente
# construir IC para a média calculada

# Confident intervals for our example

n <- nrow(imoveiscwbav)
m <- predantilog
s <- sd(imoveiscwbav$price)
dam <- s/sqrt(n)
CIlwr_rob <- m + (qnorm(0.025))*dam
CIupr_rob <- m - (qnorm(0.025))*dam 

CIlwr_rob
CIupr_rob

# Então o preço do imóvel pode variar entre R$582.971,00 e
# R$670.067,70 com preço médio de R$626.519,30


# Vimos que o modelo é heterocedástico e não-normal ######

#### VAMOS TENTAR UM MODELO ALTERNATIVO COM BOX-COX E
# RETIRAR OS OUTLIERS

### Transformação box-cox

library(AID)

imoveisav=as.matrix(imoveiscwbav)
X11(width = 10, height = 12)
outlm <- boxcoxlm(x = imoveisav[,2:20], y = imoveisav[,1])
# As colunas 2 a 20 são as variáveis X não transformadas,
# a coluna 1 é a coluna "price"

# Os resíduos não são normais após a transformação box-cox,
# vamos tentar eliminar outliers (voltamos com modelo 
# inicial, só que com preço em log - baseado na teoria)

resultados <- lm(price~age+parea+tarea+bath+
                   ensuit+garag+plaz+park+trans+
                   kidca+school+health+bike+barb+
                   balc+elev+fitg+party+categ,
                 data=imoveiscwbav)
summary (resultados)

# Verificando presença de outliers pelo teste de Bonferroni

library(carData)
library(car)

outlierTest(resultados)

# existem dois imóveis outliers

imoveiscwbav <- imoveiscwbav[imoveiscwbav$price!=4229891,]
imoveiscwbav <- imoveiscwbav[imoveiscwbav$price!=2892000,]

# rodando a regressão novamente e refazendo o teste

resultados <- lm(price~age+parea+tarea+bath+
                   ensuit+garag+plaz+park+trans+
                   kidca+school+health+bike+barb+
                   balc+elev+fitg+party+categ,
                 data=imoveiscwbav)
summary (resultados)

outlierTest(resultados)

# existem mais dois imóveis outliers, podemos eliminar com

imoveiscwbav <- imoveiscwbav[imoveiscwbav$price!=2818000,]
imoveiscwbav <- imoveiscwbav[imoveiscwbav$price!=1999100,]

# Rodando a regressão novamente e refazendo o teste

resultados <- lm(price~age+parea+tarea+bath+
                   ensuit+garag+plaz+park+trans+
                   kidca+school+health+bike+barb+
                   balc+elev+fitg+party+categ,
                 data=imoveiscwbav)
summary (resultados)

outlierTest(resultados)

# não temos mais outliers, vamos refazer o box-cox

imoveisav=as.matrix(imoveiscwbav)
X11(width = 10, height = 12)
outlm <- boxcoxlm(x = imoveisav[,2:20], 
                  y = imoveisav[,1])

# Agora os resíduos são normais após a transformação
# box-cox

outlm$lambda.hat #Estimate of Box-Cox param. based on
#                 Shapiro-Wilk test stat.
# O lambda é "0.07", vamos precisar dele para reverter
# o preço após o final processo 
outlm$p.value # p.value of Shapiro-Wilk test for 
#               transformed data (p-value>0.05)
outlm$tf.residuals # transformed data residuals
outlm$tf.data # transformed data for y variable

# Vamos incluir os dados transformados da variável y 
# no nosso dataset, já vamos transfromar em log

imoveiscwbav$lnpricetrans <- log(outlm$tf.data)

### Agora vamos refazer o processo, primeiro reavaliar
# o teste resset

resettest(lnpricetrans~age+parea+tarea+bath+
            ensuit+garag+plaz+park+trans+
            kidca+school+health+bike+barb+
            balc+elev+fitg+party+categ,
          power=2:3, type="regressor", data=imoveiscwbav)

qf(0.95, df1=38, df2=479)


# RESET test
# data:lnpricetrans~age + parea + tarea + bath + ensuit +
#                   garag + plaz + park + trans + kidca +
#                   school + health + bike + barb +     
#                   balc + elev + fitg + party + categ
# RESET = 2.5178, df1 = 38, df2 = 479, p-value = 4.513e-08

# valor do teste reset é maior que o valor tabelado,
# então temos problema com a especificação do modelo,
# isso pode ser em decorrência de uma variável que não
# está presente no modelo

# Como fizemos o Box-Cox com as variáveis acima, não
# podemos transformá-las pois poderemos perder a 
# normalidade da regressão

# Vamos ver o teste de heterocedasticidade

bptest(lnpricetrans~age+parea+tarea+bath+
         ensuit+garag+plaz+park+trans+
         kidca+school+health+bike+barb+
         balc+elev+fitg+party+categ,studentize=FALSE, 
       data=imoveiscwbav)

# Ainda temos heterocedasticidade, teremos de aplicar a 
# regressão robusta

## Vamos ver a multicolinearidade

vif(lm(lnpricetrans~age+parea+tarea+bath+
         ensuit+garag+plaz+park+trans+
         kidca+school+health+bike+barb+
         balc+elev+fitg+party+categ,data=imoveiscwbav), 
    type="high-order")

# Vamos eliminar "tarea" e ver como fica

vif(lm(lnpricetrans~age+parea+bath+
         ensuit+garag+plaz+park+trans+
         kidca+school+health+bike+barb+
         balc+elev+fitg+party+categ,data=imoveiscwbav), 
    type="high-order")

# Vamos testar eliminando "parea"

vif(lm(lnpricetrans~age+tarea+bath+
         ensuit+garag+plaz+park+trans+
         kidca+school+health+bike+barb+
         balc+elev+fitg+party+categ,data=imoveiscwbav), 
    type="high-order")

# Ficou muito parecido vamos eliminar "tarea"

# Vamos ver o que o stepwise apresenta

resultados <- lm(lnpricetrans~age+parea+bath+
                   ensuit+garag+plaz+park+trans+
                   kidca+school+health+bike+barb+
                   balc+elev+fitg+party+categ,
                 data=imoveiscwbav)
summary (resultados)

library(RcmdrMisc)
step<-stepwise(resultados, direction= 'backward/forward',
                 criterion ='AIC')
step

# agora vamos testar o modelo indicado pelo stepwise

resettest(lnpricetrans~age+parea+bath+
            ensuit+garag+park+trans+barb+
            balc+elev+fitg+party+categ,
          power=2:3, type="regressor", data=imoveiscwbav)

qf(0.95, 26,497)

# Apesar de ainda não passar no teste reset, 
# vamos escolher este modelo 

# Uma última checagem no teste de heterocedasticidade

bptest(lnpricetrans~age+parea+bath+
         ensuit+garag+park+trans+barb+
         balc+elev+fitg+party+categ,studentize=FALSE, 
       data=imoveiscwbav)

qchisq(0.95, df=13, lower.tail = TRUE)

# Melhorou, mas ainda temos heterocedasticidade

# O melhor modelo escolhido foi

resultados <- lm(lnpricetrans~age+parea+bath+
                   ensuit+garag+park+trans+barb+
                   balc+elev+fitg+party+categ,
                 data=imoveiscwbav)
summary (resultados)

# Agora a regressão robusta:

# install.packages("robust")
library(robust)

resultrob <- lmRob(lnpricetrans~age+parea+bath+
                     ensuit+garag+park+trans+barb+
                     balc+elev+fitg+party+categ,
                   data=imoveiscwbav)
summary(resultrob)

# retiramos as variáveis "trans" e "barb" que não
# foram significativas, o modelo é:

resultrob <- lmRob(lnpricetrans~age+parea+bath+
                   ensuit+garag+park+balc+elev+
                   fitg+party+categ, 
                   data = imoveiscwbav)
summary(resultrob)

## Ainda temos variáveis não significativas a 95% 
# vamos retirar "ensuit"

resultrob <- lmRob(lnpricetrans~age+parea+bath+
                   garag+park+balc+elev+fitg+party+categ, 
                   data = imoveiscwbav)
summary(resultrob)

# Agora todas são significativas a 95% de confiança

# A regressão normal seria

resultados <- lm(lnpricetrans~age+parea+bath+
                 garag+park+balc+elev+fitg+party+categ,
                 data = imoveiscwbav)
summary(resultados)


AIC(resultados)
BIC(resultados)

#install.packages("AICcmodavg")
library(AICcmodavg)

# AICc é usado para pequenas amostras
AICc(resultados)

#install.packages("performace")
library(performance)

# Menores valores de AIC, BIC, Sigma e RMSE são 
# desejáveis. Maiores valores de R2 e R2(adj) 
# são melhores. Sigma é o Erro Padrão dos Resíduos
# RMSE é a raiz quadrada média dos erros 
# (menor é melhor)

model_performance(resultados)
model_performance(resultrob)


# Calculando os intervalos de confiança - 
# Para regressão linear "normal" (para os parâmetros)

confint(resultados, level = 0.95)

# Calculando os intervalos de confiança - 
# Para a regressão robusta (para os parâmetros)

confint(resultrob, level = 0.95)

### Mas onde estão os resíduos??
resultrob$residuals
imoveiscwbav <- within(imoveiscwbav, 
                {residuos <- residuals(resultrob) })

# testando a normalidade dos resíduos

shapiro.test(imoveiscwbav$residuos)

# Vejamos o teste de kolmogorov-smirnov

library(RcmdrMisc)
normalityTest(~residuos, test="lillie.test", 
              data=imoveiscwbav)

# a distribuição não é normal segundo teste de shapiro-wilk, 
# ao excluirmos variáveis altera a situação de normalidade.
# Mas o problema não é grave, veja que quase passou no
# teste de kolmogorov-smirnov que é um dos testes mais usados, 
# com P-value=0.0455 é quase 0.05.
# Perceba que poderemos comprovar que existe normalidade 
# com 1% de significância (99%) de confiança (p-valor>0.01)

### Mas onde estão os valores preditos (Y chapéu)
resultrob$fitted.values
imoveiscwbav <- within(imoveiscwbav, 
                       {preditos <- fitted.values(resultrob)})

################ Fazendo predições no modelos OLS 

#Para:
#age = 5 anos
#parea = 80 metros quadrados
#bath = 1 banheiros
#garag = 1 garagem
#park = 2 km de distância do parque mais próximo
#balc = 1 (imóvel possui sacada)
#elev = 1 (prédio possui elevador)
#fitg = 1 (prédio possui academia)
#party = 1 (prédio possui área de festas)
#categ = 1 (o imóvel é um apartamento - não é uma casa 
#           em condomínio ou de rua)

propvalue <- predict(object = resultrob,
        data.frame(age=5, parea=80, bath=1, 
                   garag=1, park=2, balc=1,
                   elev=1, fitg=1, party=1, categ=1))

# Vamos tirar o antilog:
predantilog<-exp(propvalue)

# Agora vamos voltar a transformação de box-cox

# Lembrando que Yboxcox = (Y^lamda-1)/lambda 

# O nosso lambda é:
outlm$lambda.hat

# então:

valormedioimovel2<-(((predantilog*outlm$lambda.hat)+1))^
                    (1/outlm$lambda.hat)
valormedioimovel2
# R$590.381,20


confintrob2 <- as.matrix(confint(resultrob, level=0.95))
newdata<- as.matrix(data.frame(age=5, parea=80, bath=1, 
           garag=1, park=2, balc=1, elev=1, fitg=1, 
           party=1, categ=1))

confintroblower2 <- confintrob2[1,1]+(sum(newdata[1,]*
                                  confintrob2[2:11,1]))
confintroblower2
predantilog<-exp(confintroblower2)
valorimovellower2<-(((predantilog*outlm$lambda.hat)+1))^
                   (1/outlm$lambda.hat)
valorimovellower2
# R$326.612,90

confintrobupper2 <- confintrob2[1,2]+(sum(newdata[1,]*
                                  confintrob2[2:11,2]))
confintrobupper2
predantilog<-exp(confintrobupper2)
valorimovelupper2<-(((predantilog*outlm$lambda.hat)+1))^
                   (1/outlm$lambda.hat)
valorimovelupper2
# R$1.084.841,00

# O valor predito médio para o imóvel é de R$590.381,20,
# e pode variar entre R$326.612,90 a R$1.084.841,00

# Confident intervals for mean

n <- nrow(imoveiscwbav)
m <- valormedioimovel2
s <- sd(imoveiscwbav$price)
dam <- s/sqrt(n)
CIlwr_rob2 <- m + (qnorm(0.025))*dam
CIupr_rob2 <- m - (qnorm(0.025))*dam 

CIlwr_rob2
CIupr_rob2

# O preço médio para o imóvel é R$590.381,20 e pode variar
# entre R$549.531,20 e R$631.231,30