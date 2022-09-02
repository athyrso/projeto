
# RRN - conforme aula -----------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Instalação e Carregamento de Todos os Pacotes ------
{
   pacotes <- c("plotly", # graficos
                "tidyverse", # ferramentas
                "kableExtra", # tabelas
                "gridExtra", #graficos
                "readr", # importa arquivo
                "readxl", # importa dados
                "rnn",# rede neural
                "ggplot2", # grafico
                "grid", #grafico
                "beepr") # executa som
   
   if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
      instalador <- pacotes[!pacotes %in% installed.packages()]
      for(i in 1:length(instalador)) {
         install.packages(instalador, dependencies = T)
         break()}
      sapply(pacotes, require, character = T) 
   } else {
      sapply(pacotes, require, character = T) 
   }
   
}
#########################################################
# definindo diretório de trabalho

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#A função 'set.seed' define o ponto inicial dos calculos, garantindo reprodutiblidade dos mesmos.

set.seed(546)



##### Import dados: ####

#write.csv2(df_012, "d:/preliminar/df_012_rnn.csv")

#df_label_teste <- read_delim("D:/preliminar/df_label_teste.csv", 
#                            delim = ";",
#                           escape_double = FALSE,
#                          locale = locale(decimal_mark = ",", 
#                         grouping_mark = "."), na = "null", 
#                        trim_ws = TRUE)

data <- read_delim("D:/preliminar/df_012.csv", 
                   delim = ";", 
                   escape_double = FALSE,
                   locale = locale(decimal_mark = ",",
                                   grouping_mark = "."),
                   trim_ws = TRUE)
data <- read_excel("D:/preliminar/df_012_rnn.xlsx")


library(readxl)

omit
##################################################

data <- read_excel("D:/preliminar/rnn/df_NN.xlsx", 
                   sheet = "df_NN")

data_used <- (data[1:3000,])

data_used <- na.omit(MA[1:10000,])
data_used <- data_used[1:2400,]
glimpse(data_used)

plot(data_used$PT_U6_203.VAL)
#Pre-processamento

{y <- data_used[,2] # Pressao de vapor
   x <- data_used[,2:6]
   
   
   Yscaled = (y - min(y)) / (max(y) - min(y))
   Xscaled = (x - min(x)) / (max(x) - min(x))
   
   y <- Yscaled
   x <- Xscaled
   
   x <- as.matrix(x)
   y <- as.matrix(y)
   
   X <- matrix(x, nrow = 20)
   Y <- matrix(y, nrow = 20)
   
   #train test split
   train=1:80
   test=81:100}

#armazenagem do erro


# Modelo




{  
   
   model <- trainr(Y = Y[,train],
                     X = X[,train],
                     learningrate = 0.001,
                     hidden_dim = c(32),
                     network_type = "rnn",
                     numepochs = 1000000)
   
   
   #predição
   {
      Yp <- predictr(model, X[,test])
      
      #Percentual de variação em uma variável explicada por outra
      #por enquanto: entenda que é um percentual de variação explicada
      
      rsq <- function(y_actual,y_predict)
      {
         cor(y_actual,y_predict)^2
      }
      
      
      Ytest <- matrix(Y[,test], nrow = 1)
      Ytest <- t(Ytest)
      Ypredicted <- matrix(Yp, nrow = 1)
      Ypredicted <- t(Ypredicted)
      
      result_data <- data.frame(Ytest)
      result_data$Ypredicted <- Ypredicted     
      
      rsq(result_data$Ytest,result_data$Ypredicted)
      
      mean(result_data$Ytest)
      mean(result_data$Ypredicted)}
   
   
   plot(colMeans(model$error),type='l',xlab='epoch',ylab='errors')
   
   #grafico
   
   {
      
      plot(colMeans(model$error),type='l',xlab='epoch',ylab='errors')
      
      plot(as.vector(t(result_data$Ytest)), col = 'red', type='l',
           main = "Actual vs Predicted Humidity: testing set",
           ylab = "Y,Yp")
      lines(as.vector(t(Yp)), type = 'l', col = 'black')
      
      legend("bottomright", c("Predicted", "Actual"),
             col = c("red","black"),
             lty = c(1,1), lwd = c(1,1))}
   
   beepr::beep(4)
   
   
   
   
}


result_data


model_gru_6_6 <- model

#poucas épocas?
# plot(colMeans(model$error),type='l',xlab='epoch',ylab='errors')



save(model ,file =  "modelo_4.rdata")


















######
#EXERCICIO 1
```{r}
library("rnn")
library("dplyr")

data <- read.csv("PETR4.SA.csv")

#Inverter a ordem das ações para pegar da última para a ´primeira
data <-data[order(data$Date, decreasing = TRUE),]

fechamento <- data$Close

fechamento_anterior <- lead(fechamento,n=1L)

data_analise <- data.frame(fechamento)
data_analise$fechamento_anterior <- fechamento_anterior


summary(data_analise)

#exclui NA
data_analise <- data_analise[1:248,]


x <- data_analise[,2]
y <- data_analise[,1]


X <- matrix(x, nrow = 31)
Y <- matrix(y, nrow = 31)


Yscaled <- (Y - min(Y)) / (max(Y) - min(Y))
Xscaled <- (X - min(X)) / (max(X) - min(X))
Y <- Yscaled
X <- Xscaled


train=1:6
test=7:8

set.seed(12)
model <- trainr(Y = Y[,train],
                X = X[,train],
                learningrate = 0.05,
                hidden_dim = 20,
                numepochs = 1000,
                network_type = "rnn"
)


#no conjunto de treinamento
Ytrain <- t(matrix(predictr(model, X[,train]),nrow=1))
Yreal <- t(matrix(Y[,train],nrow=1))

#Percentual de variação em uma variável explicada por outra
rsq <- function(y_actual,y_predict){
   cor(y_actual,y_predict)^2
}

rsq(Yreal,Ytrain)

plot(Ytrain, type = "l", col = "darkred")
lines(Yreal, col = "darkblue", type = "l")

#no conjunto de teste
Ytest=matrix(Y[,test], nrow = 1)
Ytest = t(Ytest)
Yp <- predictr(model, Y[,test])
Ypredicted=matrix(Yp, nrow = 1)
Ypredicted=t(Ypredicted)

result_data <- data.frame(Ytest)
result_data$Ypredicted <- Ypredicted     

rsq(result_data$Ytest,result_data$Ypredicted)

mean(result_data$Ytest)
mean(result_data$Ypredicted)
```








ggplot()+
   geom_line(data = data_used, aes(x=dia_hora, y=pressao_vapor))+
   geom_line()












# Icaro -------------------------------------------------------------------



############################################
## Script desenvolvido por ï¿½caro Agostino ##
##### Email: icaroagostino@gmail.com #######
############################################

#Julho/2018

rm(list=ls()) #Limpando a memoria

########################
# chamando bibliotecas #
########################

# caso nï¿½o tenha instalado as bibliotecas abaixo use o comando:
# install.packages('nome da biblioteca')

library(tseries) #Manipular ST (Trapletti and Hornik, 2017)
library(TSA) #Manipular ST (Chan and Ripley, 2012)
library(lmtest) #Test. Hip. mod. lin. (Zeileis and Hothorn, 2002)
library(forecast) #Modelos de previsï¿½o (Hyndman and Khandakar, 2008)
library(ggplot2) #Elegant Graphics (Wickham, 2009)
library(ggfortify) #Manipular graf. (ST) (Horikoshi and Tang, 2016)

# Obs.: a biblioteca 'ggfortify' ï¿½ opcional, ela permite
# manipular melhor 'autoplot' para dados tipo ST.

########################
### Importando dados ###
########################

# para este exemplo vamos importar um banco direto da internet
# que estï¿½ hospedado em https://github.com/icaroagostino/ARIMA
# sï¿½o dados mensais do saldo de emprego do estado do Maranhï¿½o

dados <- read.table("https://raw.githubusercontent.com/icaroagostino/ARIMA/master/dados/MA.txt", header=T) #lendo banco
attach(dados) #tranformando em objeto

# precisamos tranformar os dados em ST utilizando o comando 'ts'
# o primeiro argumento da funï¿½ï¿½o ï¿½ o nome da variï¿½vel no banco

MA <- ts(MA, start = 2007, frequency = ) #tranformando em ST

# start = data da primeira observaï¿½ï¿½o
# frequency = 1  (anual)
# frequency = 4  (trimestral)
# frequency = 12 (mensal)
# frequency = 52 (semanal)

# caso queira importar direto do pc vocï¿½ precisa definir o 
# diretï¿½rio onde estï¿½o os dados, uma forma simples ï¿½ usar
# o atalho "Ctrl + Shift + H" ou atravï¿½s do comando abaixo

# setwd(choose.dir())

# a formato mais simples para importar dados ï¿½ o txt,
# substitua o nome do arquivo no comando read.table 
# mantendo a extenï¿½ï¿½o ".txt"

############################
## Etapa 1: Identificaï¿½ï¿½o ##
############################

# Inspeï¿½ï¿½o visual
MA <- dados_01
glimpse(MA)
autoplot(MA$PT_U6_203.VAL) +
   xlab("tempo") +
   ylab("Pressão")

# verificaï¿½ï¿½o da autocorrelaï¿½ao (acf)
# e aucorrelaï¿½ao parical (pacf)

ggtsdisplay(MA) #ST + acf + pacf
ggAcf(MA) #funï¿½ï¿½o de autocorrelaï¿½ï¿½o
ggPacf(MA) #funï¿½ï¿½o de autocorrelaï¿½ï¿½o parcial

########################
## Etapa 2: Estimaï¿½ï¿½o ##
########################

# para a estimaï¿½ï¿½o dos parametros e ajuste do modelo
# serï¿½ utilizado a funï¿½ï¿½o nnetar(), que utiliza o algoritimo
# baseado na funï¿½ï¿½o nnet() desenvolvido e publicado por
# Venables e Ripley (2002). Estï¿½ abordagem somente considera
# a arquitertura feed-forward networks com uma camada
# intermediï¿½ria usando a notaï¿½ï¿½o NNAR(p,k) para sï¿½ries sem
# sazonalidade e NNAR(p,P,k)[m] para sï¿½ries com sazonalidade
# sendo que 'p' representa o nï¿½mero de lags na camada de 
# entrada, 'k' o nï¿½mero de nï¿½s na camada intermediï¿½ria da
# rede, P ï¿½ nï¿½mero de lags sazonais e [m] a ordem sazonal

NNAR_fit <- nnetar(MA)
NNAR_fit #sai o modelo ajustado

# Estimaï¿½ï¿½o manual NNAR(p,P,k)[m]

# NNAR_fit_manual <- nnetar(MA, p = 1, P = 1, size = 1)

# Obs: informe os parï¿½metros a serem estimados, o primeiro 
# argumento ï¿½ a TS, seguido do nï¿½mero de p lags defasados,
# o nï¿½mero P lags sazonais, o nï¿½mero de k nï¿½s na camada
# intermadiï¿½ria, tambï¿½m ï¿½ possï¿½vel definir o nï¿½mero de
# repetiï¿½ï¿½es para o ajuste do modelo adicionando o argumento
# 'repeats = 20', o que acarretarï¿½ em um provavï¿½l aumento 
# da acurï¿½cia, mas tambï¿½m exigira maior tempo para o ajuste 
# da rede caso repeats > 20

###################################################
## Etapa 3: Validaï¿½ï¿½o (Verificaï¿½ï¿½o dos residuos) ##
###################################################

# Verificar se os residuos sï¿½o independentes (MA)

checkresiduals(forecast(NNAR_fit))

# Verificar os residuos padronizados (MA)

Std_res <- (resid(NNAR_fit) - mean(resid(NNAR_fit), na.rm = T)) / sd(resid(NNAR_fit), na.rm = T)

autoplot(Std_res) +
  geom_hline(yintercept = 2, lty=3) +
  geom_hline(yintercept = -2, lty=3) +
  geom_hline(yintercept = 3, lty=2, col="4") +
  geom_hline(yintercept = -3, lty=2, col="4")

#######################
## Etapa 4: previsï¿½o ##
#######################

# Nessa etapa ï¿½ definido o horizonte de previsï¿½o (h)

print(forecast(NNAR_fit, h = 12, PI = T))
autoplot(forecast(NNAR_fit, h = 12, PI = T))
accuracy(forecast(NNAR_fit)) #periodo de treino

# Obs.: a inclusï¿½o do intervalo de confianï¿½a aumenta
# consideravelmente o tempo de processamento, caso queira
# retirar basta mudar o argumento para 'PI = F'

# Como referï¿½ncia para maiores detalhes sobre diversos 
# aspesctos relacionados a previsï¿½o fica como sugestï¿½o
# o livro 'Forecast principles and practice' (Hyndman e 
# Athanasopoulos, 2018) o primeiro autor do livro ï¿½ 
# tambï¿½m criador do pacote 'forecast' utilizado neste
# script e o livro pode ser lido online gratuitamente
# em: https://otexts.org/fpp2/index.html

# Para maiores detalhes sobre aplicaï¿½ï¿½es de RNA em 
# linguagem R consulte a biblioteca 'nnet', desenvolvida
# por Venables e Ripley (2002), com a ultima versï¿½o 7.3
# de 2016 e para aplicaï¿½ï¿½es mais avanï¿½adas o pacote
# 'RSNNS', desenvolvida por Bergmeir e Benitez (2012),
# com a ultima versï¿½o 0.4 de 2017

# para referenciar as bibliotecas use o comando:
# citation('nome da biblioteca')




# Salvando arquivos -------------------------------------------------------

save(dados_01, file = "dados01.rdata")


