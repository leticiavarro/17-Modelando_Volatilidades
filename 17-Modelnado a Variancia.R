                    #Aula 16 - Metodologia Box- Jenkings

remove.packages("readxl")   #remove pacotes 
install.packages("readxl", dependencies = T)    #instala pacotes
remove.packages("aTSA")
install.packages("aTSA", dependencies = T)
remove.packages("tseries")
install.packages("tseries", dependencies = T)
install.packages("rugarch")

library(rugarch)
library(readxl)
library(aTSA)
library(tseries)
library("urca") 
library(forecast)

BITCOIN <- na.omit(read_excel("C:/EconometriaA/Bitcoin.xls"))

Bitcoin <-  ts(log(BITCOIN$Close), start = 2014, frequency = 365)


#Se não for estacionária, diferenciar a série

IntOrdem1 <- diff(log(BITCOIN$Close))
IntegradaOrdem1 <- ts(IntOrdem1, start = 2014, frequency = 365)

plot(IntegradaOrdem1, type="l", main="Primeira Diferança dos Logs do Bitcoin - LogReturn", ylab="Log Preço", xlab="Data", col="Blue")
grid(col = "black", lty = "dotted")


#Estimando Regressões e Tabelando Resultados
est1 <- data.frame()
for (i in 1:21) {                 #Loop para os AR: ARIMA(i,0,0)  #aplicar função em cima do próximo
  est1[i,1] <- paste("AR",i)      #Coluna com os nomes do Modelo
  est1[i,2] <- AIC(arima(IntegradaOrdem1,  order = c(i,1,0)))  #Coluna com valores AIC
  est1[i,3] <- BIC(arima(IntegradaOrdem1,  order = c(i,1,0)))  #Coluna com valores BIC
}
Resultados <- data.frame(rbind(est1))  
colnames(Resultados) <- c("Modelo","AIC","BIC")

#estimacoes <- list(AR1, AR2,AR3,AR4,AR5,
#                   AR6,AR7,AR8,AR9,AR10,
#                   AR11,AR12,AR13,AR14,AR15,
#                   AR16,AR17,AR18,AR19,AR20,AR21)      #Cria uma lista com os estimadores


#sapply(estimacoes, AIC)            #Aplica o comando AIC na lista
#sapply(estimacoes, BIC)            #Aplica o comando BIC na lista

#AIC <- sapply(estimacoes, AIC)      #Cria Coluna com resultados AIC
#BIC <- sapply(estimacoes, BIC)      #Cria Coluna com resultados BIC
#Modelo <- c("AR1", "AR2","AR3","AR4","AR5",
#            "AR6","AR7","AR8","AR9","AR10",
#            "AR11","AR12","AR13","AR14","AR15",
#            "AR16","AR17","AR18","AR19","AR20","AR21")   #cria coluna com nome dos modelos
#
#Resultados <- data.frame(Modelo, AIC, BIC)  #Junta as três colunas acima num único resultado
#View(Resultados)

#todo código acima pode ser resumido no código abaixo


#Efetuar teste ARCH-LM para o melhor modelo

arch.test(AR1)

#Modelando a Variância
AR1 <- arima(BITCOIN$Close, c (1,1,0))

residuos <- AR1$residuals
plot(residuos, type="o", main="Residuos do AR1")
grid(col = "black", lty = "dotted")

#FAC  e FACP  dos Residuos

acf(residuos,lend=2, lwd=5,col="darkblue",main= "Função Autocorrelação - FAC")              #Melhorando aspecto da FAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

pacf(residuos,lend=60, lwd=5,col="darkblue",main= "Função Autocorrelação Parcial - FACP")   #Melhorando aspecto da PAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")


GARCH90 <- garch(IntegradaOrdem1,c(9,0),trace=F)
GARCH80 <- garch(IntegradaOrdem1,c(8,0),trace=F)
GARCH70 <- garch(IntegradaOrdem1,c(7,0),trace=F) 
GARCH60 <- garch(IntegradaOrdem1,c(6,0),trace=F)
GARCH50 <- garch(IntegradaOrdem1,c(5,0),trace=F)
GARCH40 <- garch(IntegradaOrdem1,c(4,0),trace=F)
GARCH30 <- garch(IntegradaOrdem1,c(3,0),trace=F)
GARCH20 <- garch(IntegradaOrdem1,c(2,0),trace=F)
GARCH10 <- garch(IntegradaOrdem1,c(1,0),trace=F)



estimacoes_garch <- list(GARCH90,GARCH80,GARCH70,GARCH60,GARCH50,GARCH40,GARCH30,GARCH20,GARCH10)

AIC_Garch <- sapply(estimacoes_garch, AIC)      #Cria Coluna com resultados AIC

Modelos_Garch <- c("GARCH90","GARCH80","GARCH70","GARCH60","GARCH50","GARCH40","GARCH30","GARCH20","GARCH10")                                            #cria coluna com nome dos modelos

Resultados_garch <- data.frame(Modelos_Garch, AIC_Garch)  #Junta as três colunas acima num único resultado
View(Resultados_garch)

previsao1 <- predict(GARCH90,IntegradaOrdem1)

plot(previsao1,type="o", main="Volatilidade do Bitcoin", ylab="Preço", xlab="Data", col="Blue")
grid(col = "black", lty = "dotted")
     
previsao2 <- predict(GARCH71,IntegradaOrdem1,15)
plot(previsao2,type="o", main="Volatilidade do Bitcoin", ylab="Preço", xlab="Data", col="Blue")
grid(col = "black", lty = "dotted")