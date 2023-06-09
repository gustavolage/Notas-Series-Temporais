#####################################################################
################# Modelos de Alisamento Exponencial #################
#####################################################################

X <- sunspot.year
X <- window(X, start = 1800, end = 1829)

plot.ts(X)



####### --> Simple Exponential smoothing forecasts
ses(X, h = 5) %>% plot() 
# Faz previs�es constantes


####### --> Modelo Linear de Holt
holt(X, h = 5) %>% plot()
# Agora temos a vari�vel tend�ncia 


####### --> Holt - Damped
holt(X, h = 5, damped = TRUE) %>% plot()
# A mesma coisa que em cima, por�m o c�lculo da componente tend�ncia � 
# dividido em v�rios hiperparametros


## OBS: Ajuste via minimiza��o da soma dos erros ao quadrado (EMQ)
#       Assim como nos modelos ARIMA, sob suposi��o de normalidade
#       o EMQ equivale ao estimador de m�xima verossimilhan�a.


####### --> Modelos de Holt - Winters (Sazional)
# Agora temos um modelo com componente level + tend�ncia + sazionalidade 
# OBS: S� funciona para s�ries com frequ�ncia > 1(m > 1), i.�.,  s�ries mensais, bimestrais, tri...

### HW - Aditivo (default)
fit.a <- HoltWinters(x = AirPassengers, seasonal = 'additive')
fit.a

# Smoothing parameters:
#   alpha: 0.2479595
#   beta : 0.03453373
#   gamma: 1

# beta ~ 0 -> fenomeno 'Drift', i.e., crescimento cont�nuo (b = 3.12)
# gamma ~ 1 -> o peso est� disposto apenas na ultima componente sazional (s� ela importa)

plot(fit.a)


### HW - Multiplicativo
fit.m <- HoltWinters(x = AirPassengers, seasonal = 'multiplicative')
fit.m

# Smoothing parameters:
#   alpha: 0.2755925
#   beta : 0.03269295
#   gamma: 0.8707292

# aplha e beta parecidos com hw aditivo
# gamma = 0.87 -> as componentes sazionais anteriores tem um pequeno peso tamb�m

plot(fit.m)


### Previs�es
library(forecast)
# h = 10
# 95% de conf.

predict(fit.a, n.ahead = 10, prediction.interval = TRUE)
forecast(fit.a, h = 10, level = 95) %>% plot()

predict(fit.m, n.ahead = 10, prediction.interval = TRUE)
forecast(fit.m, h = 10, level = 95) %>% plot()


## Comparativo
x <- cbind(AirPassengers, predict(fit.a, 24), predict(fit.m,24))
colnames(x) <- c('AirPassangers', 'HW-aditivo', 'HW-multiplicativo')

plot(x, plot.type = 'single', col=c(1,2,3), lwd=2, lty=c(1,2,3))
legend(x=1950, y=700, legend = colnames(x), box.lwd = 'white', col = c(1,2,3), text.col = c(1,2,3), lwd=2, lty = c(1,2,3))



########################################################################
####################### Pacote Forecast (BESTTT) #######################
########################################################################
require(forecast)

hw(y = AirPassengers, h = 10, seasonal = c('additive', 'multiplicative'),
   damped = TRUE, level = c(90,95), initial = c("optimal", "simple")) 


###### EXEMPLOS

#########################
### --> AirPassangers ###
#########################

## HW - Aditivo
hwa <- hw(AirPassengers, h=24, seasonal = 'additive', initial = 'o')
summary(hwa)
plot(hwa)

## HW - Multiplicativo
hwm <- hw(AirPassengers, h=24, seasonal = 'multiplicative', initial = 'o')
summary(hwm)
plot(hwm)

## HW - Multiplicativo + Damped
hwmd <- hw(AirPassengers, h=24, seasonal = 'multiplicative', initial = 'o',
           damped = TRUE)
summary(hwmd)
plot(hwmd)


### Sele��o do Modelo via AICc (Crit�rio de Akaike Corrigido)  
        ## LEMBRAR DE POR $model$aicc ##
        #
hwa$model$aicc
hwm$model$aicc
hwmd$model$aicc
# OBS:  Se o modelo tiver o par�metro initial = 's', aicc = NULL 

# Melhor: HW - Multiplicativo sem damped


### An�lise Residual
E <- hwm$residuals

## An�lise Visual
par(mfrow = c(2,2))
plot(E)
acf(E)
pacf(E)
qqnorm(E)
qqline(E)

# Podemos ver que ainda sobrou correla��o nos res�duos, e n�o conseguimos tratar isso neste modelo.

## Testes Estat�sticos
require(tseries)

# Est�cionaridade
kpss.test(E)    # H0: S�rie Estacion�ria
# Independ�ncia
Box.test(E, lag = 15, fitdf = 3)  # H1: N�o Independ�nte   ## fitdf = p+q 
#Normalidade
shapiro.test(E)  # H0: Normal


############################
####### --> airmiles #######
############################
par(mfrow = c(3,1))

plot(airmiles)
# Tem tendencia -> discartamos ses

# Holt
fit.holt <- holt(airmiles, h = 10, level = 95, initial = 'o')
plot(fit.holt)
# Holt Damped
fit.holt.damped <- holt(airmiles, h = 10, damped = TRUE, initial = 'o')
plot(fit.holt.damped)

### Sele��o do Modelo - AICc
fit.holt$model$aicc
fit.holt.damped$model$aicc

# Melhor: Holt sem damped


### An�lise Residual
E <- fit.holt$residuals

## An�lise Visual
par(mfrow = c(2,2))
plot(E)
acf(E)
pacf(E)
qqnorm(E)
qqline(E)

# Aparenta ser bem ajustado.

## Testes Estat�sticos

# Est�cionaridade
kpss.test(E)    # H0: S�rie Estacion�ria
# Independ�ncia
Box.test(E, lag = 15, fitdf = 2)  # H0: Independ�nte   ## fitdf = p+q 
#Normalidade
shapiro.test(E)  # H0: Normal
