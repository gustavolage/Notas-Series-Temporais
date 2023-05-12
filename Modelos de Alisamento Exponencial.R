#####################################################################
################# Modelos de Alisamento Exponencial #################
#####################################################################

X <- sunspot.year
X <- window(X, start = 1800, end = 1829)

plot.ts(X)



####### --> Simple Exponential smoothing forecasts
ses(X, h = 5) %>% plot() 
# Faz previsões constantes


####### --> Modelo Linear de Holt
holt(X, h = 5) %>% plot()
# Agora temos a variável tendência 


####### --> Holt - Damped
holt(X, h = 5, damped = TRUE) %>% plot()
# A mesma coisa que em cima, porém o cálculo da componente tendência é 
# dividido em vários hiperparametros


## OBS: Ajuste via minimização da soma dos erros ao quadrado (EMQ)
#       Assim como nos modelos ARIMA, sob suposição de normalidade
#       o EMQ equivale ao estimador de máxima verossimilhança.


####### --> Modelos de Holt - Winters (Sazional)
# Agora temos um modelo com componente level + tendência + sazionalidade 
# OBS: Só funciona para séries com frequência > 1(m > 1), i.é.,  séries mensais, bimestrais, tri...

### HW - Aditivo (default)
fit.a <- HoltWinters(x = AirPassengers, seasonal = 'additive')
fit.a

# Smoothing parameters:
#   alpha: 0.2479595
#   beta : 0.03453373
#   gamma: 1

# beta ~ 0 -> fenomeno 'Drift', i.e., crescimento contínuo (b = 3.12)
# gamma ~ 1 -> o peso está disposto apenas na ultima componente sazional (só ela importa)

plot(fit.a)


### HW - Multiplicativo
fit.m <- HoltWinters(x = AirPassengers, seasonal = 'multiplicative')
fit.m

# Smoothing parameters:
#   alpha: 0.2755925
#   beta : 0.03269295
#   gamma: 0.8707292

# aplha e beta parecidos com hw aditivo
# gamma = 0.87 -> as componentes sazionais anteriores tem um pequeno peso também

plot(fit.m)


### Previsões
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


### Seleção do Modelo via AICc (Critério de Akaike Corrigido)  
        ## LEMBRAR DE POR $model$aicc ##
        #
hwa$model$aicc
hwm$model$aicc
hwmd$model$aicc
# OBS:  Se o modelo tiver o parâmetro initial = 's', aicc = NULL 

# Melhor: HW - Multiplicativo sem damped


### Análise Residual
E <- hwm$residuals

## Análise Visual
par(mfrow = c(2,2))
plot(E)
acf(E)
pacf(E)
qqnorm(E)
qqline(E)

# Podemos ver que ainda sobrou correlação nos resíduos, e não conseguimos tratar isso neste modelo.

## Testes Estatísticos
require(tseries)

# Estácionaridade
kpss.test(E)    # H0: Série Estacionária
# Independência
Box.test(E, lag = 15, fitdf = 3)  # H1: Não Independênte   ## fitdf = p+q 
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

### Seleção do Modelo - AICc
fit.holt$model$aicc
fit.holt.damped$model$aicc

# Melhor: Holt sem damped


### Análise Residual
E <- fit.holt$residuals

## Análise Visual
par(mfrow = c(2,2))
plot(E)
acf(E)
pacf(E)
qqnorm(E)
qqline(E)

# Aparenta ser bem ajustado.

## Testes Estatísticos

# Estácionaridade
kpss.test(E)    # H0: Série Estacionária
# Independência
Box.test(E, lag = 15, fitdf = 2)  # H0: Independênte   ## fitdf = p+q 
#Normalidade
shapiro.test(E)  # H0: Normal
