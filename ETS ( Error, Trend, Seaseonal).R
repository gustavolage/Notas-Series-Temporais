#########################################################################
################ Modelos ETS ( ERROR, TREND, SEASEONAL ) ################
#########################################################################
rm(list = ls(all = T))
### AirPassangers

### MEE: Modelos de Espa�o de Estado

## Ajuste

fit = ets(AirPassengers)  # Automaticamente encontrar� o melhor modelo
# OBS: Necess�rio analise de residuos (ets() n�o faz)

summary(fit)

# para esse banco de dados o modelo escolhido foi:
#                ETS(M,Ad,M) 

## Visual
plot(fit)


### An�lise de Res�duos
E <- fit$residuals

## Visual Respiduos
par(mfrow = c(2,2))
plot(E)
acf(E)
pacf(E)
qqnorm(E)
qqline(E)

# Podemos ver que se comportou bem, por�m ainda sobrou um restinho de correla��o
# nas componentes sazionais (lag = 1)

### Testes Estat�sticos

# Estacionaridade
require(tseries)
kpss.test(E) # Est�cinaria

# Independ�ncia
Box.test(E, lag =15, type = 'Ljung-Box', fitdf = 3)  # N�o Independ�ntes

# Normalidade
shapiro.test(E)  # Res�duos Normais


### Previs�o 24 meses a frente
par(mfrow = c(1,1))

require(forecast)

forecast(fit, h = 24, level = c(80, 95)) %>% plot()

# Previs�es intervalares n�o param�tricas
forecast(fit, h = 24, level = c(80, 95), bootstrap = T) %>% plot()


### Debitcards
require(fpp2)
plot(debitcards)

fit2 <- ets(debitcards)

summary(fit2) # ETS(M,A,M) 

E <- fit2$residuals

par(mfrow = c(2,2))
plot(E)
acf(E)
pacf(E)
qqnorm(E)
qqline(E)

# Aparentemnete estaciion�ria
# Sobrou correla��es em alguns lags

kpss.test(E)
Box.test(E, lag = 15, type = 'Ljung-Box', fitdf = 3)
shapiro.test(E)

par(mfrow = c(1,1))

forecast(fit2, h = 36, level = 95) %>% plot()

# Intervalar N�o Param�trico
forecast(fit2, h = 36, level = 95, bootstrap = T) %>% plot()
