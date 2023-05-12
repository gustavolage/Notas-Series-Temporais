#########################################################################
################ Modelos ETS ( ERROR, TREND, SEASEONAL ) ################
#########################################################################
rm(list = ls(all = T))
### AirPassangers

### MEE: Modelos de Espaço de Estado

## Ajuste

fit = ets(AirPassengers)  # Automaticamente encontrará o melhor modelo
# OBS: Necessário analise de residuos (ets() não faz)

summary(fit)

# para esse banco de dados o modelo escolhido foi:
#                ETS(M,Ad,M) 

## Visual
plot(fit)


### Análise de Resíduos
E <- fit$residuals

## Visual Respiduos
par(mfrow = c(2,2))
plot(E)
acf(E)
pacf(E)
qqnorm(E)
qqline(E)

# Podemos ver que se comportou bem, porém ainda sobrou um restinho de correlação
# nas componentes sazionais (lag = 1)

### Testes Estatísticos

# Estacionaridade
require(tseries)
kpss.test(E) # Estácinaria

# Independência
Box.test(E, lag =15, type = 'Ljung-Box', fitdf = 3)  # Não Independêntes

# Normalidade
shapiro.test(E)  # Resíduos Normais


### Previsão 24 meses a frente
par(mfrow = c(1,1))

require(forecast)

forecast(fit, h = 24, level = c(80, 95)) %>% plot()

# Previsões intervalares não paramétricas
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

# Aparentemnete estaciionária
# Sobrou correlações em alguns lags

kpss.test(E)
Box.test(E, lag = 15, type = 'Ljung-Box', fitdf = 3)
shapiro.test(E)

par(mfrow = c(1,1))

forecast(fit2, h = 36, level = 95) %>% plot()

# Intervalar Não Paramétrico
forecast(fit2, h = 36, level = 95, bootstrap = T) %>% plot()
