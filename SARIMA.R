############################################################################
###################### Modelos SARIMA (p,d,q)x(P,D,Q) ######################
############################################################################

rm(list = ls(all = TRUE))

X <- AirPassengers
plot.ts(X)


# Já podemos ver que essa série apresenta tendência e sazonalidade 


### Teste de Estacionáridade
library(tseries)
kpss.test(X)     # Rejeitamos H0: A série não se mostra estacionária


### Primeira Diferença
DX <- diff(X)
#DX <- data.frame(DX)

### Decomposição da Série

#dec <- decompose(DX)
#plot(dec)
#stl(DX, s.window = 11) %>% plot(main = 'STL decomposition (s.window = 11)')

par(mfrow = c(3,1))
plot.ts(DX)
acf(DX, lag.max = 20)
pacf(DX, lag.max = 20)

# Podemos ver que a série ainda não é estacionária, primeiro pela padronização dos
# valores observados, segundo pelo decaimento amortizado e ciclico dos graficos de
# autocorrelação.

### Testes para vermos quantas diferençãs devemos tomar

## Componente não sazional
ndiffs(X) # Podemos ver que o teste é falho para séries sazionais

## Componente Sazional
library(forecast)
nsdiffs(DX)

# Conclusão: Nosso modelo terá d=1, D=1 

### Aplicando a Diferença Sazional 
    # Como se trata de uma série mensal -> s = 12

D12DX = diff(DX, lag = 12)

#ndiffs(D12DX)
#nsdiffs(D12DX)


#########################################################################
###################### Ajustando Possiveis Modelos ######################
#########################################################################

par(mfrow = c(3,1))
plot.ts(D12DX)
acf(D12DX, lag.max = 5*12)
pacf(D12DX, lag.max = 5*12)


#################### Definindo valores dos parâmetros ####################

# p = 1            --> ACF Decaindo  rápido e PACF com quebra no lag 1
# q = 0 ou q = 1   --> PACF Decaindo rápido e ACF com uma quebra no lag 1, mas não muito expressiva.
# P = 0            --> ACF e PACF sem autocorrelações nos lags sazonias.
# Q = 0            --> ACF e PACF sem autocorrelações nos lags sazonias.


### Modelos Candidatos
# --> SARIMA(1,1,0) x (0,1,0)
# --> SARIMA(1,1,1) X (0,1,0)

# SARIMA(1,1,0) x (0,1,0)
SARIMA_1 <- arima(D12DX, order = c(1,1,0), seasonal = c(0,1,0), include.mean = FALSE)

# Résiduos
E <- SARIMA_1$residuals

### Análise Visulal
par(mfrow = c(2,2))
plot(E)
acf(E)
pacf(E)
qqnorm(E)
qqline(E)

### Testes estatísticos
# Estacionaridade
kpss.test(E)   # Estacionaria 

# Independência
Box.test(E, lag = 20, type = 'Ljung-Box', fitdf = 4) # H0 -> não são independentes

# Normalidade
shapiro.test(E) # H0 -> indícios de não normalidade.


# ------------------------


# SARIMA(1,1,1) x (0,1,0)
SARIMA_2 <- arima(D12DX, order = c(1,1,1), seasonal = c(0,1,0), include.mean = FALSE)

# Résiduos
E <- SARIMA_1$residuals

### Análise Visulal
par(mfrow = c(2,2))
plot(E)
acf(E)
pacf(E)
qqnorm(E)
qqline(E)

### Testes estatísticos
# Estacionaridade
kpss.test(E)   # Estacionaria 

# Independência
Box.test(E, lag = 20, type = 'Ljung-Box', fitdf = 4) # H0 -> não são independentes

# Normalidade
shapiro.test(E) # H0 -> indícios de não normalidade.


#### Comparativo

SARIMA_1
SARIMA_2

# Sarima 2 obteve um melhor AIC, portanto usaremos ele.
#### OBS: Era pra ter ficado melhor, os pontos da qqnorm não estão em cima da reta como deveriam.