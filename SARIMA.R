############################################################################
###################### Modelos SARIMA (p,d,q)x(P,D,Q) ######################
############################################################################

rm(list = ls(all = TRUE))

X <- AirPassengers
plot.ts(X)


# J� podemos ver que essa s�rie apresenta tend�ncia e sazonalidade 


### Teste de Estacion�ridade
library(tseries)
kpss.test(X)     # Rejeitamos H0: A s�rie n�o se mostra estacion�ria


### Primeira Diferen�a
DX <- diff(X)
#DX <- data.frame(DX)

### Decomposi��o da S�rie

#dec <- decompose(DX)
#plot(dec)
#stl(DX, s.window = 11) %>% plot(main = 'STL decomposition (s.window = 11)')

par(mfrow = c(3,1))
plot.ts(DX)
acf(DX, lag.max = 20)
pacf(DX, lag.max = 20)

# Podemos ver que a s�rie ainda n�o � estacion�ria, primeiro pela padroniza��o dos
# valores observados, segundo pelo decaimento amortizado e ciclico dos graficos de
# autocorrela��o.

### Testes para vermos quantas diferen��s devemos tomar

## Componente n�o sazional
ndiffs(X) # Podemos ver que o teste � falho para s�ries sazionais

## Componente Sazional
library(forecast)
nsdiffs(DX)

# Conclus�o: Nosso modelo ter� d=1, D=1 

### Aplicando a Diferen�a Sazional 
    # Como se trata de uma s�rie mensal -> s = 12

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


#################### Definindo valores dos par�metros ####################

# p = 1            --> ACF Decaindo  r�pido e PACF com quebra no lag 1
# q = 0 ou q = 1   --> PACF Decaindo r�pido e ACF com uma quebra no lag 1, mas n�o muito expressiva.
# P = 0            --> ACF e PACF sem autocorrela��es nos lags sazonias.
# Q = 0            --> ACF e PACF sem autocorrela��es nos lags sazonias.


### Modelos Candidatos
# --> SARIMA(1,1,0) x (0,1,0)
# --> SARIMA(1,1,1) X (0,1,0)

# SARIMA(1,1,0) x (0,1,0)
SARIMA_1 <- arima(D12DX, order = c(1,1,0), seasonal = c(0,1,0), include.mean = FALSE)

# R�siduos
E <- SARIMA_1$residuals

### An�lise Visulal
par(mfrow = c(2,2))
plot(E)
acf(E)
pacf(E)
qqnorm(E)
qqline(E)

### Testes estat�sticos
# Estacionaridade
kpss.test(E)   # Estacionaria 

# Independ�ncia
Box.test(E, lag = 20, type = 'Ljung-Box', fitdf = 4) # H0 -> n�o s�o independentes

# Normalidade
shapiro.test(E) # H0 -> ind�cios de n�o normalidade.


# ------------------------


# SARIMA(1,1,1) x (0,1,0)
SARIMA_2 <- arima(D12DX, order = c(1,1,1), seasonal = c(0,1,0), include.mean = FALSE)

# R�siduos
E <- SARIMA_1$residuals

### An�lise Visulal
par(mfrow = c(2,2))
plot(E)
acf(E)
pacf(E)
qqnorm(E)
qqline(E)

### Testes estat�sticos
# Estacionaridade
kpss.test(E)   # Estacionaria 

# Independ�ncia
Box.test(E, lag = 20, type = 'Ljung-Box', fitdf = 4) # H0 -> n�o s�o independentes

# Normalidade
shapiro.test(E) # H0 -> ind�cios de n�o normalidade.


#### Comparativo

SARIMA_1
SARIMA_2

# Sarima 2 obteve um melhor AIC, portanto usaremos ele.
#### OBS: Era pra ter ficado melhor, os pontos da qqnorm n�o est�o em cima da reta como deveriam.