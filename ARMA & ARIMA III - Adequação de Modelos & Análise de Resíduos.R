######################################################################
############# Aderqua��o do Modelo & An�lise de Res�duos #############
######################################################################

#### An�ises Visuais dos res�duos

## M�dia

par(mfrow = c(2,1))
plot.ts(diff(UKgas), main = 'Correto')
plot.ts(UKgas, main = 'Incorreto')

# Podemos ver que no primeiro gr�fico a m�dia do processo varia no tempo.

# ----------------

########### Criando uma s�rie Composta por SOMA ############

n <- 200   # tamanho

X <- 1000 + 0.1*(1:n)    # tend�ncia
plot.ts(X)

S <- 20*sin((2*pi/12)*(1:n))    # cilica
plot.ts(X+S)


E <- rnorm(n, 0, 5)   # Ru�do Branco
Z <- X+S+E
plot.ts(Z)


## Vari�ncia

par(mfrow = c(2,1))
plot.ts(Z, main = 'Correto')
plot.ts(UKgas, main = 'Incorreto')

# Podemos ver que no primeiro gr�fico a vari�ncia do processo varia no tempo.


# Autocorrela��o

n = 1000
E <- rnorm(n)
plot.ts(E)

par(mfrow = c(2,1))
acf(E, main = 'Correto')
acf(Z, main = 'Incorreto')


# -----------------

## Ader�ncia

par(mfrow = c(1,2))
qqnorm(E, main = 'Correto')
abline(0,1)
qqnorm(UKgas, main = 'Incorreto')
abline(0,1)

# Podemos ver que no primeiro gr�fico, os res�duos seguem uma distribui��o normal,
# por isso vemos os pontos sobrepostos na reta, diferente do segundo gr�fico em
# que os pontos fazem curva, por termos observa��es correlacionadas.




#############################################################
################## Testes de Independ�ncia ##################
#############################################################



### Hip�teses:
      # H0: ro(1) = ro(2) = ... = ro(m) = 0
      # H1: pelo menos um diferente de 0

      # Teste de Box-Pierce
      # Teste de Ljung-Box    #BEST
      # Function: Box.test()
        # obs: m > p + q   -> geralmente escolhe-se m ~ 15

      ## Compute the Box--Pierce or Ljung--Box test statistic for examining
      ## the null hypothesis of independence in a given time series. 
      ## These are sometimes known as 'portmanteau' tests.

### Testes de Ader�ncia:

  ## Normalidade
      # Teste de Shapiro-Wilk
      # shapiro.test()

  ## Outras Distribui��es
      # Teste de Kolmogov-Smirnov
      # ks.test()


### Parcim�nia
# Os crit�rios de aprcim�nia fazem um balan�o entre a qualidade do ajuste e 
# o grau de complexidade do modelo

  # Cr�terio de informa��o de Akaike
      # AIC = 2k - 2 log( L(^B, ^sigma^2) )
    # em que, k denota o n�mero de par�metros do modelo e L(^B, ^sigma^2) a 
    # fun��o de verossimilha�a ajustada.


  # AIC corrigido para pequenas amostras
      # AIC = AIC + (2k^2 + 2k)/(n - k - 1)
 
  ## em ambos os casos deve-se escolher o modelo com menor valor do cr�terio


rm(list = ls(all=TRUE))
dev.off()

### Botando em pr�tica
set.seed(5)
n = 1100
X <- arima.sim(n, model = list(order = c(1,1,1), ar = 0.6, ma = 0.5),
               function(n) rnorm(n, mean = 0, sd = 3))

library(forecast)
ndiffs(X, alpha = 0.05) 

W <- diff(X, differences = 1)

## An�lise Visual
windows()
par(mfrow = c(3,1))
plot.ts(W)
acf(W)
pacf(W)


## Testes

# Estacionaridade
library(tseries)
kpss.test(W)   # Estacionaria (p-value>0.05 -> is level or trend stationary.)

# Independ�ncia
Box.test(W, lag = 20, type = 'Ljung-Box', fitdf = 2) # Usar fitdf = p + q
# rejeitamos H0 -> s�o dependentes

# Normalidade
shapiro.test(W) # Rejeitamos H0 -> ind�cios de n�o normalidade.
#qqnorm(W)
#abline(0,1)



############################################################
############## Ajustando e Comparando Modelos ##############
############################################################


############ Modelo AR(4) para W_t


fit.ar = arima(W, order = c(4,0,0), include.mean = FALSE)

E <- fit.ar$residuals

## An�lise Visual

windows()
par(mfrow = c(2,2))
plot(E)
qqnorm(E)
abline(0,3)
acf(E)
pacf(E)


## Testes

# Estacionaridade
kpss.test(E)   # Estacionaria (p-value>0.05 -> is level or trend stationary.)

# Independ�ncia
Box.test(E, lag = 20, type = 'Ljung-Box', fitdf = 4) # H0 -> s�o independentes

# Normalidade
shapiro.test(E) # H0 -> ind�cios de normalidade.



############ Modelo MA(5) para W_t


fit.ma = arima(W, order = c(0,0,5), include.mean = FALSE)

E <- fit.ma$residuals

## An�lise Visual

windows()
par(mfrow = c(2,2))
plot(E)
qqnorm(E)
abline(0,3)
acf(E)
pacf(E)


## Testes

# Estacionaridade
kpss.test(E)   # Estacionaria (p-value>0.05 -> is level or trend stationary.)

# Independ�ncia
Box.test(E, lag = 20, type = 'Ljung-Box', fitdf = 5) # Usar fitdf = p + q
# rejeitamos H0 -> s�o independentes

# Normalidade
shapiro.test(E) # H0 -> ind�cios de normalidade.



############ Modelo ARMA(1,1) para W_t


fit.ar4_ma5 = arima(W, order = c(1,0,1), include.mean = FALSE)

E <- fit.ar4_ma5$residuals

## An�lise Visual

windows()
par(mfrow = c(2,2))
plot(E)
qqnorm(E)
abline(0,3)
acf(E)
pacf(E)


## Testes

# Estacionaridade
kpss.test(E)   # Estacionaria (p-value>0.05 -> is level or trend stationary.)

# Independ�ncia
Box.test(E, lag = 20, type = 'Ljung-Box', fitdf = 4) # H0 -> s�o independentes

# Normalidade
shapiro.test(E) # H0 -> ind�cios de normalidade.


#######################################################################
################## Avaliando o Melhor Modelo por AIC ##################
#######################################################################

fit.ar
fit.ma
fit.ar4_ma5

# Se fomos olhar para o sigma� o melhor modelo seria o AR(4), por�m a complexidade 
# deste � maior que a do modelo ARMA(1,1), e por conta disso o melhor(menor)
# valor do AIC foi o do modelo ARMA, assim sendo nosso modelo escolhido. 