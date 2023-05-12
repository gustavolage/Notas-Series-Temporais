#########################################################################
############ Simulando e Ajustando um processo ARIMA(p,d,q) ############
#########################################################################


# Um ARIME(p,d,q) por der simulado em R utilizando a função
#     arima.sim(n, model)
# Por exemplo:

x <- arima.sim(n = 1000, model = list(order = c(1,1,1), ar = 0.6, ma = 0.5))
# Simula o modelo ARIMA(1,1,1)
# W_t = X_t - X_[t-1]
# W_t = 0.6*W_[t-1] + E_t + 0.5*E_t-1     E_t ~ N(0,1)

# Para definir um valor diferente de sigma, por exemplo, sigma2 = 5:

y <- arima.sim(n = 1000, model = list(orer = c(1,1,1), ar = 0.6, ma = 0.5),
               rand.gen = function(n) rnorm(n,0,sqrt(5)))

par(mfrow = c(2,1))
plot.ts(x)
plot.ts(y)




# Da mesma forma, podemos ajustar um modelo ARIMA, via estimador de minimos quadrados
#     arima(p,d,q)

arima(x, order = c(1,1,1), include.mean = FALSE)
arima(y, order = c(1,0,1), include.mean = FALSE)


# A função ndiff(), do pacote forecast, esima o numero de diferenciações 
# necessárias, para tornar nossa série estacionária

# Modelo ARMA(1,1)
x <- arima.sim(n=500, model = list(order = c(1,0,1), ar = 0.7, ma = 0.6))

# Modelo ARIMA(1,1,1)
y <- arima.sim(n=500, model = list(order = c(1,1,1), ar = 0.7, ma = 0.6))

# Modelo ARIMA(1,1,1)
z <- arima.sim(n=500, model = list(order = c(1,2,1), ar = 0.7, ma = 0.6))

par(mfrow = c(3,1))
plot.ts(x)
plot.ts(y)
plot.ts(z)


# Testes
library(forecast)
ndiffs(x, alpha = 0.05, test = "kpss", max.d = 2)

ndiffs(y, alpha = 0.05, test = "kpss", max.d = 2)  # <- d =1
ndiffs(y, alpha = 0.05, test = "pp", max.d = 2)  # <- d =0
ndiffs(y, alpha = 0.05, test = "adf", max.d = 2)   # <- d =0

ndiffs(z, alpha = 0.05,  test = 'kpss')

# Comparando testes

kpss.test(y)
pp.test(y)
adf.test(y)

#?kpss.test
#?pp.test
#?adf.test



#####################################################
############ Redundância dos Parâmetros #############
#####################################################

set.seed(8675309)
x <- rnorm(150, mean=5) # gerate iid N(5,1)

### Era de se esperar que a estimativa fosse ar1 = 0 e ma1 = 0, no entanto
### a função de estimação se perde e encontra
### ar1 = -0.959  &  ma1 = 0.952

arima(x, order=c(1,0,1)) # estimation ARMA

### Podemos notar que a proximidade dos parâmetros ar1 e ma1 indica que 
### que o modelo está super parametrizado, pois o fatores quase podem ser
### cancelados na froma polinomial: (1 + 0.959) X_t =  (1 - 0.952) E_t

### Podemos ver também o tamanho o erro padrão do estimador

### Neste caso, os gráficos de correlação, mostram que 
### NÃO existem correlação nas defasagens

ndiffs(x, test = "kpss")

par(mfrow = c(3,1))

plot.ts(x)
acf(x)
pacf(x)
