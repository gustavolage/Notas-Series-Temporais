

####################################################
############# Teste de Estacionaridade #############
####################################################


##### Hípoteses Usuais:

## * H0: O processo é estacionario
## * H1: O processo possui raiz unitária ou possui tendência

  # Raiz Unitária: Processos em que o valor 1 é raiz do ploninômio
    # Exemplo: 
    # - Passeio Aleatório: x_t = x_[t-1] + E_t
    #   PHI(B) = 1 - B -> B=1 é raiz, logo esse processo tem raiz unitária

    # - ARIMA(p,d,q), d>1: PHI_p(B)*(1-B)^d * x_t =  THETA(B)*E_t
    #   B=1 é raiz de PHI_p(B)*(1-B)^d, logo esse processo tem raiz unitária.


## Testes - Presetes no pacote "tseries"
  # Teste KPSS -> kpss.test()        
      # Description: (KPSS) test for the null hypothesis that x is level or trend stationary.

  # Teste de Phillip - Perron -> pp.test()
      # Description:  Computes the Phillips-Perron test for the null hypothesis that x has a unit root.

  # Teste ADF (Dickey DA and Fuller WA) -> adf.test()
      # Description: Computes the Augmented Dickey-Fuller test for the null that x has a unit root.

?kpss.test
?pp.test
?adf.test

# Vamos Simular um Modelo com tendencia ARIMA(1,1,1)

  # Polinomio Caracteristico AR: PHI(B) = 1 - 0.7B    -> phi_1 = 0.7
                # raize: x = 1,42 ; -> Estacionario

  # Polinomio Caracteristico MA: THETA(B) = 1 + 0.6B    -> theta_1 = 0.6
                # raiz: x = 1.66 -> É inversível.

# ARIMA: PHI_p(B)*(1-B)^d * x_t =  THETA(B)*E_t <-> (1- 0.7B)*(1-B)^1*X_t = (1+0.6B)*E_t

n = 200
E <- rnorm(n, 0, 3)
X <- numeric(n)
X[1] = 0

for (i in 3:n){
  X[i] = 0.7*X[i-1] + E[i] + 0.6*E[i-1]
}

plot.ts(X)


######### Comparando com função arima pronta

n = 200
E <- rnorm(n, 0, 3)
X <- numeric(n)
X[1] = X[2] = X[3]= 0
for (i in 4:n){
  X[i] = 0.6*X[i-1] + -0.7*X[i-2] + 0.2*X[i-3] + E[i]
}

plot.ts(X)

y = arima.sim(n=200,model=list(ar=c(0.6,-0.7,0.2)))
plot.ts(y)


X <- AirPassengers

library(tseries)
library(dplyr)

kpss.test(X)
pp.test(X)
adf.test(X)

library(forecast)
X %>% ndiffs()
diff(X) %>% ndiffs()

a <- diff(X)
adf.test(a)
?adf.test # Computes the Augmented Dickey-Fuller test for the null that x has a unit root.


