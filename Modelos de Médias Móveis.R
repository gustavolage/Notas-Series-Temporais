############### MODELOS MA(q) ####################


# Inversibilidade: O modelo MA(q) é inversível se as raízes do polinômio 
# característico de médias móveis phi(B) = 1 + theta_1(B) + theta_2(B)^2 + ... + theta_q(B)^q
# estão fora do circulo unitário, isto é, todas as raízes são maiores que 1 em valor absoluto.

## O modelo MA(q) pode ser ecrito como um modelo AR(inf) 

## OBS: Modelos MA(q) são sempre estacionários

## FAC: decai progressivamente sem corte a partir de q

## FACP: decai para zero sem cortes
### - Uma vez que qualque modelo MA(q) inversível pode ser escrito como um modelo AR(inf)

# 1 - Modelo MA(1): X_t = e_t + 0,7*e_[t-1], com epsilon ~ N(0,9)

n = 300
E = rnorm(n, 0, 3)
X <- numeric(n)
X[1] <- 0
for (i in 2:n){
  X[i] = E[i] + 0.7*E[i-1]
}

par(mfrow = c(1,3))
plot.ts(X)
acf(X)
pacf(X)

# ACF com apenas 1 lag significante e FACP decai sem cortes


# 2 - Modelo MA(1): X_t = e_t - 0,7*e_[t-1], com epsilon ~ N(0,9)

n = 300
E = rnorm(n, 0, 3)
X <- numeric(n)
X[1] <- 0
for (i in 2:n){
  X[i] = E[i] - 0.7*E[i-1]
}

par(mfrow = c(1,3))
plot.ts(X)
acf(X)
pacf(X)

# ACF com apenas 1 lag significante e FACP decai sem cortes


# 3 - Modelo MA(2): X_t = e_t + 0,5*e_[t-1] - 0,4*e[t-2], com epsilon ~ N(0,9)

n = 300
E = rnorm(n, 0, 3)
X <- numeric(n)
X[1] <- 0
X[2] <- 0 
for (i in 3:n){
  X[i] = E[i] + 0.5*E[i-1] - 0.4*E[i-2]
}

par(mfrow = c(1,3))
plot.ts(X)
acf(X)
pacf(X)

# ACF tem 2 lags significativos e PACF bagunçado