
######################################
########## �nalise dos lags ##########
######################################

##########################
####### Modelos AR(p)#####

# Poodemos estimar o valor p de uma series AR pela sua fun��o ACF e principalmente pela PACF
# Para isso, precisamos que a serie seja estacion�ria



###################################
######### Estacion�ridade #########
###################################

# - Def: Temos que um processo estoc�stiico � estacion�rio se a m�dia ao longo deste se mant�m igual, 
# assim como suas caract�risticas de vari�ncia e cov�riancia em h defasagens

# As ra�zes do polin�mio caracter�stico devem ser maior do que 1 em m�dulo.

# Os valores de Phi(ctes) devem ser menores do que 1 em valor absoluto.



# 1 - Modelo AR(2): X_t = 0.5X_t-1 +0.4X_t-2 + epsilon_t, com epsilon ~ N(0,9)

# Polinomio caracter�stico: phi(B) = 1 - 0.5B - 0.4B^2

# Raizes do polinomio: B1 = 1.075, B2 = -2.325

# Resultado: Como |B1| > 1, |B1| > 2 -> esse processo � estacion�rio.


n = 200
X = numeric()
X[1:2] = 0

for(i in 3:n){
  X[i] = 0.5*X[i-1] +0.4*X[i-2] + rnorm(1,0,3)
}

par(mfrow=c(1,3))
plot.ts(X)
acf(X,10)
pacf(X,10)

# Como esperado, a ACF decresce amortecidamente, por ser estacionaria e a PACF tem 2 lags de significancia 



# 2 - Serie real

ap <- AirPassengers

par(mfrow=c(1,1))
plot.ts(ap)

library(magrittr)

ap_diff <- diff(ap)


dec = decompose(ap_diff, type = 'a')
plot(dec)


par(mfrow=c(1,3)) 
plot.ts(ap_diff) 
acf(ap_diff,10) 
pacf(ap_diff,10)

# N�o teve um bom desempenho


