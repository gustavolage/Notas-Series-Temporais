############ Séries Temporais

##### Análise e Decomposição de uma ST

rm(list=ls(all=TRUE))


##### Dados

X <- UKgas <- UKgas
usad <-USAccDeaths

## Pacotes

library(magrittr)
library(ggplot2)
library(reshape2)

plot(UKgas)
#plot(usad)

##### Formas de Extração da Tendência

# Regressão linear

tempo = time(X)
rl = lm(X ~ tempo)

plot(X, 
     type = 'l',
     lwd = 1.5,
     col = 1) 

lines(x = tempo,
      y = rl$fitted,
      type='l',
      col = 2,
      lwd = 2,
      lty = 2)                                                    

# Polinomial

out1 = lm(X ~ poly(tempo, 2))
out2 = lm(X ~ poly(tempo, 6))

plot(X, 
     type = 'l',
     lwd = 1.5,
     col = 1) 
lines(x = tempo,
      y = out1,
      type = 'l',
      lty = 'dashed',
      col = 'blue')

# Médias Moveis


sma4 <-filter(X, rep(1/4, 4), sides = 1)      # Unilateral
sma7 <- filter(X, rep(1/7, 7), sides = 2)   # Bilateral


plot(X, 
     type = 'l',
     lwd = 1.5,
     col = 1) 
lines(x = tempo,
      y = sma4,
      type='l',
      col = 2,
      lwd = 2,
      lty = 2)
lines(x = tempo,
      y = sma7,
      type='l',
      col = 'blue',
      lwd = 2,
      lty = 2)

######################
#### Decomposição ####
######################

dec = decompose(X, type = 'a')
plot(dec)

dec.multiplicativa = decompose(X, type = 'm')
plot(dec.multiplicativa)


######################
####### STL ##########
######################

# usar s.window >= 7
# s.window ímpar

stl(X, s.window = 7) %>% plot( main = 's.window = 7')  
stl(X, s.window = 9) %>% plot( main = 's.window = 9')
stl(X, s.window = 11) %>% plot( main = 's.window = 11')
stl(X, s.window = 21) %>% plot( main = 's.window = 21')

stl(X, s.window = 'periodic', t.window = NULL, robust = TRUE) %>% plot( main = 's.window = periodic')

# robust -> desconsiderar outliers

## Queremos ajustar os PARAMETROS para chegarmos em um RUIDO BRANCO

############################################
####### Multiplicativa usando LOG ##########
############################################


logX <- log(X)
stl(logX, s.window = 'periodic') %>% plot( main = 'log X')



#########################################
####### MSLT (Pacote forecast) ##########
#########################################

# - Contem o argumento lambda, para aplicação da transformação BoxCox 
# (transforma a distribuição das observaçõe em  uma Normal)

# - Boa para identificar dupla sazionalidade (ex: Intraday e Mensal)

library(forecast)
library(fpp2)

# Dados
calls <- calls

plot(calls)

#SLT
stl(calls, s.window = 7) %>% plot(main = ' s.window = 7')

stl(calls, s.window = 11) %>% plot(main = ' s.window = 11')

stl(calls, s.window = 'period') %>% plot(main = ' s.window = period')

window(calls, end = 4) %>% stl(calls, s.window = 'period') %>% plot(main = ' s.window = period')

#MSLT
mstl(calls, lambda = 'auto') %>% plot(main = 'mstl BoxCox auto')

mstl(calls, lambda = NULL) %>% plot(main = 'mstl BoxCox Null')


window(calls, end = 4) %>% mstl(calls, lambda = 'auto') %>% plot(main = 'mstl BoxCox auto')



