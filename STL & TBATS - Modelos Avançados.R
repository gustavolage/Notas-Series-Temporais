#############################################################################
################ Modelos Avan�ados  - Sazionalidade Multipla ################
#############################################################################

rm(list = ls(all = T))


########## Decomposi��o STL: Sazionalidade, Tend�ncia e Loess ###########

# Vantagens em rela��o ao m�todo cl�ssico:
    # Tend�ncia via Loess ( regress�o din�mica )
    # Sazionalidade Din�mica
    # Usualmente fornece melhores resultados que a decomopsi��o cl�ssica


###### Decomposi��o MSTL: Multiplas Sazionalidade, Tend�ncia e Loess ########

    # Faz multiplas aplica��es da decomposi��o STL
    # no pacote _forecast_ do R, mstl()


require(forecast)

### Exemplos:
require(fpp2)

## --> gasoline

plot(mstl(gasoline))
 # Temos que h� uma sazionalidade de freq = 52.18 ( Semanal -> 365.25/7)

## --> calls ( Liga��es a um banco americano de 5 em 5 min ( 07:00 at� 21:05 ) )

plot(calls)
plot(window(calls, end = 4))
plot(mstl(window(calls, end = 4)))
# Temos que h� uma sazionalidade de m1 = 169 ( Di�ria -> (21:05-07:00)*60/5) e m2 = 845 (Semanal = 169*5 (dias uteis) )


### Previs�o

# Fun��o stlf()
  
    # Retira a sazionalidade via MSTL
    # Faz previs�es com sazionalidade ajustada utilizando ETS
    # Inclui sazionalidade no �ltimo ciclo

y <- window(calls, end = 4)

plot(stlf(y, h = frequency(y)))

# Modelo ETS(A,N,N) -> erro Additive, trend Null, Seaseonal Null (sempre, j� que � retirada na fun��o stl)



########## Modelo BATS ###########

    # B: transforma��o de *Box-Cox para heterogeneidade
    # A: modelo *ARMA para erros 
    # T: *Tend�ncia via alisamento exponencial
    # S: *Sazionalidade multipla via alisamento Exponencial

# Problema: Para s�ries com sazionalidade complexa ( m != int, ex: m = 52.18 (semanal) )

# Para resolver isso:

### Decomposi��o em s�rie harm�nica (S�ries de Fourier) 

    # Id�ia: a sazionalidade pode ser vista como uma sobreposi��o de s�ries harm�nicas( senos, cossenos )
    # Aplica-se transforma��o de Fourier
    # Permite que sazionalidades m�ltiplas sejam modeladas
    # Permite que ciclos n�o inteiros sejam modelados.



###################################
########## Modelo TBATS ###########
###################################

    # T: termos *Trigonom�tricos para sazionalidade
    # B: transforma��o de *Box-Cox para heterogeneidade
    # A: todelo *ARMA para erros 
    # T: *Tend�ncia via alisamento exponencial
    # S: *Sazionalidade multipla via alisamento Exponencial

## OBS:
  # O modelo TBATS tamb�m pode ser escrito como modelo de espa�o de estado n�o linear
  # A verossimilhan�a � conhecida
  # Previs�o pontual e intervalar podem ser feitas via bootstrap

fit <- tbats(gasoline)
plot(fit)

### Previs�o
plot(forecast(fit, h = 104))  # dois anos de previs�o 

# TBATS(1, {0,0}, -, {<52.18 , 12>})

    # 1 --> Transforma��o Box-Cox (omega = 1)
    # {0,0} --> ARMA NULO
    # - --> N�o usou o par�metro Damped
    # {<52.18 , 12>} --> um ciclo sazional com m=52.18 e com 12 equa��es harm�nicas


fit2 <- tbats(y)
plot(fit2)

### Previs�o
plot(forecast(fit2, h = frequency(y)))  # Uma semana de previs�o
     
     