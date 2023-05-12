#############################################################################
################ Modelos Avançados  - Sazionalidade Multipla ################
#############################################################################

rm(list = ls(all = T))


########## Decomposição STL: Sazionalidade, Tendência e Loess ###########

# Vantagens em relação ao método clássico:
    # Tendência via Loess ( regressão dinâmica )
    # Sazionalidade Dinâmica
    # Usualmente fornece melhores resultados que a decomopsição clássica


###### Decomposição MSTL: Multiplas Sazionalidade, Tendência e Loess ########

    # Faz multiplas aplicações da decomposição STL
    # no pacote _forecast_ do R, mstl()


require(forecast)

### Exemplos:
require(fpp2)

## --> gasoline

plot(mstl(gasoline))
 # Temos que há uma sazionalidade de freq = 52.18 ( Semanal -> 365.25/7)

## --> calls ( Ligações a um banco americano de 5 em 5 min ( 07:00 até 21:05 ) )

plot(calls)
plot(window(calls, end = 4))
plot(mstl(window(calls, end = 4)))
# Temos que há uma sazionalidade de m1 = 169 ( Diária -> (21:05-07:00)*60/5) e m2 = 845 (Semanal = 169*5 (dias uteis) )


### Previsão

# Função stlf()
  
    # Retira a sazionalidade via MSTL
    # Faz previsões com sazionalidade ajustada utilizando ETS
    # Inclui sazionalidade no último ciclo

y <- window(calls, end = 4)

plot(stlf(y, h = frequency(y)))

# Modelo ETS(A,N,N) -> erro Additive, trend Null, Seaseonal Null (sempre, já que é retirada na função stl)



########## Modelo BATS ###########

    # B: transformação de *Box-Cox para heterogeneidade
    # A: modelo *ARMA para erros 
    # T: *Tendência via alisamento exponencial
    # S: *Sazionalidade multipla via alisamento Exponencial

# Problema: Para séries com sazionalidade complexa ( m != int, ex: m = 52.18 (semanal) )

# Para resolver isso:

### Decomposição em série harmônica (Séries de Fourier) 

    # Idéia: a sazionalidade pode ser vista como uma sobreposição de séries harmônicas( senos, cossenos )
    # Aplica-se transformação de Fourier
    # Permite que sazionalidades múltiplas sejam modeladas
    # Permite que ciclos não inteiros sejam modelados.



###################################
########## Modelo TBATS ###########
###################################

    # T: termos *Trigonométricos para sazionalidade
    # B: transformação de *Box-Cox para heterogeneidade
    # A: todelo *ARMA para erros 
    # T: *Tendência via alisamento exponencial
    # S: *Sazionalidade multipla via alisamento Exponencial

## OBS:
  # O modelo TBATS também pode ser escrito como modelo de espaço de estado não linear
  # A verossimilhança é conhecida
  # Previsão pontual e intervalar podem ser feitas via bootstrap

fit <- tbats(gasoline)
plot(fit)

### Previsão
plot(forecast(fit, h = 104))  # dois anos de previsão 

# TBATS(1, {0,0}, -, {<52.18 , 12>})

    # 1 --> Transformação Box-Cox (omega = 1)
    # {0,0} --> ARMA NULO
    # - --> Não usou o parâmetro Damped
    # {<52.18 , 12>} --> um ciclo sazional com m=52.18 e com 12 equações harmônicas


fit2 <- tbats(y)
plot(fit2)

### Previsão
plot(forecast(fit2, h = frequency(y)))  # Uma semana de previsão
     
     