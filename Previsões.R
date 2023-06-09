###########################################################################
###################### Previs�o Pontual e Intervalar ######################
###########################################################################

# Vimos que o melhor modelo para os dados AirPassanger � o SARIMA(1,1,1)(0,1,0)


fit <- arima(x = AirPassengers, order = c(1,1,1), seasonal = c(0,1,0))
library(dplyr)
fit %>% predict(n.ahead = 12)  # 1 ano de previs�o


# O pacote forecast permite calcular a regi�o intervalar 
require(forecast)
fit %>% forecast(h = 12, level = 95) %>% plot()  # 1 ano de previs�o com 95% conf.


# Do pacote forescast tamb�m tem o auto.arima
fit.auto_arima <- auto.arima(AirPassengers)
fit.auto_arima %>% forecast(h=12, level=95) %>% plot()
