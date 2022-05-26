library(forecast)
library(urca)
library(fpp2)
library(tseries)
library(zoo)
library(stats)
library(lmtest)
library(gridExtra)
library(xts)
library(aTSA)
library(psych)
library(ggpubr)


#zamiana danych na szereg czasowy i logarytmowanie
x<- xauusd
gold_ts <- ts((log(x$Zamkniecie)), start = c(2003,2), frequency = 12)

# Wizualizacja danych na wykresie i opis danych
x11()
plot(gold_ts,
     type="l",
     lwd=2, # podwojna grubosc
     ylab="Price",
     main="Logarytmiczne ceny zamkniecia XAU/USD")

# wykres sezonowosci
seasonplot(gold_ts,
           col=rainbow(12),
           year.labels=TRUE,
           pch=19,
           main="Wykres sezonowosci: GOLD",
           xlab="Miesiace")

# statystyki opisowe
summary(gold_ts)
describe(gold_ts)

#Podzial danych na zbior uczacy i testowy
gold_train <- ts((gold_ts[1:161]),start = c(2003,2), frequency = 12)
  gold_test <- ts(gold_ts[162:215],start = c(2016,7), frequency = 12)

plot(gold_train,main="Logarytmiczne ceny XAU/USD", ylab="Price",xlim = c(2003,2021), ylim=c(5.8,7.8),lwd=2)
lines(gold_test,col="green",lwd=2)

#Autokorelacja
ggAcf(gold_train, lag=30)+ ggtitle("Korelogram - GOLD") + xlab("Opoznienie") + theme_light() + 
  theme(plot.title = element_text(hjust=0.5))

# Sprawdzenie autokorelacji za pomoca testu statystycznego
# H0: Brak autokorelacji
Box.test(gold_train, lag=30, type="Ljung-Box")
p_value <- 1:30
for(i in 1:30){
  p_value[i] <- Box.test(gold_train, lag=i, type = "Ljung-Box")$p.value
}
plot(p_value)
p_val

#Badanie Stacjonarnosci
aTSA::adf.test(gold_train)

#roznicowanie zmiennych
diff_gold <- diff(gold_train)

plot(diff_gold,
     lwd=2,
     main = "Pierwsze różnice miesiecznych cen XAU/USD",
)

#Test ADF zroznicowanych zmiennych
adf.test(diff_gold)

#Korelogram
x11()
ggAcf(diff_gold, lag=35)+ ggtitle("Korelogram - GOLD") + xlab("Opoznienie") + theme_light() + 
  theme(plot.title = element_text(hjust=0.5))

tsdisplay(diff_gold)

#Test Ljung-Boxa
Box.test(diff_gold, lag=30, type="Ljung-Box") 
for(i in 1:30){
  p_val[i] <- Box.test(diff_gold, lag=i, type = "Ljung-Box")$p.value
}

plot(p_val, main="p_value")
df_pval <- data.frame(p_val.gold_diff=p_val)



#MODELOWANIE parametrow modelu ARIMA - ACF & PACF
x11()
par(mfrow = c(2,1))
Acf(diff_gold,lwd = 2.5, col = "dark green",  lag.max = 30)
Pacf(diff_gold,lwd = 2.5, col = "dark green", lag.max = 30)

#Budowa modeli
arima111.drift <- Arima(gold_train, order = c(1,1,1),include.drift = T)
arima011.drift <- Arima(gold_train, order = c(0,1,1),include.drift = T)
arima110.drift <- Arima(gold_train, order = c(1,1,0),include.drift = T)
arima111 <- Arima(gold_train, order = c(1,1,1))
arima011 <- Arima(gold_train, order = c(0,1,1))
arima110 <- Arima(gold_train, order = c(1,1,0))
arima112 <- Arima(gold_train, order = c(1,1,2))
arima211 <- Arima(gold_train, order = c(2,1,1))
arima212 <- Arima(gold_train, order = c(2,1,2))

#Badanie modeli i wybor najlepszego z nich na podstawie kryt. inf

# Porownanie wartosci AIC dla  modeli.
AIC(arima111.drift,arima011.drift,arima110.drift,arima111,arima011,arima110,arima112,
    arima211, arima212)
# Porownanie wartosci BIC dla  modeli
BIC(arima111.drift,arima011.drift,arima110.drift,arima111,arima011,arima110,arima112,
    arima211, arima212)


#Diagnostyka modelu
# czy reszty sa bialym szumem
Acf(resid(arima011.drift), lag.max = 36,
    lwd = 2, col = "dark green")
Pacf(resid(arima011.drift), lag.max = 36,
     lwd = 2, col = "dark green")

# Test Ljung-Boxa dla reszt
checkresiduals(arima011.drift)
p_val_residuals <- 1:30
for(i in 1:30){
  p_val_residuals[i] <- Box.test(resid(arima011.drift), lag=i, type = "Ljung-Box")$p.value
}
plot(p_val_residuals)
as.data.frame(p_val_residuals) 

Box.test(resid(arima011.drift), type = "Ljung-Box", lag = 30)
Box.test(resid(arima110.drift), type = "Ljung-Box", lag = 30)
Box.test(resid(arima011), type = "Ljung-Box", lag = 30)
Box.test(resid(arima110), type = "Ljung-Box", lag = 30)



#############
# Prognoza  #
#############
forecast_arima011.drift <- forecast::forecast(arima011.drift, h=54)
forecast_arima110.drift <- forecast::forecast(arima110.drift, h=54)
forecast_arima011 <- forecast::forecast(arima011, h=54)
forecast_arima110 <- forecast::forecast(arima110, h=54)
forecast_arima111 <- forecast::forecast(arima111, h=54)
forecast_arima111.drift <- forecast::forecast(arima111.drift, h=54)


plot(forecast_arima011.drift,lwd=2)
lines(gold_test, col="brown",lwd=2)

lines(gold_train, col="red")
plot(gold_ts)


#Ocena prognozy - bledy
kryteria<-c("MAE", "RMSE", "MAPE", "MASE")
accuracy(forecast_arima011.drift)[,kryteria]
accuracy(forecast_arima110.drift)[,kryteria]
accuracy(forecast_arima011)[,kryteria]
accuracy(forecast_arima110)[,kryteria]


#Ocena prognozy na zbiorze testowym
accuracy(forecast_arima011.drift, gold_test)[,kryteria]
accuracy(forecast_arima111.drift, gold_test)[,kryteria]
accuracy(forecast_arima110.drift, gold_test)[,kryteria]
accuracy(forecast_arima011,gold_test)[,kryteria]
accuracy(forecast_arima111,gold_test)[,kryteria]
accuracy(forecast_arima110,gold_test)[,kryteria]


