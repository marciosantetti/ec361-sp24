library(tidyverse)
library(fpp3)
library(hrbrthemes)
library(scales)
library(ggeasy)
library(MetBrewer)
library(janitor)
library(patchwork)
library(broom)


theme_set(theme_ipsum())



#--------------------------------------------------------------------------------



##--- Total vehicle sales (TOTALNSA) - [https://fred.stlouisfed.org/series/TOTALNSA]



car <- read_csv("TOTALNSA.csv") |> 
  clean_names() |> 
  rename(cars = totalnsa)


car_ts <- car |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date)


car_ts |> 
  autoplot(log(cars))

#--------------------------------------------------------------------------------


#-- Stationarity:

car_ts |> 
  features(cars, unitroot_kpss)





#--------------------------------------------------------------------------------


#-- Fourier Modeling:


car_fit <- car_ts |> 
  model(`K = 1` = ARIMA(log(cars) ~ fourier(K=1) + PDQ(0,0,0)),
        `K = 2` = ARIMA(log(cars) ~ fourier(K=2) + PDQ(0,0,0)),
        `K = 3` = ARIMA(log(cars) ~ fourier(K=3) + PDQ(0,0,0)),
        `K = 4` = ARIMA(log(cars) ~ fourier(K=4) + PDQ(0,0,0)),
        `K = 5` = ARIMA(log(cars) ~ fourier(K=5) + PDQ(0,0,0)),
        `K = 6` = ARIMA(log(cars) ~ fourier(K=6) + PDQ(0,0,0)))

car_fit |> 
  forecast(h = 24) |> 
  autoplot(car_ts, level = 95) +
  facet_wrap(vars(.model), ncol = 2)

car_fit |> 
  glance() |> 
  arrange(AICc)

car_fit |> 
  select(`K = 6`) |> 
  augment() |> 
  ggplot(aes(x = date, y = cars)) +
  geom_line(aes(color = "Cars sold")) +
  geom_line(aes(y = .fitted, color = "Fitted values"))


car_fit |> 
  select(`K = 6`) |> 
  augment() |> 
  features(.innov, ljung_box, lag = 2 * 12, dof = 2)



#---------------------------------------------------------------------------


ffunds <- read_csv("FEDFUNDS.csv") |> 
  clean_names() 

car <- car |> 
  add_column(fedfunds = ffunds$fedfunds)


car_ts <- car |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date)


car_ts |> 
  autoplot(fedfunds)


#---------------------------------------------------------------------------


#-- Regression:

car_ts |> 
  features(fedfunds, unitroot_kpss)

car_ts |> 
  features(cars, unitroot_kpss)

car_ts |> 
  features(difference(fedfunds, lag = 1), unitroot_kpss)

car_ts |> 
  features(difference(cars, lag = 1), unitroot_kpss)


car_ts <- car_ts |> 
  mutate(dcars = difference(cars, lag = 1),
         dfedfunds = difference(fedfunds, lag = 1))


car_ts |> 
  autoplot(dfedfunds)


car_reg <- car_ts |> 
  model(reg = TSLM(dcars ~ dfedfunds)) 

car_reg2 <- car_ts |> 
  model(reg = TSLM(cars ~ fedfunds)) 

car_reg2 |> 
  augment() |> 
  features(.innov, ljung_box, lag = 2 * 12)


#---------------------------------------------------------------------------


#-- ARIMA Regression:


car_arima <- car_ts |> 
  model(arima = ARIMA(log(cars) ~ fedfunds))

car_arima |> 
  report()

car_arima |> 
  augment() |> 
  features(.innov, ljung_box, lag = 2 * 12, dof = 6)


#---------------------------------------------------------------------------


#-- Regression with season():

car_ts |> 
  autoplot(cars)

car_season_arima <- car_ts  |> 
  model(season_reg = ARIMA(cars ~ season())) 

car_season_arima |> 
  augment() |> 
  features(.innov, ljung_box, lag = 2 * 12, dof = 6)




#---------------------------------------------------------------------------







