library(tidyverse)
library(fpp3)
library(hrbrthemes)
library(scales)
library(ggeasy)
library(MetBrewer)
library(janitor)
library(patchwork)


theme_set(theme_ipsum())


#--------------------------------------------------------------------------------


unemp <- read_csv("UNRATE.csv") |> 
  clean_names()

unemp_ts <- unemp |> 
  mutate(date = yearquarter(date)) |> 
  as_tsibble(index = date)


gdp <- read_csv("GDPC1.csv") |> 
  clean_names() |> 
  rename(gdp = gdpc1)

gdp_ts <- gdp |> 
  mutate(date = yearquarter(date)) |> 
  as_tsibble(index = date)


infrate <- read_csv("CPIAUCSL3.csv") |> 
  clean_names() |> 
  rename(infrate = cpiaucsl_pc1)

infrate_ts <- infrate |> 
  mutate(date = year(date),
         infrate = as.double(infrate)) |> 
  as_tsibble(index = date)


#--------------------------------------------------------------------------------



##-- GDP:


gdp_ts |> 
  model(arima110 = ARIMA(gdp ~ pdq(1, 1, 0)),
        arima211 = ARIMA(gdp ~ pdq(2, 1, 1)),
        arima_auto = ARIMA(gdp))

gdp_ts |> 
  model(arima110 = ARIMA(gdp ~ pdq(1, 1, 0)),
        arima211 = ARIMA(gdp ~ pdq(2, 1, 1)),
        arima_auto = ARIMA(gdp)) |> 
  glance() |> 
  arrange(AICc) |> 
  select(.model, AIC, AICc)

#--------------------------------------------------------------------------------



##-- UNRATE:


unemp_ts |> 
  model(arima100 = ARIMA(unrate ~ pdq(1, 0, 0)),
        arima101 = ARIMA(unrate ~ pdq(1, 0, 1)),
        arima_auto = ARIMA(unrate))


unemp_ts |> 
  model(arima100 = ARIMA(unrate ~ pdq(1, 0, 0)),
        arima101 = ARIMA(unrate ~ pdq(1, 0, 1)),
        arima_auto = ARIMA(unrate)) |> 
  glance() |> 
  arrange(AICc) |> 
  select(.model, AIC, AICc)


#--------------------------------------------------------------------------------




##-- INFRATE:


infrate_ts |> 
  model(arima500 = ARIMA(infrate ~ pdq(5, 0, 0)),
        arima301 = ARIMA(infrate ~ pdq(3, 0, 1)),
        arima_auto = ARIMA(infrate))

infrate_ts |> 
  model(arima500 = ARIMA(infrate ~ pdq(5, 0, 0)),
        arima301 = ARIMA(infrate ~ pdq(3, 0, 1)),
        arima_auto = ARIMA(infrate)) |> 
  glance() |> 
  arrange(AICc) |> 
  select(.model, AIC, AICc)
