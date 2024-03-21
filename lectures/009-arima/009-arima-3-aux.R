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


saving <- read_csv("PSAVERT.csv") |> 
  clean_names() |> 
  rename(psav = psavert)

saving_ts <- saving |> 
  mutate(date = yearquarter(date),
         psav = as.double(psav)) |> 
  as_tsibble(index = date)




#--------------------------------------------------------------------------------

saving_ts |> 
  autoplot()


saving_ts |> 
  ACF() |> 
  autoplot()



saving_ts |> 
  PACF() |> 
  autoplot()

saving_arima_fit <- saving_ts |> 
  model(arima410 = ARIMA(psav ~ pdq(4, 1, 0) + PDQ(0, 0, 0)),
        arima214 = ARIMA(psav ~ pdq(2, 1, 4) + PDQ(0, 0, 0)),
        arima_auto = ARIMA(psav))



saving_arima_fit |> 
  glance() |> 
  arrange(AICc) |> 
  select(.model, AIC, AICc)
  