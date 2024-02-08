library(tidyverse)
library(fpp3)
library(lubridate)

## tsibble objects do not allow for `summarize()` operations.



beer <- aus_production |> 
  select(Quarter, Beer) |> 
  filter(year(Quarter) >= 1992)


beer |> 
  as_tibble() |> 
  mutate(period = seq(1, nrow(beer), 1)) |> 
  summarize(mean_beer = sum(Beer) / nrow(beer),
            variance = (1 / nrow(beer)) * sum((Beer - mean_beer)^2),
            autocovariance = (1 / nrow(beer)) * sum((Beer - mean_beer) * (l_beer - mean_beer), na.rm = TRUE),
            acf = autocovariance / variance)


beer |> 
  ACF(lag_max = 9)

beer |> 
  gg_lag(geom = "point", lags = 1:9)




###

air <- read_csv("https://raw.githubusercontent.com/selva86/datasets/master/AirPassengers.csv")


air |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date) |> 
  gg_lag(value, geom = "point",
         lags = 1:12)

air_passengers <- air |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date)


air_passengers |> 
  ACF(lag_max = 12)


air_qtr <- air |> 
  mutate(period = floor_date(date, unit = "quarter")) |> 
  group_by(period) |> 
  summarize(value = mean(value))

air_qtr |> 
  ggplot(aes(x = period, y = value)) +
  geom_point() +
  geom_line() +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  labs(title = "International airline passengers",
       subtitle = "1949Q1–1960Q4",
       caption = "Source: Brown (1962).",
       x = "",
       y = "Thousands")


air_qtr |> 
  mutate(period = yearquarter(period)) |> 
  as_tsibble(index = period) |> 
  gg_lag(geom = "point")



###

elec <- aus_production |> 
  select(Quarter, Electricity) |> 
  filter(year(Quarter) >= 1992)


elec |> 
  gg_lag(geom = "point")

beer |> 
  autoplot()
