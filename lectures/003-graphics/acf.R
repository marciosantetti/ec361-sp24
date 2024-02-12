library(tidyverse)
library(fpp3)
library(lubridate)

## tsibble objects do not allow for `summarize()` operations.



beer <- aus_production |> 
  select(Quarter, Beer) |> 
  filter(year(Quarter) >= 1992)


beer |> 
  as_tibble() |> 
  ggplot(aes(x = Quarter, y = Beer)) +
  geom_line() +
  geom_point()

beer |> 
  as_tibble() |> 
  mutate(period = seq(1, nrow(beer), 1)) |> 
  mutate(lbeer = lag(Beer, 1),
         lbeer2 = lag(Beer, 2),
         lbeer3 = lag(Beer, 3),
         lbeer4 = lag(Beer, 4),
         lbeer5 = lag(Beer, 5)) |> 
  summarize(mean_beer = sum(Beer) / nrow(beer),
            variance = (1 / nrow(beer)) * sum((Beer - mean_beer)^2),
            autocovariance1 = (1 / nrow(beer)) * sum((Beer - mean_beer) * (lbeer - mean_beer), na.rm = TRUE),
            autocovariance2 = (1 / nrow(beer)) * sum((Beer - mean_beer) * (lbeer2 - mean_beer), na.rm = TRUE),
            autocovariance3 = (1 / nrow(beer)) * sum((Beer - mean_beer) * (lbeer3 - mean_beer), na.rm = TRUE),
            autocovariance4 = (1 / nrow(beer)) * sum((Beer - mean_beer) * (lbeer4 - mean_beer), na.rm = TRUE),
            autocovariance5 = (1 / nrow(beer)) * sum((Beer - mean_beer) * (lbeer5 - mean_beer), na.rm = TRUE),
            acf1 = autocovariance1 / variance,
            acf2 = autocovariance2 / variance,
            acf3 = autocovariance3 / variance,
            acf4 = autocovariance4 / variance,
            acf5 = autocovariance5 / variance)


beer |> 
  ACF(lag_max = 5)


USgas::us_total |> 
  as_tsibble(index = year,
             key = state) |> 
  filter(state == "New York") |> 
  ACF() |> 
  autoplot()


USgas::us_total |> 
  as_tsibble(index = year,
             key = state) |> 
  filter(state == "New York") |>
  ggplot(aes(y = y, x = year)) +
  geom_point() +
  geom_line()



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



#################

gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2018) |>
  mutate(trading_day = row_number()) |>
  update_tsibble(index = trading_day, regular = TRUE) |>
  mutate(diff = difference(Close)) |>
  #ACF(diff) |>
  autoplot()
