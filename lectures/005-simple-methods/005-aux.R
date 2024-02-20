library(tidyverse)
library(fpp3)
library(hrbrthemes)
library(scales)
library(ggeasy)
library(MetBrewer)
library(janitor)


theme_set(theme_ipsum())


####


# Random walk:

set.seed(166) 
w = rnorm(200)
x = cumsum(w) 


x |> 
  as_tibble() |> 
  mutate(t = row_number()) |> 
  ggplot(aes(y = value, x = t)) +
  geom_line()




######


## Air passengers data:


# Mean method:

air <- read_csv("air_passengers.csv")


air_ts <- air |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date)


air_mean <- air_ts |> 
  model(mean_model = MEAN(passengers))


air_mean_fc <- air_mean  |> 
  forecast(h = 24)

air_mean_fc |> 
  autoplot(air_ts, level = NULL, color = "#974c90", linewidth = 1.1) +
  labs(title = "Monthly air passsengers, Jan 1949 – Dec 1960",
       subtitle = "24-month forecast using the average (mean) method") +
  labs(y = "Passengers",
       x = "")
  
# Naive method:

air_naive <- air_ts |> 
  model(naive_model = NAIVE(passengers))


air_naive_fc <- air_naive  |> 
  forecast(h = 24)

air_naive_fc |> 
  autoplot(air_ts, level = NULL, color = "#974c90", linewidth = 1.1) +
  labs(title = "Monthly air passsengers, Jan 1949 – Dec 1960",
       subtitle = "24-month forecast using the naïve method") +
  labs(y = "Passengers",
       x = "")



# Seasonal naive method:

air_snaive <- air_ts |> 
  model(snaive_model = SNAIVE(passengers))


air_snaive_fc <- air_snaive  |> 
  forecast(h = 24)

air_snaive_fc |> 
  autoplot(air_ts, level = NULL, color = "#974c90", linewidth = 1.1) +
  labs(title = "Monthly air passsengers, Jan 1949 – Dec 1960",
       subtitle = "24-month forecast using the seasonal naïve method") +
  labs(y = "Passengers",
       x = "")


# Seasonal naive with drift method:

air_snaive_drift <- air_ts |> 
  model(snaive_model_drift = SNAIVE(passengers, drift = TRUE))


air_snaive_drift_fc <- air_snaive_drift  |> 
  forecast(h = 24)

air_snaive_drift_fc |> 
  autoplot(air_ts, level = NULL, color = "#974c90", linewidth = 1.1) +
  labs(title = "Monthly air passsengers, Jan 1949 – Dec 1960",
       subtitle = "24-month forecast using the seasonal naïve method with drift") +
  labs(y = "Passengers",
       x = "")


# Drift method:

air_drift <- air_ts |> 
  model(drift_model = RW(passengers ~ drift()))

air_drift_fc <- air_drift |> 
  forecast(h = 24)


air_drift_fc |> 
  autoplot(air_ts, level = NULL, color = "#974c90", linewidth = 1.1) +
  labs(title = "Monthly air passsengers, Jan 1949 – Dec 1960",
       subtitle = "24-month forecast using the drift method") +
  labs(y = "Passengers",
       x = "")



## All together:

air_fit <- air_ts |> 
  model(mean_model = MEAN(passengers),
        naive_model = NAIVE(passengers),
        snaive_model = SNAIVE(passengers),
        drift_model = RW(passengers ~ drift()))

air_fc <- air_fit |> 
  forecast(h = 24)

air_fc |> 
  autoplot(air_ts, level = NULL) +
  labs(title = "Forecasts for monthly air passengers, Jan 1949 – Dec 1960",
       y = "Passengers",
       x = "") +
  guides(colour = guide_legend(title = "Forecast")) +
  easy_plot_legend_size(12) +
  scale_color_met_d("Derain")



#####################


## US-Euro exchange rate:

dex <- read_csv("DEXUSEU.csv") |> 
  clean_names() |> 
  rename(exch = dexuseu)


dex_ts <- dex |> 
  mutate(exch = as.double(exch)) |> 
  as_tsibble(index = date) |> 
  mutate(day = row_number()) |>
  update_tsibble(index = day, regular = TRUE)

dex_ts |> 
  autoplot(exch) +
  labs(title = "U.S. Dollars to Euro spot exchange rate",
       subtitle = "Feb 2019 – Feb 2024 (daily)",
       caption = "Source: U.S. Federal Reserve System",
       y = "U.S. Dollars to 1 Euro",
       x = "")


dex_fit <- dex_ts |> 
  model(mean_model = MEAN(exch),
        naive_model = NAIVE(exch),
        drift_model = RW(exch ~ drift()))

dex_fc <- dex_fit |> 
  forecast(h = 30)

dex_fc |> 
  autoplot(dex_ts, level = NULL) +
  labs(title = "Forecasts for U.S. Dollars to Euro spot exchange rate",
       subtitle = "Feb 2019 – Feb 2024 (daily)",
       y = "U.S. Dollars to 1 Euro",
       x = "") +
  guides(colour = guide_legend(title = "Forecast")) +
  easy_plot_legend_size(12) 
