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


air <- read_csv("air_passengers.csv")


air_ts <- air |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date)

air_ts |> 
  mutate(diff_passengers = difference(passengers, lag = 1),
         seas_diff_passengers = difference(passengers, lag = 12)) |> 
  autoplot(diff_passengers)


air_ts |> 
  mutate(diff_passengers = difference(passengers, lag = 1),
         seas_diff_passengers = difference(passengers, lag = 12),
         diff_log_passengers = difference(log(passengers), lag = 1),
         seas_diff_log_passengers = difference(log(passengers), lag = 12)) |> 
  autoplot(diff_log_passengers)


air_ts |> 
  features(passengers, unitroot_kpss)



#--------------------------------------------------------------------------------


set.seed(1223)

T <- 100
epsilon <- rnorm(n = T, mean = 0, sd = 1)

yt <- rnorm(n = T, mean = 10, sd = 1)

arp <- 18 - 0.8 * lag(yt, n = 1) + epsilon


plot.ts(arp)



#--------------------------------------------------------------------------------

set.seed(1223)
data <- tibble(
  T = 1:100,
  epsilon = rnorm(n = T, mean = 0, sd = 1),
  yt = rnorm(n = T, mean = 10, sd = 1),
  yt2 = rnorm(n = T, mean = 20, sd = 1),
  ar = 18 - 0.8 * lag(yt, n = 1) + epsilon,
  ar2 = 8 + 1.3 * lag(yt2, n = 1) - 0.7 * lag(yt2, n = 2) + epsilon
)


data |> 
  as_tsibble(index = T) |> 
  autoplot(ar, linewidth = 0.7) +
  labs(title = "An autoregressive process",
       y = "y") +
  easy_y_axis_title_size(13) +
  easy_x_axis_title_size(13)


data |> 
  as_tsibble(index = T) |> 
  autoplot(ar2, linewidth = 0.7) +
  labs(title = "Another autoregressive process",
       y = "y") +
  easy_y_axis_title_size(13) +
  easy_x_axis_title_size(13)



#--------------------------------------------------------------------------------

set.seed(1223)
data3 <- tibble(
  T = 1:100,
  epsilon = rnorm(n = T, mean = 0, sd = 1),
  yt = rnorm(n = T, mean = 133.3333, sd = 1),
  ar = 100 + 0.25 * lag(yt, n = 1) + epsilon
)

data3 |> 
  as_tsibble(index = T) |> 
  autoplot(ar, linewidth = 0.7) +
  labs(title = "Another autoregressive process",
       y = "y") +
  easy_y_axis_title_size(13) +
  easy_x_axis_title_size(13)



#--------------------------------------------------------------------------------


set.seed(1223)
data2 <- tibble(
  T = 1:100,
  epsilon = rnorm(n = T, mean = 0, sd = 1),
  ma = 20 + 0.8 * lag(epsilon, n = 1) + epsilon,
  ma2 = epsilon - lag(epsilon, n = 1) + 0.8 * lag(epsilon, n = 2)
)


data2 |> 
  as_tsibble(index = T) |> 
  autoplot(ma, linewidth = 0.7) +
  labs(title = "A moving average process",
       y = "y") +
  easy_y_axis_title_size(13) +
  easy_x_axis_title_size(13)

data2 |> 
  as_tsibble(index = T) |> 
  autoplot(ma2, linewidth = 0.7) +
  labs(title = "Another moving average process",
       y = "y") +
  easy_y_axis_title_size(13) +
  easy_x_axis_title_size(13)


#--------------------------------------------------------------------------------



unemp <- read_csv("UNRATE.csv") |> 
  clean_names()

unemp_ts <- unemp |> 
  mutate(date = yearquarter(date)) |> 
  as_tsibble(index = date)

unemp_ts |> 
  autoplot(unrate) +
  labs(title = "U.S. unemployment rate",
       subtitle = "1990Q1—2023Q4",
       caption = "Source: U.S. Bureau of Labor Statistics.",
       y = "Percent") +
  easy_y_axis_title_size(14) +
  easy_plot_caption_size(13)

unemp_fit <- unemp_ts |> 
  model(unemp_arima = ARIMA(unrate))

unemp_fit |> 
  report()



unemp_fit |> 
  augment() |> 
  ggplot(aes(x = date, y = unrate)) +
  geom_line(aes(color = "Original series")) +
  geom_line(aes(y = .fitted, color = "Fitted values"), linewidth = 0.8) +
  scale_color_manual(values = c( "#f23869", "#133337")) +
  labs(title = "U.S. unemployment rate",
       y = "Percent",
       color = "") +
  easy_y_axis_title_size(14) +
  easy_plot_caption_size(13) +
  easy_plot_legend_size(13) +
  easy_legend_at("top")

unemp_ts |> 
  ACF() |> 
  autoplot()

unemp_ts |> 
  PACF() |> 
  autoplot()

#--------------------------------------------------------------------------------

gdp <- read_csv("GDPC1.csv") |> 
  clean_names() |> 
  rename(gdp = gdpc1)

gdp_ts <- gdp |> 
  mutate(date = yearquarter(date)) |> 
  as_tsibble(index = date)

gdp_ts |> 
  autoplot()


gdp_fit <- gdp_ts |> 
  model(gdp_arima = ARIMA(gdp))


gdp_fit |> 
  augment() |> 
  ggplot(aes(x = date, y = gdp)) +
  geom_line(aes(color = "Original series")) +
  geom_line(aes(y = .fitted, color = "Fitted values"), linewidth = 0.8) +
  scale_color_manual(values = c( "#f23869", "#133337")) +
  labs(title = "U.S. Real Gross Domestic Product",
       y = "Billions of chained 2017 dollars",
       color = "") +
  easy_y_axis_title_size(14) +
  easy_plot_caption_size(13) +
  easy_plot_legend_size(13) +
  easy_legend_at("top")

gdp_fit |> 
  report()


#--------------------------------------------------------------------------------


infrate <- read_csv("CPIAUCSL3.csv") |> 
  clean_names() |> 
  rename(infrate = cpiaucsl_pc1)

infrate_ts <- infrate |> 
  mutate(date = year(date),
         infrate = as.double(infrate)) |> 
  as_tsibble(index = date)

infrate_ts |> 
  autoplot() +
  labs(title = "U.S. annual inflation rate",
       subtitle = "1948—2023",
       caption = "Source: U.S. Bureau of Labor Statistics.",
       y = "% change from a year ago") +
  easy_y_axis_title_size(14) +
  easy_plot_caption_size(13)


infrate_fit <- infrate_ts |> 
  model(infrate_arima = ARIMA(infrate))


infrate_fit |> 
  report()


infrate_fit |> 
  augment() |> 
  ggplot(aes(x = date, y = infrate)) +
  geom_line(aes(color = "Original series")) +
  geom_line(aes(y = .fitted, color = "Fitted values"), linewidth = 0.8) +
  scale_color_manual(values = c( "#f23869", "#133337")) +
  labs(title = "U.S. annual inflation rate",
       y = "% change from a year ago",
       color = "") +
  easy_y_axis_title_size(14) +
  easy_plot_caption_size(13) +
  easy_plot_legend_size(13) +
  easy_legend_at("top")


#--------------------------------------------------------------------------------


