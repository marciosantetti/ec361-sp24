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
  mutate(date = year(date),
         psav = as.double(psav)) |> 
  as_tsibble(index = date) |> 
  filter_index(. ~ "2023")




#--------------------------------------------------------------------------------

saving_ts |> 
  autoplot() +
  labs(title = "Personal saving rate",
       y = "Percent",
       subtitle = "1959–2023",
       caption = "Source: U.S. Bureau of Economic Analysis.") +
  easy_y_axis_title_size(13) +
  easy_plot_caption_size(13)


saving_ts |> 
  ACF() |> 
  autoplot() +
  labs(title = "ACF plot",
       y = "ACF") +
easy_y_axis_title_size(15)



saving_ts |> 
  PACF() |> 
  autoplot() +
  labs(title = "PACF plot",
       y = "PACF") +
  easy_y_axis_title_size(15)



saving_arima_fit <- saving_ts |> 
  model(arima310 = ARIMA(psav ~ 1 + pdq(3, 1, 0)), # "1" includes a constant (c).
        arima113 = ARIMA(psav ~ 1 + pdq(1, 1, 3)), # "1" includes a constant (c).
        arima_auto = ARIMA(psav)) # letting {fable} select the best model.



saving_arima_fit |> 
  glance() |> 
  arrange(AICc) |> 
  select(.model, AIC, AICc)


saving_arima_fit |> 
  augment() |> 
  filter(.model == "arima310") |> 
  features(.innov, ljung_box, lag = 10, dof = 3)


saving_arima_fit |> 
  augment() |> 
  filter(.model == "arima_auto") |> View()


saving_arima_fit |> 
  augment() |> 
  filter(.model == "arima_auto") |>
  ggplot(aes(x = date, y = psav)) +
  geom_line(aes(color = "Original series")) +
  geom_line(aes(y = .fitted, color = "Fitted values"))


#--------------------------------------------------------------------------------


saving_arima_fc <- saving_arima_fit |> 
  forecast(h = 6)


saving_arima_fc |> 
  filter(.model == "arima310") |> 
  autoplot(saving_ts, level = 95)


saving_arima_fc |> 
  filter(.model == "arima113") |> 
  autoplot(saving_ts, level = 95)


saving_arima_fc |> 
  filter(.model == "arima_auto") |> 
  autoplot(saving_ts, level = 95)


#--------------------------------------------------------------------------------


# Simulating an ARMA(2,2) model:

set.seed(1223)
data <- tibble(
  T = 1:100,
  yt = rnorm(n = T, mean = 23.52941, sd = 1),
  epsilon = rnorm(n = T, mean = 0, sd = 1),
  arma22 = 20 + 0.8 * lag(yt, n = 1) - 0.65 * lag(yt, n = 2) + 0.4 * lag(epsilon, n = 1) - 0.2 * lag(epsilon, n = 2) + epsilon
)

data_ts <- data |> 
  as_tsibble(index = T)

data_ts |> 
  autoplot(arma22, linewidth = 0.7) +
  labs(title = "An ARMA(2, 2) process",
       y = "y") +
  easy_y_axis_title_size(13) +
  easy_x_axis_title_size(13)


data_ts |> 
  model(arima22 = ARIMA(yt ~ 1 + pdq(2, 0, 2))) |>
  report()


data_ts |> 
  model(arima22 = ARIMA(yt ~ 1 + pdq(2, 0, 2))) |> 
  augment() |> View()

data_ts |> 
  model(arima22 = ARIMA(yt ~ 1 + pdq(2, 0, 2))) |> 
  augment() |> 
  ggplot(aes(x = T, y = yt)) +
  geom_line(aes(color = "Y")) +
  geom_line(aes(y = .fitted, color = "Fitted values"))


data_ts |> 
  model(arima22 = ARIMA(arma22 ~ 1 + pdq(2, 0, 2))) |> 
  forecast(h = 2) |> 
  autoplot(data_ts)


#--------------------------------------------------------------------------------
