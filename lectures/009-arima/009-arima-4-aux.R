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


#--------------------------------------------------------------------------------

# Prework:


air_ts |> 
  autoplot()

air_ts |> 
  gg_season(labels = "right") +
  labs(title = "U.S. air passengers: Seasonal plot")

air_ts |> 
  ACF(lag_max = 36) |> 
  autoplot() +
  labs(title = "U.S. air passengers: ACF plot")



#--------------------------------------------------------------------------------

air_ts |> 
  autoplot()

air_ts |> 
  mutate(seas_diff = difference(passengers, lag = 12)) |>
  features(seas_diff, unitroot_kpss) # reject H0.

air_ts |> 
  model(arima_auto = ARIMA(passengers))



# Only seasonal difference:

air_ts |> 
  mutate(seas_diff = difference(passengers, lag = 12)) |> # taking seasonal differencing.
  gg_tsdisplay(seas_diff, plot_type = "partial")


# One additional difference:

air_ts |> 
  mutate(seas_diff = difference(passengers, lag = 12),
         dseas_diff = difference(seas_diff, lag = 1)) |>  #taking 1st difference of seasonal difference.
  gg_tsdisplay(dseas_diff, plot_type = "partial")

#--------------------------------------------------------------------------------



air_ts |> 
  mutate(seas_diff = difference(passengers, lag = 12),
         dseas_diff = difference(seas_diff, lag = 1)) |>
  features(dseas_diff, unitroot_kpss)


#--------------------------------------------------------------------------------



air_ts <- air_ts |> 
  mutate(seas_diff = difference(passengers, lag = 12),
         dseas_diff = difference(seas_diff, lag = 1))


acf <- air_ts |> 
  ACF(dseas_diff) |> 
  autoplot()

pacf <- air_ts |> 
  PACF(dseas_diff) |> 
  autoplot()

acf | pacf


air_arima_fit <- air_ts |> 
  model(arima110_010 = ARIMA(passengers ~ pdq(1, 1, 0) + PDQ(0, 1, 0)),
        arima011_010 = ARIMA(passengers ~ pdq(0, 1, 1) + PDQ(0, 1, 0)),
        arima310_010 = ARIMA(passengers ~ pdq(3, 1, 0) + PDQ(0, 1, 0)),
        arima_auto = ARIMA(passengers))

air_arima_fit


air_arima_fit |> 
  glance() |> 
  arrange(AICc) |> 
  select(.model, AIC, AICc)

#--------------------------------------------------------------------------------


air_arima_fit |> 
  select(arima_auto) |> 
  gg_tsresiduals()

air_arima_fit |> 
  augment() |> 
  filter(.model == "arima310_010") |> 
  features(.innov, ljung_box, lag = 2 * 12, dof = 3)


#--------------------------------------------------------------------------------


# Log-transformed

air_ts |> 
  ggplot(aes(x = date, y = log(passengers))) +
  geom_line() +
  labs(title = "International airline passengers (logs)",
       subtitle = "Jan 1949 – Dec 1960",
       caption = "Source: Brown (1962).",
       x = "",
       y = "log(Thousands)") +
  easy_y_axis_title_size(14) +
  easy_plot_caption_size(14)



air_ts |> 
  mutate(log_seas_diff = difference(log(passengers), lag = 12)) |> 
  autoplot(log_seas_diff)

air_ts <- air_ts |> 
  mutate(log_seas_diff = difference(log(passengers), lag = 12))


air_ts |> 
  features(log_seas_diff, unitroot_kpss)




acf2 <- air_ts |> 
  ACF(log_seas_diff) |> 
  autoplot() +
  labs(title = "ACF plot")

pacf2 <- air_ts |> 
  PACF(log_seas_diff) |> 
  autoplot() +
  labs(title = "PACF plot")

acf2 | pacf2









#--------------------------------------------------------------------------------


air_log_fit <- air_ts |> 
  model(arima200_011 = ARIMA(log(passengers) ~ 1 + pdq(2, 0, 0) + PDQ(0, 1, 1)),
        arima202_011 = ARIMA(log(passengers) ~ 1 + pdq(2, 0, 2) + PDQ(0, 1, 1)),
        arima_auto = ARIMA(log(passengers))) 

air_ts |> 
  model(arima_auto = ARIMA(log(passengers))) |>
  report()


air_log_fit |> 
  glance() |> 
  arrange(AICc) |> 
  select(.model, AIC, AICc)


air_log_fit |> 
  select(arima_auto) |> 
  forecast(h = 24) |> 
  autoplot(air_ts, level = 95, linewidth = .9, color = "#73797f")



air_log_fit |> 
  select(arima_auto) |> 
  forecast(h = 24) |> 
  head(5)

#--------------------------------------------------------------------------------