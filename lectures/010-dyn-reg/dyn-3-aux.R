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


air <- read_csv("air_passengers.csv")


air_ts <- air |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date)

air_ts |> 
  ggplot(aes(x = date, y = passengers)) +
  geom_line() +
  labs(title = "International airline passengers",
       subtitle = "Jan 1949 – Dec 1960",
       caption = "Source: Brown (1962).",
       x = "",
       y = "Thousands") +
  easy_y_axis_title_size(14) +
  easy_plot_caption_size(14)

#--------------------------------------------------------------------------------


#-- TSLM:

air_tslm <- air_ts |> 
  model(reg = TSLM(log(passengers) ~ trend() + season()))


air_tslm |> 
  report()

air_tslm |> 
  augment() |> 
  features(.innov, ljung_box, lag = 2 * 12)


air_tslm |> 
  augment() |> 
  gg_tsdisplay(.innov, plot_type = "partial")



#--------------------------------------------------------------------------------




#--- ARIMA errors :

air_arima <- air_ts |> 
  model(arima_errors = ARIMA(log(passengers) ~ trend() + season() + PDQ(0, 0, 0)))


air_arima |>
  report()

air_arima |> 
  augment() |> 
  gg_tsdisplay(.innov, plot_type = "partial")



air_arima |> 
  augment() |> 
  features(.innov, ljung_box, lag = 2 * 12)

air_arima |> 
  forecast(h = 24) |> 
  autoplot(air_ts, level = 95)

#--------------------------------------------------------------------------------



#--- Fourier terms:

air_harmonic <- air_ts |> 
  model(harmonic = ARIMA(log(passengers) ~ fourier(K = 6) + PDQ(0, 0, 0)))


air_harmonic |> 
  augment() |> 
  gg_tsdisplay(.innov, plot_type = "partial")


air_harmonic |> 
  augment() |> 
  features(.innov, ljung_box, lag = 2 * 12)

air_harmonic |> 
  forecast(h = 24) |> 
  autoplot(air_ts, level = 95)




#--------------------------------------------------------------------------------


air_harmonic_fit <- model(air_ts,
              k1 = ARIMA(log(passengers) ~ fourier(K = 1) + PDQ(0, 0, 0)),
              k2 = ARIMA(log(passengers) ~ fourier(K = 2) + PDQ(0, 0, 0)),
              k3 = ARIMA(log(passengers) ~ fourier(K = 3) + PDQ(0, 0, 0)),
              k4 = ARIMA(log(passengers) ~ fourier(K = 4) + PDQ(0, 0, 0)),
              k5 = ARIMA(log(passengers) ~ fourier(K = 5) + PDQ(0, 0, 0)),
              k6 = ARIMA(log(passengers) ~ fourier(K = 6) + PDQ(0, 0, 0)))

air_harmonic_fit |>
  forecast(h = 24) |>
  autoplot(air_ts, level = 95) +
  facet_wrap(vars(.model), ncol = 2) +
  guides(colour = "none", fill = "none", level = "none") +
  geom_label(aes(x = yearmonth("1953 Jan"), y = 700, label = paste0("AICc = ", format(AICc))),
    data = glance(air_harmonic_fit)) +
  labs(title= "U.S. air passengers: 24-month ahead forecast",
       y="Thousands",
       x = "")


air_harmonic_fit |> 
  select(k6) |> 
  augment() |> 
  features(.innov, ljung_box, lag = 2 * 12, dof = 2)


#--------------------------------------------------------------------------------


dat <- read_csv("ps1_data.csv")


phillips_ts <- dat |> 
  mutate(date = floor_date(period, unit = "year")) |> 
  group_by(date) |> 
  summarize(infrate = mean(infrate),
            unrate = mean(unrate)) 


phillips_ts <- phillips_ts |> 
  mutate(date = year(date)) |> 
  as_tsibble(index = date)

phillips_ts <- phillips_ts |> 
  mutate(delta_infrate = difference(infrate, lag = 1))


phillips_arima <- phillips_ts |> 
  model(arima_reg = ARIMA(delta_infrate ~ unrate))

phillips_arima |> 
  augment() |> 
  features(.innov, ljung_box, lag = 10)


#-- Scenarios:

future_scenarios <- scenarios(average = new_data(phillips_ts, n = 5) |> 
                                mutate(unrate = mean(phillips_ts$unrate)),   # 5.916276
                              pessimistic = new_data(phillips_ts, n = 5) |> 
                                mutate(unrate = 10),
                              optimistic = new_data(phillips_ts, n = 5) |> 
                                mutate(unrate = 3.36))


phillips_scen_fc <- phillips_arima |> 
  forecast(h = 5, new_data = future_scenarios)


phillips_ts |> 
  autoplot(delta_infrate) +
  autolayer(phillips_scen_fc, level = NULL, linewidth = 0.8) +
  scale_color_manual(values= c("#4682b4", "#800020", "#228b22"))


phillips_ts |> 
  autoplot(delta_infrate) +
  autolayer(phillips_scen_fc |> filter(.scenario == "average"), level = 95, color = "#4682b4")

phillips_ts |> 
  autoplot(delta_infrate) +
  autolayer(phillips_scen_fc |> filter(.scenario == "pessimistic"), level = 95, color = "#800020")


phillips_ts |> 
  autoplot(delta_infrate) +
  autolayer(phillips_scen_fc |> filter(.scenario == "optimistic"), level = 95, color = "#228b22")
