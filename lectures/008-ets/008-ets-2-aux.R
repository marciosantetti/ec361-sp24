library(tidyverse)
library(fpp3)
library(hrbrthemes)
library(scales)
library(ggeasy)
library(MetBrewer)
library(janitor)
library(patchwork)


theme_set(theme_ipsum())


#--------------------------------------------------------------------

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



## Estimation:

air_season_fit <- air_ts |> 
  model(additive_method = ETS(passengers ~ error("A") + trend("A") + season("A")),
        multiplicative_method = ETS(passengers ~ error("M") + trend("A") + season("M")))

air_season_A_fit <- air_ts |> 
  model(additive_method = ETS(passengers ~ error("A") + trend("A") + season("A")))


air_season_M_fit <- air_ts |> 
  model(multiplicative_method = ETS(passengers ~ error("M") + trend("A") + season("M")))



## Parameters:


air_season_A_fit |> 
  augment()

air_season_A_fit |> 
  report()

air_season_M_fit |> 
  augment()

air_season_M_fit |> 
  report()



## Fit:

air_season_A_fit |> 
  augment() |> 
  ggplot(aes(x = date, y = passengers)) +
  geom_line(aes(color = "Original series")) +
  geom_line(aes(y = .fitted, color = "Fitted values (Additive)"), linewidth = .8) +
  scale_color_manual(values = c("#28935c", "#890c0a")) +
  easy_y_axis_title_size(14) +
  easy_legend_at("top") +
  easy_plot_legend_size(13) +
  labs(y = "Air passengers (thousands)",
       color = "",
       x = "")


air_season_M_fit |> 
  augment() |> 
  ggplot(aes(x = date, y = passengers)) +
  geom_line(aes(color = "Original series")) +
  geom_line(aes(y = .fitted, color = "Fitted values (Multiplicative)"), linewidth = .8) +
  scale_color_manual(values = c("#28935c", "#890c0a")) +
  easy_y_axis_title_size(14) +
  easy_legend_at("top") +
  easy_plot_legend_size(13) +
  labs(y = "Air passengers (thousands)",
       color = "",
       x = "")



## Forecasts:

air_season_A_fc <- air_season_A_fit |> 
  forecast(h = 24)

air_season_M_fc <- air_season_M_fit |> 
  forecast(h = 24)


air_season_A_fc |> 
  autoplot(air_ts, color = "#d62b5b", linewidth = 0.8) +
  geom_line(aes(y = .fitted), color = "#890c0a", linewidth = 0.8,
            data = augment(air_season_A_fit)) +
  easy_y_axis_title_size(14) +
  easy_legend_at("top") +
  labs(y = "Air passengers (thousands)",
       color = "",
       x = "", 
       title = "24-month ahead forecast (additive method)")

air_season_M_fc |> 
  autoplot(air_ts, color = "#d62b5b", linewidth = 0.8) +
  geom_line(aes(y = .fitted), color = "#890c0a", linewidth = 0.8,
            data = augment(air_season_M_fit)) +
  easy_y_axis_title_size(14) +
  easy_legend_at("top") +
  labs(y = "Air passengers (thousands)",
       color = "",
       x = "", 
       title = "24-month ahead forecast (multiplicative method)")


## Model comparison:


air_training <- air_ts |> 
  filter_index(. ~ "1958-12-01")



air_season_models_fit <- air_training |> 
  model(additive_method = ETS(passengers ~ error("A") + trend("A") + season("A")),
        multiplicative_method = ETS(passengers ~ error("M") + trend("A") + season("M")))


air_season_models_fc <- air_season_models_fit |> 
  forecast(h = 24)


air_season_models_fc |> 
  autoplot(air_ts, linewidth = .9, level = NULL) +
  easy_y_axis_title_size(14) +
  easy_legend_at("top") +
  easy_plot_legend_size(13) +
  labs(y = "Air passengers (thousands)",
       color = "",
       x = "", 
       title = "24-month ahead forecast (model comparison)")



air_season_models_fc |> 
  accuracy(air_ts) |> 
  select(.model, MAE, RMSE, MAPE, MASE)

#--------------------------------------------------------------------


air_pass_ets <- air_ts |> 
  model(ETS(passengers))


air_pass_ets_fc <- air_pass_ets |> 
  forecast(h = 24)


air_pass_ets_fc |> 
  autoplot(air_ts, linewidth = .9, level = NULL) +
  easy_y_axis_title_size(14) +
  easy_legend_at("top") +
  easy_plot_legend_size(13) +
  labs(y = "Air passengers (thousands)",
       color = "",
       x = "", 
       title = "24-month ahead forecast (fable selection)")


air_pass_ets_fc |> 
  autoplot(air_ts, linewidth = .9) +
  easy_y_axis_title_size(14) +
  easy_legend_at("top") +
  easy_plot_legend_size(13) +
  labs(y = "Air passengers (thousands)",
       color = "",
       x = "", 
       title = "24-month ahead forecast (fable selection)")


#--------------------------------------------------------------------


## Model selection

air_ts |> 
  model(ets = ETS(passengers ~ error("A") + trend("A") + season("A"))) |> 
  components() |> 
  autoplot()



air_ts |> 
  model(ets = ETS(passengers ~ error("M") + trend("A") + season("M"))) |> 
  components() |> 
  autoplot()


air_ts |> 
  model(ets = ETS(passengers)) |> 
  report()

air_season_A_fit |> 
  report()

air_season_M_fit |> 
  report()

#--------------------------------------------------------------------


## Unemployment rate:

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


unemp_ts |> 
  model(unrate_ETS = ETS(unrate)) |> 
  report()


unemp_ts |> 
  model(unrate_ETS = ETS(unrate)) |>
  forecast(h = 8) |> 
  autoplot(unemp_ts)


#--------------------------------------------------------------------


## CPI:

cpi <- read_csv("CPIAUCSL.csv") |> 
  clean_names() |> 
  rename(cpi = cpiaucsl)


cpi_ts <- cpi |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date)

cpi_ts |> 
  model(cpi_ETS = ETS(cpi)) |> 
  report()


cpi_ts |> 
  model(cpi_ETS = ETS(cpi)) |>
  forecast(h = 24) |> 
  autoplot(cpi_ts)
