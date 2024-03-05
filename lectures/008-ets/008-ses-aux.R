library(tidyverse)
library(fpp3)
library(hrbrthemes)
library(scales)
library(ggeasy)
library(MetBrewer)
library(janitor)
library(patchwork)


theme_set(theme_ipsum())


####--------------------------------------------------------------------


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


#####------------------------------------------------------------------

## Simple ETS model:

unemp_ets_fit <- unemp_ts |> 
  model(SES = ETS(unrate ~ error("A") + trend("N") + season("N")))



unemp_ets_fit |> 
  report()

unemp_ets_fit |> 
  augment()


unemp_ets_fc <- unemp_ets_fit |> 
  forecast(h = 8)


unemp_ets_fc



unemp_ets_fit |> 
  augment() |> 
  ggplot(aes(x = date, y = unrate)) +
  geom_line(aes(color = "Original series")) +
  geom_line(aes(y = .fitted, color = "Fitted values (SES)"), linewidth = .8) +
  scale_color_manual(values = c("#28935c", "#890c0a")) +
  easy_y_axis_title_size(14) +
  easy_legend_at("top") +
  easy_plot_legend_size(13) +
  labs(y = "Unemployment rate",
       color = "",
       x = "")

unemp_ets_fc |> 
  autoplot(unemp_ts, color = "#28935c", linewidth = 0.8) +
  geom_line(aes(y = .fitted), color = "#890c0a",
            data = augment(unemp_ets_fit)) +
  easy_y_axis_title_size(14) +
  labs(y = "Unemployment rate",
       color = "",
       x = "", 
       title = "8-quarter ahead forecast")



######-----------------------------------------------------------------------------------


cpi <- read_csv("CPIAUCSL.csv") |> 
  clean_names() |> 
  rename(cpi = cpiaucsl)


cpi_ts <- cpi |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date)


cpi_ts |> 
  autoplot() +
  labs(title = "U.S. Consumer Price Index (CPI)",
                  subtitle = "01/2000—01/2024",
                  caption = "Source: U.S. Bureau of Labor Statistics.",
                  y = "Index 1982-1984 = 100") +
  easy_y_axis_title_size(14) +
  easy_plot_caption_size(13)


cpi_ets_trend_fit <- cpi_ts |> 
  model(ETS_Trend = ETS(cpi ~ error("A") + trend("A") + season("N")))

cpi_ets_trend_fit |> 
  augment()


cpi_ets_trend_fit |> 
  report()

cpi_ets_trend_fit |> 
  augment() |> 
  ggplot(aes(x = date, y = cpi)) +
  geom_line(aes(color = "Original series")) +
  geom_line(aes(y = .fitted, color = "Fitted values (ETS with trend)"), linewidth = .8) +
  scale_color_manual(values = c("#28935c", "#890c0a")) +
  easy_y_axis_title_size(14) +
  easy_legend_at("top") +
  easy_plot_legend_size(13) +
  labs(y = "CPI",
       color = "",
       x = "")



cpi_ets_trend_fc <- cpi_ets_trend_fit |> 
  forecast(h = 12)



cpi_ets_trend_fc |> 
  autoplot(cpi_ts, color = "#28935c", linewidth = 0.8) +
  geom_line(aes(y = .fitted), color = "#890c0a",
            data = augment(cpi_ets_trend_fit)) +
  easy_y_axis_title_size(14) +
  labs(y = "CPI",
       color = "",
       x = "")



#-------------------------------------------------------------------------

cpi_ets_trend_damped_fit <- cpi_ts |> 
  model(ETS_Trend_Damped = ETS(cpi ~ error("A") + trend("Ad") + season("N")))

cpi_ets_trend_damped_fit |> 
  augment()


cpi_ets_trend_damped_fit |> 
  report()


cpi_ets_trend_damped_fc <- cpi_ets_trend_damped_fit |> 
  forecast(h = 36)

cpi_ets_trend_damped_fc |> 
  autoplot(cpi_ts, color = "#28935c", linewidth = 0.8) +
  geom_line(aes(y = .fitted), color = "#890c0a",
            data = augment(cpi_ets_trend_damped_fit)) +
  easy_y_axis_title_size(14) +
  labs(y = "CPI",
       color = "",
       x = "")




#-------------------------------------------------------------------------

cpi_train <- cpi_ts |> 
  filter_index(. ~ "2021-12-01")

cpi_ets_models <- cpi_train |> 
  model(SES = ETS(cpi ~ error("A") + trend("N") + season("N")),
        ETS_trend = ETS(cpi ~ error("A") + trend("A") + season("N")),
        ETS_Damped_trend = ETS(cpi ~ error("A") + trend("Ad") + season("N")))

cpi_models_fc <- cpi_ets_models |> 
  forecast(h = 25)

cpi_models_fc |> 
  accuracy(cpi_ts) |> 
  select(.model, MAE, RMSE, MAPE, MASE)


cpi_models_fc |> 
  autoplot(cpi_ts) +
  easy_y_axis_title_size(14) +
  labs(y = "CPI",
       x = "")

