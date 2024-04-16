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


dat <- read_csv("ps1_data.csv")


dat_yr <- dat |> 
  mutate(date = floor_date(period, unit = "year")) |> 
  group_by(date) |> 
  summarize(infrate = mean(infrate),
            unrate = mean(unrate)) 


dat_yr <- dat_yr |> 
  mutate(date = year(date)) |> 
  as_tsibble(index = date)

dat_yr <- dat_yr |> 
  mutate(delta_infrate = difference(infrate, lag = 1))


dat_yr |> 
  filter_index(1970 ~ 1995) |> 
  ggplot(aes(x = unrate, y = delta_infrate)) +
  geom_point() +
  expand_limits(y = c(-5, 6))
 
 
dat_yr |> 
  ggplot(aes(x = unrate, y = infrate)) +
  geom_point()

dat_yr |> 
  ggplot(aes(x = unrate, y = delta_infrate)) +
  geom_point() +
  geom_smooth(method = "lm", se=F)



#--------------------------------------------------------------------------------

dat_yr |> 
  features(unrate, unitroot_kpss)


dat_yr |> 
  features(delta_infrate, unitroot_kpss)


#--------------------------------------------------------------------------------

dat_yr |> 
  model(reg = TSLM(delta_infrate ~ unrate)) |> 
  augment() |> 
  features(.innov, ljung_box, lag = 10)


dat_yr |> 
  model(arima_reg = ARIMA(delta_infrate ~ unrate)) |> 
  augment() |> 
  features(.innov, ljung_box, lag = 10, dof = 2)



dat_yr |> 
  model(arima_reg = ARIMA(delta_infrate ~ unrate)) |> 
  augment() |> 
  ggplot(aes(x = date, y = delta_infrate)) +
  geom_line(aes(color = "Change in inflation rate")) +
  geom_line(aes(y = .fitted, color = "Fitted values")) +
  scale_color_manual(values = c("#2a5fac", "#ec5c11")) +
  easy_legend_at("top") +
  easy_add_legend_title("")  +
  easy_plot_legend_size(13) +
  labs(y = "Change in inflation rate",
       x = "") +
  easy_y_axis_title_size(13)
  





#--------------------------------------------------------------------------------


dat_ts <- dat |> 
  mutate(period = yearmonth(period)) |> 
  as_tsibble(index = period)
  

dat_ts <- dat_ts |> 
  mutate(delta_infrate = difference(infrate, lag = 1))

dat_ts |> 
  filter_index("1970-01-01" ~ "1995-12-01") |> 
  ggplot(aes(x = unrate, y = delta_infrate)) +
  geom_point()


dat_ts |> 
  filter_index("2020-01-01" ~ .) |> 
  ggplot(aes(x = unrate, y = infrate)) +
  geom_point()


dat_ts |> 
  filter_index("2020-01-01" ~ .) |>
  model(arima_reg = ARIMA(delta_infrate ~ unrate)) |> 
  report()
