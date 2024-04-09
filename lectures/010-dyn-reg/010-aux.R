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


dat <- read_csv("fredgraph.csv") |> 
  clean_names() |> 
  rename(cons = pcec96,
         inc = dspic96)

dat_ts <- dat |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date)

dat_ts |> 
  ggplot(aes(x = date, y = cons)) +
  geom_line(aes(color = "Real personal consumption")) +
  geom_line(aes(y = inc, color = "Real disposable income")) +
  scale_color_manual(values = c("#2a5fac", "#ec5c11")) +
  labs(y = "Billions of dollars (2017)",
       x = "",
       title = "U.S. real personal consumption and real disposable income",
       subtitle = "01/2007-02/2024",
       caption = "Source: U.S. Bureau of Economic Analysis (BEA).") +
  easy_legend_at("top") +
  easy_add_legend_title("") +
  easy_plot_legend_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_caption_size(13)


dat_ts |> 
  ggplot(aes(x = inc, y = cons)) +
  geom_point(shape = 21, size = 1.9) +
  geom_smooth(method = "lm", se = F, color = "#4a2956") +
  labs(y = "Consumption",
       x = "Disposable income",
       title = "U.S. real personal consumption and real disposable income",
       subtitle = "01/2007-02/2024",
       caption = "Source: U.S. Bureau of Economic Analysis (BEA).") +
  easy_y_axis_title_size(13) +
  easy_plot_caption_size(13)

#--------------------------------------------------------------------------------


reg_fit <- dat_ts |> 
  model(reg = TSLM(cons ~ inc))

reg_fit |> 
  augment() |> 
  features(.innov, ljung_box, lag = 10)


#--------------------------------------------------------------------------------

reg_trend <- dat_ts |> 
  model(reg_trend = TSLM(cons ~ trend()))

reg_trend |> 
  report()

reg_trend |> 
  augment() |> 
  ggplot(aes(x = date, y = cons)) +
  geom_line(aes(color = "Personal consumption")) +
  geom_line(aes(y = .fitted, color = "Fitted values"))



#--------------------------------------------------------------------------------

recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)


recent_production |> 
  model(reg_season = TSLM(Beer ~ trend() + season())) |> 
  augment() |> 
  ggplot(aes(x = Quarter, y = Beer)) +
  geom_line(aes(color = "Beer production"), linewidth = .7) +
  geom_line(aes(y = .fitted, color = "Fitted values")) +
  scale_color_manual(values = c("#77003c", "#32af6f")) +
  labs(y = "Megaliters",
       x = "") +
  easy_plot_legend_size(13) +
  easy_add_legend_title("") +
  easy_y_axis_title_size(13) +
  easy_legend_at("top")

#--------------------------------------------------------------------------------

air <- read_csv("air_passengers.csv")


air_ts <- air |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date)


air_ts |> 
  model(reg = TSLM(passengers ~ trend() + season())) |> 
  report()

air_ts |> 
  model(reg = TSLM(passengers ~ trend() + season())) |>
  augment() |> 
  ggplot(aes(x = date, y = passengers)) +
  geom_line(aes(color = "Air passengers")) +
  geom_line(aes(y = .fitted, color = "Fitted values"))


air_ts |> 
  model(reg = TSLM(log(passengers) ~ trend() + season())) |>
  augment() |> 
  ggplot(aes(x = date, y = passengers)) +
  geom_line(aes(color = "Air passengers")) +
  geom_line(aes(y = .fitted, color = "Fitted values"))



air_ts |> 
  model(reg = TSLM(log(passengers) ~ trend() + fourier(K = 6))) |>
  augment() |> 
  ggplot(aes(x = date, y = passengers)) +
  geom_line(aes(color = "Air passengers")) +
  geom_line(aes(y = .fitted, color = "Fitted values"))



air_ts |> 
  model(reg = TSLM(log(passengers) ~ trend() + fourier(K = 6))) |>
  forecast(h = 24) |> 
  autoplot(air_ts, level = 95)

#--------------------------------------------------------------------------------



#--------------------------------------------------------------------------------

#-- Unit-root testing:

dat_ts |> 
  features(cons, unitroot_kpss)

dat_ts |> 
  features(inc, unitroot_kpss)


dat_ts <- dat_ts |> 
  mutate(dcons = difference(cons, lag = 1),
         dinc = difference(inc, lag = 1))


dat_ts |> 
  features(dcons, unitroot_kpss)

dat_ts |> 
  features(dinc, unitroot_kpss)


#--------------------------------------------------------------------------------



dat_ts |> 
  model(ols = TSLM(dcons ~ dinc)) |> 
  augment() |>
  ACF(.resid) |> 
  autoplot()


dat_ts |> 
  model(ols = TSLM(dcons ~ dinc)) |> 
  augment() |>
  PACF(.resid) |> 
  autoplot()

dat_ts |> 
  model(ols = TSLM(dcons ~ dinc)) |> 
  augment() |> 
  features(.resid, ljung_box, lag = 10)

reg_fit <- dat_ts |> 
  model(reg = ARIMA(dcons ~ dinc))


reg_fit |> 
  augment() |> 
  autoplot(.resid)

reg_fit |> 
  augment() |> 
  features(.resid, ljung_box, lag = 10, dof = 2)


#--------------------------------------------------------------------------------


reg_fit2 <- dat_ts |> 
  model(reg2 = ARIMA(cons ~ inc + pdq(1, 1, 0) + PDQ(0, 0, 0)))


reg_fit2 |> 
  report()


reg_fit2 |> 
  augment() |> 
  features(.resid, ljung_box, lag = 10, dof = 1)



#--------------------------------------------------------------------------------


dat_ts_filter <- dat_ts |> 
  filter_index(. ~ "2019-12-01")


dat_ts_filter |> 
  ggplot(aes(x = date, y = cons)) +
  geom_line(aes(color = "Real personal consumption")) +
  geom_line(aes(y = inc, color = "Real disposable income")) +
  scale_color_manual(values = c("#2a5fac", "#ec5c11")) +
  labs(y = "Billions of dollars (2017)",
       x = "",
       title = "U.S. real personal consumption and real disposable income",
       subtitle = "01/2007-12/2019",
       caption = "Source: U.S. Bureau of Economic Analysis (BEA).") +
  easy_legend_at("top") +
  easy_add_legend_title("") +
  easy_plot_legend_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_caption_size(13)


reg_fit3 <- dat_ts_filter |> 
  model(reg = ARIMA(dcons ~ dinc))

reg_fit3 |> 
  gg_tsresiduals()


reg_fit3 |> 
  augment() |> 
  autoplot(.resid)

reg_fit |> 
  augment() |> 
  features(.innov, ljung_box, lag = 10, dof = 5)



#----------------------------------------------------------------------

fit <- us_change |>
  model(ARIMA(Consumption ~ Income))
report(fit)


augment(fit) |>
  features(.innov, ljung_box, dof = 3, lag = 8)
