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

okun <- read_csv("okun.csv") |> 
  clean_names() |> 
  rename(gdp = gdpc1_pc1,
         unrate = unrate_chg)

okun_ts <- okun |> 
  mutate(date = year(date)) |> 
  mutate(gdp = as.double(gdp),
         unrate = as.double(unrate)) |> 
  as_tsibble(index = date) |> 
  filter_index(1950 ~ 2007)


okun_ts |> 
  autoplot(gdp) +
  labs(title = "U.S. annual real GDP growth",
       subtitle = "1970—2023",
       y = "% change from a year ago",
       x = "",
       caption = "Source: U.S. Bureau of Economic Analysis.") +
  easy_y_axis_title_size(13) +
  easy_plot_caption_size(13)


okun_ts |> 
  autoplot(unrate) +
  labs(title = "U.S. annual change in unemployment",
       subtitle = "19—2023",
       y = "year-to-year change",
       x = "",
       caption = "Source: U.S. Bureau of Labor Statistics.") +
  easy_y_axis_title_size(13) +
  easy_plot_caption_size(13)


#--------------------------------------------------------------------------------


okun_ts |> 
  features(gdp, unitroot_kpss)

okun_ts |> 
  features(unrate, unitroot_kpss)


#--------------------------------------------------------------------------------



okun_reg <- okun_ts |> 
  model(reg = TSLM(unrate ~ gdp))

okun_reg |> 
  augment() |> 
  select(.innov) |> 
  autoplot() +
  geom_point() +
  labs(y = "Residual term",
       x = "") +
  easy_y_axis_title_size(13)




okun_reg |> 
  augment() |> 
  features(.innov, ljung_box, lag = 10)

okun_reg |> 
  gg_tsresiduals()

okun_reg |> 
  augment() |> 
  ACF(.innov) |> 
  autoplot()

okun_reg |> 
  augment() |> 
  PACF(.innov) |> 
  autoplot()
  

#-----------------------------------------------------------------------



okun_arima <- okun_ts |> 
  model(arima_reg = ARIMA(unrate ~ gdp))

okun_arima |> 
  report()

okun_arima |> 
  augment() |> 
  ggplot(aes(x = date, y = gdp, color = "Gross Domestic Product"))


okun_arima |> 
  augment() |> 
  features(.innov, ljung_box, lag = 10, dof = 1)







#--------------------------------------------------------------------------------



okun_future <- okun_ts |> 
  new_data(n = 5) |> 
  mutate(unrate = mean(okun_ts$unrate))


okun_arima |> 
  forecast(new_data = okun_future) |> 
  autoplot(okun_ts)


#--------------------------------------------------------------------------------