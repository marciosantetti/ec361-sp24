library(tidyverse)
library(fpp3)
library(hrbrthemes)
library(scales)
library(ggeasy)
library(MetBrewer)
library(janitor)
library(patchwork)


theme_set(theme_ipsum())






air <- read_csv("air_passengers.csv")


air_ts <- air |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date)



air_snaive <- air_ts |> 
  model(snaive_model = RW(log(passengers) ~ drift() + lag(12))) 


air_snaive |> 
  augment() |> 
  View()

air_snaive |> 
  augment() |> 
  ggplot(aes(x = date, y = passengers)) +
  geom_line(aes(color = "Raw data")) +
  geom_line(aes(y = .fitted, color = "Fitted values")) +
  scale_color_met_d("Gauguin")



air_snaive |> 
  gg_tsresiduals()

air_snaive_fc <- air_snaive |> 
  forecast(h = 24)

p1 <- air_snaive_fc |> 
  autoplot(air_ts, level = 95, linewidth = .9, color = "#421a92")



### No transf:


air_snaive_no <- air_ts |> 
  model(snaive_model = RW(passengers ~ drift() + lag(12))) 

air_snaive_no |> 
  augment() |> 
  ggplot(aes(x = date, y = passengers)) +
  geom_line(aes(color = "Raw data")) +
  geom_line(aes(y = .fitted, color = "Fitted values")) +
  scale_color_met_d("Gauguin")

air_snaive_no |> 
  gg_tsresiduals()

air_snaive_no_fc <- air_snaive_no |> 
  forecast(h = 24)

p2  <- air_snaive_no_fc |> 
  autoplot(air_ts, level = 95, linewidth = .9, color = "#421a92")


p1 / p2



########### 


## Decomposition:

job <- read_csv("job_openings.csv") |> 
  clean_names()

job <- job |> 
  rename(openings = jtujol)


job_ts <- job |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date)


job_ts |> 
  autoplot()



job_comp <- job_ts |> 
  model(stl = STL(openings)) |> 
  components()

job_decomp <- job_ts |> 
  model(stlf = decomposition_model(STL(openings),
                                   RW(season_adjust ~ drift()),
                                   SNAIVE(season_year)))

job_decomp |> 
  forecast(h = 24) |> 
  autoplot(job_ts, level = 95)


job_decomp |> 
  gg_tsresiduals()

job_decomp |> 
  augment() |> 
  View()
