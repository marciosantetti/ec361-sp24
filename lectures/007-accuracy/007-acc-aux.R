library(tidyverse)
library(fpp3)
library(hrbrthemes)
library(scales)
library(ggeasy)
library(MetBrewer)
library(janitor)
library(patchwork)


theme_set(theme_ipsum())


#############################################


job <- read_csv("job_openings.csv") |> 
  clean_names()

job <- job |> 
  rename(openings = jtujol)


job_ts <- job |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date)


job_ts |> 
  autoplot()



#############################################



job_train <- job_ts |> 
  filter_index(. ~ "2018-12-01")

job_test <- job_ts |> 
  filter_index("2019-01-01" ~ .)


job_fit <- job_train |> 
  model(mean_model = MEAN(openings),
        naive_model = NAIVE(openings),
        snaive_model = SNAIVE(openings),
        drift_model = RW(openings ~ drift()),
        snaive_with_drift = RW(openings ~ drift() + lag(12)))


job_fc <- job_fit |> 
  forecast(h = 24)


## All:

job_fc |> 
  autoplot(job_ts |> filter_index(. ~ "2020-12-01"), level = NULL) +
  guides(colour = guide_legend(title = "Forecast")) +
  scale_color_met_d("Austria")


## Mean model:

job_fc |> 
  filter(.model == "mean_model") |> 
  autoplot(job_ts |> filter_index(. ~ "2020-12-01"), level = NULL, color = "#ed4d5e", linewidth = 0.8) 

job_fc |> 
  filter(.model == "mean_model") |> 
  autoplot(job_ts |> filter_index(. ~ "2020-12-01"), level = 95, color = "#ed4d5e", linewidth = 0.8)


## Naive model:

job_fc |> 
  filter(.model == "naive_model") |> 
  autoplot(job_ts |> filter_index(. ~ "2020-12-01"), level = NULL, color = "#b98ae0", linewidth = 0.8) 

job_fc |> 
  filter(.model == "naive_model") |> 
  autoplot(job_ts |> filter_index(. ~ "2020-12-01"), level = 95, color = "#b98ae0", linewidth = 0.8)


## Seasonal naive model:


job_fc |> 
  filter(.model == "snaive_model") |> 
  autoplot(job_ts |> filter_index(. ~ "2020-12-01"), level = NULL, color = "#6f7e90", linewidth = 0.8) 

job_fc |> 
  filter(.model == "snaive_model") |> 
  autoplot(job_ts |> filter_index(. ~ "2020-12-01"), level = 95, color = "#6f7e90", linewidth = 0.8)


## Drift model:

job_fc |> 
  filter(.model == "drift_model") |> 
  autoplot(job_ts |> filter_index(. ~ "2020-12-01"), level = NULL, color = "#fcb2d9", linewidth = 0.8) 

job_fc |> 
  filter(.model == "drift_model") |> 
  autoplot(job_ts |> filter_index(. ~ "2020-12-01"), level = 95, color = "#fcb2d9", linewidth = 0.8)


## Seasonal naive with drift:


job_fc |> 
  filter(.model == "snaive_with_drift") |> 
  autoplot(job_ts |> filter_index(. ~ "2020-12-01"), level = NULL, color = "#ca861b", linewidth = 0.8) 

job_fc |> 
  filter(.model == "snaive_with_drift") |> 
  autoplot(job_ts |> filter_index(. ~ "2020-12-01"), level = 95, color = "#ca861b", linewidth = 0.8)

job_fc |> 
  accuracy(job_ts)

job_fc |> 
  accuracy(job_ts) |> 
  select(.model, MAE, RMSE, MAPE, MASE) |> 
  arrange(RMSE)


#########################################################################################################




air <- read_csv("air_passengers.csv")


air_ts <- air |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date)



air_train <- air_ts |> 
  filter_index(. ~ "1958-07-01")


air_fit <- air_train |> 
  model(snaive_model_drift_log = RW(log(passengers) ~ drift() + lag(12)),
        snaive_model_log = SNAIVE(log(passengers)),
        snaive_model_drift = RW(passengers ~ drift() + lag(12)),
        snaive_model = SNAIVE(passengers)) 


air_fc <- air_fit |> 
  forecast(h = 29)



## All:


air_fc |> 
  autoplot(air_ts, level = NULL, linewidth = 0.8) +
  guides(colour = guide_legend(title = "Forecast")) +
  scale_color_met_d("Austria")


## Log-transformed:

air_fc |> 
  filter(.model == "snaive_model_drift_log") |> 
  autoplot(air_ts, level = 95, color = "#ed4d5e", linewidth = 0.8)



## Not log-transformed:

air_fc |> 
  filter(.model == "snaive_model_drift") |> 
  autoplot(air_ts, level = 95, color = "#fcb2d9", linewidth = 0.8)



air_fc |> 
  accuracy(air_ts) |> 
  select(.model, MAPE, MASE)


air_fc |> 
  accuracy(air_ts, list(skill = skill_score(CRPS)))
