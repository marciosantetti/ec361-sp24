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


dat <- read_csv("fredgraph2.csv") |> 
  clean_names() |> 
  rename(cons = pcec_pch,
         inc = a067rp1q027sbea)

dat_ts <- dat |> 
  mutate(date = yearquarter(date)) |> 
  as_tsibble(index = date)


dat_ts_f <- dat_ts |> 
  filter_index(. ~ "2019-10-01")

dat_ts |> 
  model(reg = ARIMA(cons ~ inc)) |> 
  augment() |> 
  features(.innov, ljung_box, dof = 2, lag = 10)
