library(tidyverse)
library(fpp3)
library(hrbrthemes)
library(janitor)

theme_set(theme_ipsum_rc())


d <- read_csv("epwt7.csv")


d_ts <- d |> 
  as_tsibble(index = Year,
             key = c(Country, Countrycode, Continent, Subregion))



unrate <- read_csv("unrate.csv")




unrate <- unrate |> 
  mutate(period = yearmonth(DATE)) |> 
  as_tsibble(index = period)


unrate |> 
  autoplot()


unrate |> 
  model(stl = STL(UNRATE)) |> 
  components() |> 
  autoplot()


fit <- unrate |> 
  model(trend_model = TSLM(UNRATE ~ trend()))

fit |> 
  forecast(h = "6 months") |> 
  autoplot(unrate)



####


gdppc <- read_csv("gdppc.csv")


gdppc <- gdppc |> 
  clean_names() |> 
  rename(gdppc = a939rx0q048sbea)


gdppc <- gdppc |> 
  mutate(period = yearquarter(date)) |> 
  as_tsibble(index = period)


fit <- gdppc |> 
  select(period, gdppc) |> 
  model(trend_model = TSLM(gdppc ~ trend()))


fit |> 
  forecast(h = "2 years") |> 
  autoplot(gdppc)
