library(tidyverse)
library(janitor)
library(fpp3)
library(hrbrthemes)
library(RcppRoll)


theme_set(theme_ipsum())



#############################################################################


gdppc <- read_csv("A939RC0Q052SBEA.csv") |> 
  clean_names() |> 
  rename(gdppc = a939rc0q052sbea)


gdppc |> 
  ggplot(aes(x = date, y = gdppc)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(y = "Dollars",
       x = "",
       title = "U.S. nominal GDP per capita",
       subtitle = "1947Q1–2023Q4",
       caption = "U.S. Bureau of Economic Analysis")


gdp <- read_csv("GDP.csv") |> 
  clean_names()

gdp |> 
  ggplot(aes(x = date, y = gdp)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(y = "Billions of dollars",
       x = "",
       title = "U.S. nominal GDP",
       subtitle = "1947Q1–2023Q4",
       caption = "U.S. Bureau of Economic Analysis")



###########################################################################



wages <- read_csv("LES1252881500Q.csv") |> 
  clean_names() |> 
  rename(nominal_wages = les1252881500q)


wcpi <- read_csv("wages_cpi.csv") |> 
  clean_names() |> 
  rename(nominal_wages = les1252881500q,
         cpi = cpiaucsl)


wcpi |> 
  ggplot(aes(x = date, y = nominal_wages)) +
  geom_line()



wcpi |> 
  mutate(real_wages = nominal_wages / cpi) |> 
  ggplot(aes(x = date, y = real_wages)) +
  geom_line()

###########################################################################

aus_production <- aus_production |> 
  clean_names()


aus_production |> 
  autoplot(electricity)

aus_production |> 
  autoplot(electricity) +
  scale_y_continuous(labels = comma) +
  labs(title = "Australian electricity production",
       subtitle = "1949Q1–2010Q2",
       y = "Gigawatt hours",
       x = "",
       caption = "Hyndman & Athanasopoulos (2021)")

aus_production |> 
  model(stl = classical_decomposition(electricity)) |> 
  components(stl) |> 
  autoplot()


aus_production |> 
  model(cd = classical_decomposition(electricity)) |> 
  components(cd) |> 
  ggplot(aes(x = quarter, y = electricity)) +
  geom_line() +
  geom_line(aes(y = trend), color = "#860b35", linewidth = 1) +
  labs(title = "Original series + Trend-Cycle component",
       y = "Gigawatt hours",
       x = "",
       caption = "Hyndman & Athanasopoulos (2021)")


aus_production |> 
  select(quarter, electricity) |> 
  mutate(ma4 = roll_mean(electricity, n = 4, align = "center", fill = NA),
         ma2_4 = roll_mean(ma4, n = 2, align = "right", fill = NA),
         detrended = electricity - ma2_4) |> 
  ggplot(aes(x = quarter, y = ma2_4)) +
  geom_line() +
  geom_line(aes(y = detrended), color = "red")


aus_production |> 
  model(stl = STL(beer)) |> 
  components(stl) |> 
  autoplot()





global_economy |> 
  filter(Country == "United States") |> 
  View()



global_economy |>
  filter(Country == "Australia") |>
  select(Exports) |> 
  mutate(ma5_exports = roll_mean(Exports, n = 5, align = "center", fill = NA)) |> 
  autoplot(Exports) +
  geom_line(aes(y = ma5_exports), color = "orange")


us_employment |> 
  filter_index("1980-01-01" ~ .) |> 
  count(Title) |> 
  View()


us_private_service <- us_employment |> 
  filter_index("1980-01-01" ~ .) |> 
  filter(Title == "Private Service-Providing")




us_private_service <- us_private_service |> 
  mutate(ma3 = roll_mean(Employed, n = 3, align = "center", fill = NA),
         ma5 = roll_mean(Employed, n = 5, align = "center", fill = NA),
         ma9 = roll_mean(Employed, n = 9, align = "center", fill = NA),
         ma15 = roll_mean(Employed, n = 15, align = "center", fill = NA))









us_private_service <- us_private_service |> 
  select(-Series_ID)

ma3 <- us_private_service |> 
  autoplot(Employed) +
  geom_line(aes(y = ma3), color = "#860b35") +
  scale_y_continuous(labels = comma) +
  labs(title = "Private services: Number of employed persons, Jan 1980 – Sep 2019",
       subtitle = "Black: Original series; Red: 3-month moving average",
       y = "Persons employed",
       x = "")   +
  easy_y_axis_title_size(14)


ma5 <- us_private_service |> 
  autoplot(Employed) +
  geom_line(aes(y = ma5), color = "#860b35") +
  scale_y_continuous(labels = comma) +
  labs(subtitle = "Red: 5-month moving average",
       y = "",
       x = "") 

ma9 <- us_private_service |> 
  autoplot(Employed) +
  geom_line(aes(y = ma9), color = "#860b35") +
  scale_y_continuous(labels = comma) +
  labs(subtitle = "Red: 9-month moving average",
       y = "",
       x = "") 

ma15 <- us_private_service |> 
  autoplot(Employed) +
  geom_line(aes(y = ma15), color = "#860b35") +
  scale_y_continuous(labels = comma) +
  labs(subtitle = "Red: 15-month moving average",
       y = "",
       x = "") 

(ma3 | ma5) /(ma9 | ma15)


#####



aus_production |>
  filter(year(quarter) >= 1992) |>
  select(quarter, beer) |> 
  mutate(
    `4-MA` = slider::slide_dbl(beer, mean,
                               .before = 1, .after = 2, .complete = TRUE),
    `2x4-MA` = slider::slide_dbl(`4-MA`, mean,
                                 .before = 1, .after = 0, .complete = TRUE),
    ma4 = roll_mean(beer, n = 4, align = "center", fill = NA),
    ma2 = roll_mean(ma4, n = 2, align = "right", fill = NA)) |> View()




#######


air <- read_csv("https://raw.githubusercontent.com/selva86/datasets/master/AirPassengers.csv")


air |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date) |> 
  autoplot(log(value))
