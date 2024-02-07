library(fpp3)
library(tidyverse)
library(tsibble)


un <- read_csv("unrate.csv") |> 
  clean_names()

un <- un |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date)


un |> 
  autoplot(unrate, linetype = 2)

un |> 
  gg_season(unrate)

un |> 
  gg_subseries(unrate)


retail <- read_csv("RSXFS.csv") |> 
  clean_names()

retail <- retail |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date)

retail |> 
  gg_subseries(rsxfs)


####



cpi <- read_csv("CPILFESL.csv") |> 
  clean_names()


cpi |> 
  ggplot(aes(x = date, y = cpilfesl_pc1)) +
  geom_point() +
  scale_x_date(date_breaks = "10 years", date_labels = "%b %Y") +
  labs(title = "U.S. inflation rate: Core CPI",
       subtitle = "Jan 1958 – Dec 2023",
       caption =  "Source: U.S. Bureau of Labor Statistics.",
       y = "Change from a year ago",
       x = "")



####


air <- read_csv("https://raw.githubusercontent.com/selva86/datasets/master/AirPassengers.csv")


air |> 
  #mutate(date = yearmonth(date)) |> 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  scale_x_date(date_breaks = "2 years", date_labels = "%b %Y") +
  labs(title = "International airline passengers",
       subtitle = "Jan 1949 – Dec 1960",
       caption = "Source: Brown (1962).",
       x = "",
       y = "Thousands")


####

tbill <- read_csv("TB3MS.csv") |> 
  clean_names()


tbill |> 
  mutate(tb3ms = as.double(tb3ms)) |> 
  filter(date >= "1970-01-01") |> 
  ggplot(aes(x = date, y = tb3ms)) +
  geom_line(linewidth = 0.5) +
  scale_x_yearquarter(breaks = "5 years") +
  scale_y_continuous(breaks = seq(0, 175.5, 2.5),
                     labels = scales::percent_format(scale = 1)) +
  labs(title = "U.S. 3-month Treasury Bill market rate",
       subtitle = "1970Q1–2023Q4",
       x = "",
       y = "",
       caption = "Source: U.S. Federal Reserve System.")



###

air |> 
  mutate(date = yearmonth(date)) |> 
  as_tsibble(index = date) |> 
  gg_season(labels = "both") +
  labs(title = "Seasonal plot: International airline passengers",
       subtitle = "Jan 1949 – Dec 1960",
       caption = "Source: Brown (1962).",
       x = "",
       y = "Thousands")



###


vic_elec |>
  filter(year(Time) == 2014) |>
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point(alpha = 0.4, color = "#8f514a") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Temperature (degrees Celsius)",
       y = "Electricity demand (GW)",
       title = "Electricity demand vs. temperature",
       subtitle = "Victoria (AUS), 2014",
       caption = "Source: Hyndman and Athanasopoulos (2021)")


tbill |> 
  mutate(tb3ms = as.double(tb3ms)) |> 
  filter(date >= "1970-01-01") |> 
  mutate(date = yearquarter(date)) |> 
  as_tsibble(index = date) |> 
  gg_season() +
  labs(title = "Seasonal plot: U.S. 3-month Treasury Bill market rate",
       subtitle = "1970Q1–2023Q4",
       x = "",
       y = "",
       caption = "Source: U.S. Federal Reserve System.")


###

unrate |> 
  filter(date >= "1958-01-01") |> 
  add_column(cpi$cpilfesl_pc1) |> 
  rename(cpi = `cpi$cpilfesl_pc1`) |> 
  ggplot(aes(x = unrate, y = cpi)) +
  geom_point()
  