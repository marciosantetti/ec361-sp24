library(tidyverse)
library(janitor)
library(fpp3)
library(hrbrthemes)
library(RcppRoll)
library(scales)


theme_set(theme_ipsum())


######################################


us_private_service <- us_employment |> 
  filter_index("1980-01-01" ~ .) |> 
  filter(Title == "Private Service-Providing")



us_private_service <- us_private_service |> 
  select(-Series_ID)


us_private_service |> 
  model(cl = classical_decomposition(Employed)) |> 
  components() |> 
  autoplot() +
  scale_y_continuous(labels = comma)


us_private_service |> 
  model(stl = STL(Employed)) |> 
  components() |> 
  autoplot() +
  scale_y_continuous(labels = comma)




#####################################


aus_production <- aus_production |> 
  clean_names()


aus_production |> 
  model(stl = STL(beer)) |> 
  components(stl) |> 
  autoplot()


aus_production |> 
  model(cl = classical_decomposition(beer)) |> 
  components(stl) |> 
  autoplot()

