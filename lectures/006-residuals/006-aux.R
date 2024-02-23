library(tidyverse)
library(janitor)
library(fpp3)
library(scales)
library(hrbrthemes)
library(ggeasy)
library(MetBrewer)


theme_set(theme_ipsum())


#############################################


## US-Euro exchange rate:

dex <- read_csv("DEXUSEU.csv") |> 
  clean_names() |> 
  rename(exch = dexuseu)


dex_ts <- dex |> 
  mutate(exch = as.double(exch)) |> 
  as_tsibble(index = date) |> 
  mutate(day = row_number()) |>
  update_tsibble(index = day, regular = TRUE)


dex_ts2 <- dex_ts |> 
  filter(date > "2023-01-01")

dex_ts2 |> 
  autoplot(linewidth = .7) +
  labs(title = "U.S. Dollars to Euro spot exchange rate",
                               subtitle = "Jan 2023 – Feb 2024 (daily)",
                               caption = "Source: U.S. Federal Reserve System.",
                               y = "U.S. Dollars to 1 Euro",
                               x = "")

dex_fit <- dex_ts2 |> 
  model(naive_model = NAIVE(exch))


dex_fc <- dex_fit |> 
  forecast(h = 30)


dex_fc |> 
  autoplot(dex_ts2, color = "#974c90", linewidth = 1.1) + 
  labs(title = "U.S. Dollars to Euro spot exchange rate, Jan 2023 – Feb 2024 (daily)",
       subtitle = "30-day forecast using the naïve method",
       caption = "Source: U.S. Federal Reserve System.",
       y = "U.S. Dollars to 1 Euro",
       x = "") +
  easy_plot_subtitle_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_caption_size(13)
    



### Residual extraction:

dex_aug <- dex_fit |> 
  augment()


dex_aug |> 
  ggplot(aes(x = day, y = exch)) +
  geom_line(aes(color = "Original data"), linewidth = 0.6) +
  geom_line(aes(y = .fitted, color = "Fitted/Estimated values"), linewidth = 0.9) +
  scale_color_met_d("Veronese") +
  easy_add_legend_title("Series") +
  easy_plot_legend_size(13)


dex_aug |> 
  autoplot(.innov, color = "#1a4146", linewidth = .6) +
  labs(title = "Residuals from the naïve method",
       y = "Residuals") +
  easy_plot_subtitle_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_caption_size(13)


dex_aug |> 
  ggplot(aes(x = .innov)) +
  geom_histogram(color = "white", fill = "#1a4146", alpha = 0.4) +
  labs(title = "Histogram of residuals",
       y = "Count",
       x = "Residuals") +
  easy_plot_subtitle_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_caption_size(13)


dex_aug |> 
  ACF(.innov) |> 
  autoplot() +
  labs(title = "ACF plot of model residuals",
       y = "ACF") +
  easy_plot_subtitle_size(13) +
  easy_y_axis_title_size(13) +
  easy_plot_caption_size(13)



dex_fit |> 
  gg_tsresiduals()


dex_aug |> 
  features(.innov, box_pierce, lag = 10)

dex_aug |> 
  features(.innov, ljung_box, lag = 10)



dex_fc |> 
  hilo()
