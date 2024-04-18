# Required Libraries
library(readr)
library(tidyverse)
library(fpp3)


# Load data source
veggies <- read_csv("data_sources/kalimati_tarkari_dataset.csv", 
                    col_types = cols(Date = col_date(format = "%Y-%m-%d")))

# Get only 5 keys and 1 variable.
veggies <- veggies |>
  filter(Unit == "Kg") |>
  filter(Commodity %in% c("Tomato Big(Nepali)", "Potato Red",
                          "Onion Dry (Indian)", "Carrot(Local)",
                          "Cabbage(Local)")) |>
  select(Commodity, Date, Average)

# Convert to tsibble
veggies_ts <- veggies |>
  as_tsibble(key = Commodity,
             index = Date)

# Plot timeline of average price per commodity
veggies_ts |>
  autoplot()

# potatoes add NAs to gaps and fill them with previous value.
potato <- veggies_ts |>
  filter(Commodity == "Potato Red") |>
  fill_gaps(.full = TRUE) |>
  fill(Average)

# time plot
potato |>
  autoplot(Average) +
  labs(y="Nepalese Rupee",
       title="Average Price of Potato Red in Nepal") +
  theme_bw()

# ggseason plot
potato |>
  gg_season(Average) +
  labs(y="Nepalese Rupee",
       title="Average Price of Potato Red in Nepal") +
  theme_bw()
