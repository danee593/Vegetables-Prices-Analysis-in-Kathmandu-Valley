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

