# Required Libraries
library(readr)
library(tidyverse)
library(fpp3)
library(gridExtra)
library(readxl)


# Load data source
veggies <- read_csv("data_sources/kalimati_tarkari_dataset.csv", 
                    col_types = cols(Date = col_date(format = "%Y-%m-%d")))

cpi <- read_excel("data_sources/Consumer Price Index, All items.xlsx", 
                  skip = 7)

# tidy up cpi
base_index <- cpi$Index[1]
cpi <- cpi |>
  mutate(date = yearmonth(cpi$Units),
         Index = Index/base_index) |>
  select(c(date, Index))
  


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

potato |> autoplot()

# time plot
potato |>
  autoplot(Average) +
  labs(y="Nepalese Rupee",
       title="Average Price of Potato Red in Nepal") +
  theme_bw()

# ggseason plot
potato |>
  gg_season(Average, labels = "right") +
  labs(y="Nepalese Rupee",
       title="Average Price of Potato Red in Nepal") +
  theme_bw()

potato_month <- potato |>
  as_tibble() |>
  mutate(Date = yearmonth(Date)) |>
  group_by(Date, Commodity) |>
  summarize(Average = mean(Average)) |>
  as_tsibble(key = Commodity,
             index = Date)

potato_month |>
  gg_subseries() +
  labs(y="Nepalese Rupee",
       title="Average Price of Potato Red in Nepal") +
  theme_bw()


potato |>
  ACF(Average, lag_max = 365) |> autoplot()

#box-cox transformation
lambda_guerrero <- potato |>
  features(Average, features = guerrero) |>
  pull(lambda_guerrero)

box_cox_plot <- potato |>
  autoplot(box_cox(Average, lambda_guerrero)) +
  labs(y="Box-cox transformation",
       title="Box-Cox Transformation Average Price of Potato Red in Nepal") +
  theme_bw()

simple_plot <- potato |>
  autoplot(Average) +
  labs(y="Nepalese Rupee",
       title="Average Price of Potato Red in Nepal") +
  theme_bw()

grid.arrange(box_cox_plot, simple_plot, ncol = 1)

potato <- potato |>
  mutate(Average_bcx = box_cox(Average, lambda_guerrero))


# Series needs inflation adjustment

potato_constant_prices <- potato |>
  mutate(ym = yearmonth(Date)) |>
  inner_join(cpi, by = join_by(ym == date)) |>
  mutate(Average_constant = Average / Index) |>
  select(Date, Commodity, Average_constant)

p1 <-potato_constant_prices |>
  autoplot() +
  labs(y="Nepalese Rupees",
       title="Average Price of Potato Red, 2013 prices in Nepal") +
  theme_bw()

lambda_guerrero_constant_prices <- potato_constant_prices |>
  features(Average_constant, features = guerrero) |>
  pull(lambda_guerrero)

potato_constant_prices <- potato_constant_prices |>
  mutate(Average_constant_bcx = box_cox(Average_constant, lambda_guerrero_constant_prices))

p2 <- potato_constant_prices |>
  autoplot(Average_constant_bcx) +
  labs(y="Box Cox Transformation",
       title="Box-Cox Transformation Average Price, 2013 prices of Potato Red in Nepal") +
  theme_bw()

grid.arrange(p1, p2, ncol = 1)
# STL decomposition


default_stl_potato <- potato %>%
  model(stl = STL(Average_bcx))

head(components(default_stl_potato),5)

potato %>%
  autoplot(Average_bcx, color='gray') +
  autolayer(components(default_stl_potato), trend, color='red') +
  ylab("Box Cox Transformation of Average Price") +
  ggtitle("Box-Cox Transformation Average Price of Potato Red in Nepal") +
  theme_bw()

components(default_stl_potato) %>% autoplot() + xlab("Year")


stl_year_month <- potato %>%
  model(stl = STL(Average_bcx ~ season(period = "1 year") + 
                    season(period = "1 month")))

head(components(stl_year_month),5)


components(stl_year_month) %>% autoplot() + xlab("Year")



