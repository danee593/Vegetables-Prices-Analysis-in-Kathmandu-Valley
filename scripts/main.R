install.packages("lmtest")
# Required Libraries
library(readr)
library(tidyverse)
library(fpp3)
library(lmtest)
library(ggplot2)

# Plot-theme
theme_set(theme_bw())


# Load data source
veggies <- read_csv("data_sources/kalimati_tarkari_dataset.csv", 
                    col_types = cols(Date = col_date(format = "%Y-%m-%d")))

# Get only 5 keys and 1 variable.
veggies <- veggies %>% 
  filter(Unit == "Kg") %>% 
  filter(Commodity %in% c("Tomato Big(Nepali)", "Potato Red",
                          "Onion Dry (Indian)", "Carrot(Local)",
                          "Cabbage(Local)")) %>%
  select(Commodity, Date, Average)

# Convert to tsibble
veggies_ts <- veggies %>% 
  as_tsibble(key = Commodity,
             index = Date)

# Plot timeline of average price per commodity
veggies_ts %>% 
  autoplot()

# Start by looking at the Cabbage (Month is the index and Commodity is the key)
Cabbage <- veggies_ts %>%
  filter(Commodity == "Cabbage(Local)")

# Fill the gaps & missing values (otherwise decomposition do not work)
Cabbage_filled <- Cabbage_filled %>%
  fill(Average)

Cabbage_filled %>% autoplot(Average)
# Looking at the plot we can see that there is a slightly positive trend and seasonality going on.
# looking at it, it looks like the cabbage is more expensive in the beginning of the year (possibly due to weather/sparsity.)
# We can also see that this trend is additive rather than multiplicative, suggesting that we might not need a mathematical transformation)
# Box-cox is used to normalize skewed data and get heterogeneity, we can check how heterogenic the data is to decide whether or not a box-cox is a good choice.

Cabbage_filled %>% ACF(Average) %>% autoplot()

# Looking at the ACF we see a positive autocorrelation that decreases over time.It is hard to see any seasonality by this plot.
# But we see that this data is trended because the ACF of a trended time series tends to have positive values that slowly decrease as the lags increase.

# Now we can check if a box-cox would do any difference, we can first see if there is any heteroskedasticity
# the breusch-pagan test suggests that there is evidence of heteroscedasticity
lm_model <- lm(Average ~ Date, data = Cabbage_filled)
bptest(lm_model)

# Box-cox transformation - let´s check with and without box-cox
Cabbage_filled %>%
  features(Average, features = guerrero) %>%
  pull(lambda_guerrero)-> lambda

Cabbage_filled %>% autoplot(Average)

Cabbage_filled %>%
  autoplot(box_cox(Average, lambda))

Cabbage_filled <- Cabbage_filled %>%
  mutate(Average_transformed = box_cox(Average, lambda))

# So we can see that the box-cox transformation actually did account for some variance.
# stabilize the variance of data. It achieveshttp://127.0.0.1:46763/graphics/eae0aabb-fca1-42fc-af17-adb617189384.png this by applying a power transformation to the data, 
# effectively adjusting the data to make its variance more constant, when we apply the Box-Cox transformation 
# to the cabbage data, it becomes more homoscedastic.

# Decomposition let's try different decomposition models and assess which one is the best.
# X11 only works on monthly or quarterly data - so those we can forget about - SEATS does not work either
Cabbage_filled %>%
  model(STL1 = STL(Average),
        STL2 = STL(Average ~ trend(window = 7) + season(window = 5), robust = TRUE),
        fixed = STL(Average ~ season(window = "periodic"), robust = TRUE))

  components() -> dcmp
  
dcmp %>% as_tsibble() %>% autoplot(trend)
dcmp %>% as_tsibble() %>% autoplot(season_adjust)


dcmp %>% 
  autoplot(trend)

dcmp %>% 
  autoplot(season_year)

autoplot(dcmp)

# Checking which method is best for the cabbage time series
fitCabbage <- Cabbage_filled %>% 
  model(STL2 = STL(Average ~ trend(window = 7) + season(window = 5), robust = TRUE),
        naive = NAIVE(Average),
        snaive = SNAIVE(Average),
        mean = MEAN(Average),
        RW = RW(Average ~ drift()))

fitCabbage %>%
  augment() %>% ACF(.resid) %>% autoplot()



