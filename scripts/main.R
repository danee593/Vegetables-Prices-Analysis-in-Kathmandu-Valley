# Required Libraries
install.packages("lmtest")
library(readr)
library(tidyverse)
library(fpp3)
library(lmtest)
library(ggplot2)
library(readxl)

# Plot-theme
theme_set(theme_bw())

# Load data source
veggies <- read_csv("data_sources/kalimati_tarkari_dataset.csv", 
                    col_types = cols(Date = col_date(format = "%Y-%m-%d")))

cpi <- read_excel("~/forecasting_from_hell/data_sources/Nepal CPI Trading Economics.xlsx")

# Tidy up CPI
base_index <-cpi$CPI

cpi <- cpi %>% 
  mutate(Date = yearmonth(Date),
         Index = CPI/base_index) %>% 
  select(Date, Index)

# Get only 5 keys and 1 variable
veggies <- veggies %>% 
  filter(Unit == "Kg") %>% 
  filter(Commodity %in% c("Tomato Big(Nepali)", "Potato Red",
                          "Onion Dry (Indian)", "Carrot(Local)",
                          "Cabbage(Local)")) %>%
  select(Commodity, Date, Average)

# Calculate the adjusted average price per day based on CPI
veggies_adjusted <- veggies %>%
  inner_join(cpi, by = c("Date" = "Date")) %>%
  mutate(Average_constant = Average / Index)

veggies_monthly <- veggies_adjusted %>%
  group_by(Commodity, Month = yearmonth(Date)) %>%
  summarise(Average_monthly = mean(Average_constant, na.rm = TRUE))

ggplot(veggies_monthly, aes(x = Month, y = Average_monthly, color = Commodity)) +
  geom_line() +
  labs(x = "Date", y = "Average Price (Adjusted for CPI)", color = "Commodity")

# Investigation of the Cabbage
Cabbage <- veggies %>% 
  filter(Commodity == "Cabbage(Local)")

# Calculate the adjusted average price per day based on CPI
Cabbage_adjusted <- Cabbage %>%
  inner_join(cpi, by = c("Date" = "Date")) %>%
  mutate(Average_constant = Average / Index)

# Calculate the monthly average price
Cabbage_monthly <- Cabbage_adjusted %>%
  group_by(Month = yearmonth(Date)) %>%
  summarise(Average_monthly = mean(Average_constant, na.rm = TRUE))

# Plot the Cabbage
ggplot(Cabbage_monthly, aes(x = Month, y = Average_monthly)) +
  geom_line() +
  labs(x = "Date", y = "Average Price (Adjusted for CPI)") +
  theme_bw()

# Make a tsibble and fill missing values with mean
Cabbage_tsibble <- Cabbage_monthly %>%
  as_tsibble(index = Month)

Cabbage2<- Cabbage_tsibble %>%
  fill_gaps(Average_monthly = mean(Average_monthly, na.rm = TRUE))

Cabbage2 %>% autoplot(Average_monthly)
# Looking at the plot we can see that there is a positive trend going on and cyclic behavior.
# looking at it, it looks like the cabbage is more expensive in the beginning of every 2 years.
# We can also see that this trend is additive rather than multiplicative, suggesting that we might not need a mathematical transformation)
# Box-cox is used to normalize skewed data and get heterogeneity, we can check how heterogenic the data is to decide whether or not a box-cox is a good choice.

Cabbage2 %>% ACF(Average_monthly) %>% autoplot()

# Looking at the ACF we see a positive autocorrelation as well as negative correlation. 
# The positive correlation is for the two first lags and the negative is mainly for lag 5,6 and 7.
# positive autocorrelation at the early lags suggests persistence in the price behavior, 
# whilst the negative autocorrelation at lag 5,6, and 7 lags could suggest some sort of periodic behavior or seasonality in the data.

# Now we can check if a box-cox would do any difference, we can first see if there is any heteroskedasticity
# the breusch-pagan test suggests that there is no need for box_cox (0.1862 > 0.05) we do not reject hypothesis that we have homoskedasticity

lm_model <- lm(Average_monthly~ Month, data = Cabbage2)
bptest(lm_model)

# Box-cox transformation - let´s check with and without box-cox anyways (for fun and science)
# But as we see, maybe it accounts for some variance, but not really..there is no siginifcant differnce

Cabbage2%>%
  features(Average_monthly, features = guerrero) %>%
  pull(lambda_guerrero)-> lambda

Cabbage2 %>% autoplot(Average_monthly)

CabbageMonthly%>%
  autoplot(box_cox(Average, lambda))

# Decomposition let's try different decomposition models and assess which one is the best.
# Looking at the decomposition models we can see in the plots that the x11 (X_13ARIMA_SEATS) 
# is the best decomposition model

Cabbage2 %>%
  model(STL1 = STL(Average_monthly),
        STL2 = STL(Average_monthly ~ trend(window = 7) + season(window = 5), robust = TRUE),
        fixed = STL(Average_monthly ~ season(window = "periodic"), robust = TRUE),
        x11 = X_13ARIMA_SEATS(Average_monthly),
        seats = X_13ARIMA_SEATS(Average_monthly ~ seats()))%>%
  components() -> dcmpCabbage
  
dcmpCabbage %>% as_tsibble() %>% autoplot(trend)
dcmpCabbage %>% as_tsibble() %>% autoplot(season_adjust)
autoplot(dcmpCabbage)

# Create training data/set and checking which method model is best for the cabbage time series
nrow(Cabbage2)

# we can train on 40 rows
Cabbage2 %>% slice(1:(n()-54))-> trainCabbage

Cabbagefit <- trainCabbage%>%
  model(
    Naive = NAIVE(Average_monthly),
    Seasonal_Naive = SNAIVE(Average_monthly),
    Drift = RW(Average_monthly ~ drift())
  ) 

CabbageFC<- Cabbagefit %>%
  forecast(h = 5) %>%
  autoplot(train_cabbage, level = NULL) +
  labs(title = "Forecasts for monthly cabbage price") +
  guides(color = guide_legend(title = "Forecast"))

CabbageFC
# The best method model for is the random walk with a drift according to the accuracy (RMSE)
# Residual Diagnostics
# When looking at the residuals they resemble white noise, which indicates a good method model.
# We do have a significant lag (the first one), but as a rule of thumb = if 95% of the lags are not significant we can consider it white noise.
# And the lag is also relatively small/have small significance.

# The innovation residuals are uncorrelated. If there are correlations between innovation residuals, 
# then there is information left in the residuals which should be used in computing forecasts.
# The innovation residuals have zero mean. If they have a mean other than zero, then the forecasts are biased.
# Basically, we want our residuals to be white noise, normally distributed and no variance. This is a sign that the model is good
# That RW is the best model here might not come as a surprise, as that model usually do good when we have a positive trend in our data.
# the naive model here would also have been good but snaive is not good because their residuals are not white noise.

Cabbagefit %>% accuracy() %>% arrange(RMSE)

CabbageDrift <- trainCabbage %>%
  model(RW(Average_monthly ~ drift()))

CabbageDrift %>% gg_tsresiduals() 

# Exponential smoothing
# We are now going to choose a ETS model for the Cabbage.
# Additive is more when the it's constant over time, in our case we see that the variability increases proportionally with the level of the series.
# so we should have a M error.Additive Trend is good to use when the trend changes at a constant rate over time. 
# but when the trend changes at a rate that is proportional to the level of the series we can use M. 

# We can also decide to not include the trend, but that would indicate that this is a completely stationary series, which we don't agree with.
# Additive Seasonality: Use when the seasonal pattern of the data remains relatively constant in absolute terms over time.
# Multiplicative Seasonality is used here because the seasonal pattern of the data changes in proportion to the level of the series.
# the amplitude of the seasonal fluctuations increases/decreases as the level of the series increases/decreases).

Cabbage2 %>% autoplot(Average_monthly)

CabbageETS <- Cabbage2 %>% 
  model(ses = ETS(Average_monthly ~ error("M") + trend("M") + season("M")))

report(CabbageETS)







