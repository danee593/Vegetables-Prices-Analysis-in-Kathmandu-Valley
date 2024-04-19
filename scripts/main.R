# Required Libraries
install.packages("lmtest")
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

cpi <- read_excel("data_sources/Consumer Price Index, All items.xlsx", 
                 skip = 7)

# tidy up cpi
base_index <- cpi$Index[1]

cpi <- cpi %>% 
  mutate(date = yearmonth(cpi$Units),
         Index = Index/base_index) %>% 
  select(c(date, Index))

# Get only 5 keys and 1 variable.
veggies <- veggies %>% 
  filter(Unit == "Kg") %>% 
  filter(Commodity %in% c("Tomato Big(Nepali)", "Potato Red",
                          "Onion Dry (Indian)", "Carrot(Local)",
                          "Cabbage(Local)")) %>%
  select(Commodity, Date, Average)


Cabbage <- veggies %>% 
  filter(Commodity == "Cabbage(Local)")


cabbage_constant_prices <- Cabbage |>
  mutate(ym = yearmonth(Date)) %>% 
  inner_join(cpi, by = join_by(ym == date)) %>% 
  mutate(Average_constant = Average / Index) %>% 
  select(Date, Commodity, Average_constant)

Cabbage_tsibble <- as_tsibble(cabbage_constant_prices, key = Commodity, index = Date)

CabbageMonthly <- Cabbage_tsibble %>%
  index_by(Month = yearmonth(Date)) %>%
  summarise(Average = mean(Average_constant))

# Looks good (aka it turned into monthly)
print(CabbageMonthly)

CabbageMonthly %>% autoplot(Average)
# Looking at the plot we can see that there is a slightly positive trend and seasonality going on.
# looking at it, it looks like the cabbage is more expensive in the beginning of the year (possibly due to weather/sparsity.)
# We can also see that this trend is additive rather than multiplicative, suggesting that we might not need a mathematical transformation)
# Box-cox is used to normalize skewed data and get heterogeneity, we can check how heterogenic the data is to decide whether or not a box-cox is a good choice.

CabbageMonthly %>% ACF(Average) %>% autoplot()

# Looking at the ACF we see a positive autocorrelation as well as negative correlation. 
# The positive correlation is for the two first lags and the negative is mainly for lag 5,6 and 7.
# positive autocorrelation at the early lags suggests persistence in the price behavior, 
# whilst the negative autocorrelation at  lag 5,6, and 7 lags could suggest some sort of periodic behavior or seasonality in the data.

# Now we can check if a box-cox would do any difference, we can first see if there is any heteroskedasticity
# the breusch-pagan test suggests that there is no need for boxcox (0.723) we do not reject hypothesis that we have homoskedasticity
lm_model <- lm(Average ~ Month, data = CabbageMonthly)
bptest(lm_model)

# Box-cox transformation - let´s check with and without box-cox anyways (for fun and science)
# But as we see, maybe it accounts for some variance, but not really..there is no siginifcant differnce
CabbageMonthly%>%
  features(Average, features = guerrero) %>%
  pull(lambda_guerrero)-> lambda

CabbageMonthly%>% autoplot(Average)

CabbageMonthly%>%
  autoplot(box_cox(Average, lambda))

# Decomposition let's try different decomposition models and assess which one is the best.
# Looking at the decomposition models we can see in the plots that the x11 (X_13ARIMA_SEATS) 
# is the best decomposition model

CabbageMonthly %>%
  model(STL1 = STL(Average),
        STL2 = STL(Average ~ trend(window = 7) + season(window = 5), robust = TRUE),
        fixed = STL(Average ~ season(window = "periodic"), robust = TRUE),
        x11 = X_13ARIMA_SEATS(Average),
        seats = X_13ARIMA_SEATS(Average ~ seats()))%>%
  components() -> dcmpCabbage
  
dcmpCabbage %>% as_tsibble() %>% autoplot(trend)
dcmpCabbage %>% as_tsibble() %>% autoplot(season_adjust)
autoplot(dcmpCabbage)

# Create training data/set and checking which method model is best for the cabbage time series
nrow(CabbageMonthly)

# we can train on 25 rows
CabbageMonthly %>% slice(1:(n()-40))-> train_cabbage

Cabbagefit <- train_cabbage%>%
  model(
    mean = MEAN(Average),
    Naive = NAIVE(Average),
    Seasonal_Naive = SNAIVE(Average),
    Drift = RW(Average ~ drift())
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
# The innovation residuals are uncorrelated. If there are correlations between innovation residuals, 
# then there is information left in the residuals which should be used in computing forecasts.
# The innovation residuals have zero mean. If they have a mean other than zero, then the forecasts are biased.
# Basically, we want our residuals to be white noise, normally distributed and no variance. This is a sign that the model is good
# That RW is the best model here might not come as a surprise, as that model usually do good when we have a positive trend in our data.
# the naive model here would also have been good but mean and snaive is not good because their residuals are not white noise.

accuracy(Cabbagefit)
Cabbagefit %>% accuracy() %>% arrange(RMSE)

CabbageDrift <- train_cabbage %>%
  model(RW(Average ~ drift()))

CabbageDrift %>%
  gg_tsresiduals() 

CabbageNaive <- train_cabbage %>%
  model(NAIVE(Average))

CabbageNaive %>%
  gg_tsresiduals() 

# Exponential smoothing






