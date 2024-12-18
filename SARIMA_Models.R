#load all libraries 
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tsfeatures)
library(fabletools)
library(fable)
library(tsibble)
library(feasts)
library(seasonal)
library(tseries)
library(purrr)
library(forecast)
library(here)

#This is a part 2 of the TimeSeriesModels.R, where I extend the ARIMA model into SARIMA by incoporating exogeneous variables, which is in the DA401DATA dataset. Explanations for beginning steps can be found in the TimeSeriesModels.R file. 
#Data for non-econ factors start from 2008. There is another Annual yearly which has more non-econ factors, however the number of observations are too small (24 for 24 years), thus we will not use it for now, but the dataset is available for future use. 
Data <- read_csv(here("DA401_Monthly_Data.csv"))

#tsibble
data_tsibble <- Data %>%
  mutate(Date = yearmonth(paste(Year, Month))) %>% 
  as_tsibble(index = Date) 
data_tsibble <- data_tsibble %>% select(-c('Year', 'Month')) %>% drop_na()

#BoxCox Transformation 
lambda <- data_tsibble %>%
  features(ExRate, features = guerrero) %>%
  pull(lambda_guerrero)

print(lambda)

# Create a new transformed exrate using box-cox with the above lambda
data_tsibble <- data_tsibble %>%
  mutate(ExRate_Transformed = box_cox(ExRate, lambda))

data_tsibble$exrate_rm <- forecast::tsclean(data_tsibble$ExRate_Transformed, replace.missing = TRUE, lambda = lambda)

#Line plot to show the values of exrate over time to see what we need to predict. 
autoplot(data_tsibble, exrate_rm) +
  ggtitle("Exchange Rate Over Time") +
  xlab("Time") +
  ylab("Exchange Rate")

#Split train-test 7:3
split_index <- round(nrow(data_tsibble) * 0.7)
train <- data_tsibble %>% slice(1:split_index)
test <- data_tsibble %>% slice((split_index + 1):nrow(data_tsibble))


######## ECON FACTORS 

econ_factors_train <-  cbind(train$Log_PI, train$Difference_In_IR, train$VNINDEX_Score, train$VIX_Score)
econ_factors_test <- cbind(test$Log_PI, test$Difference_In_IR, test$VNINDEX_Score, test$VIX_Score)

test_start <- end(train$exrate_rm)[1] + 1
test_ts <- ts(test$exrate_rm, start = test_start) #how to set time from 2000-2024...

sarima_model1 <- auto.arima(train$exrate_rm, xreg = econ_factors_train)
sarima_model1 

sarima_model1 %>%
  forecast(h=1, xreg = econ_factors_test) %>%
  autoplot() + 
  autolayer(test_ts, series = "Real Exchange Rate") 

prediction_m1 <- sarima_model1 %>%
  forecast(h=56, xreg = econ_factors_test) 

## PLOT 
test <- test %>%
  mutate(
    Prediction = prediction_m1$mean,
    Lower_80 = prediction_m1$lower[, 1],
    Upper_80 = prediction_m1$upper[, 1],
    Lower_95 = prediction_m1$lower[, 2],
    Upper_95 = prediction_m1$upper[, 2]
  )

# Combine train, test, and prediction data
train <- train %>% mutate(Type = "Train", Prediction = NA, Lower_80 = NA, Upper_80 = NA, Lower_95 = NA, Upper_95 = NA)
test <- test %>% mutate(Type = "Test")
combined_data <- bind_rows(train, test)

#Plot using ggplot
ggplot(data = combined_data, aes(x = Date)) +
  geom_line(aes(y = exrate_rm, color = Type)) +
  geom_line(aes(y = Prediction, color = "Prediction")) +
  geom_ribbon(aes(ymin = Lower_80, ymax = Upper_80), fill = "pink", alpha = 0.2) +
  geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95), fill = "red", alpha = 0.1) +
  labs(
    title = "SARIMA using Economics factors",
    y = "Exchange Rate",
    x = "Date",
    color = "Series"
  ) + theme_minimal()

######### NON-ECON FACTORS

tourist_factor_train <- train$Num_Foreign_Tourists
tourist_factor_test <- test$Num_Foreign_Tourists


sarima_model2 <- auto.arima(train$exrate_rm, xreg =tourist_factor_train)
sarima_model2

prediction_m2 <- sarima_model2 %>%
  forecast(h = 56, xreg = tourist_factor_test) 

test2 <- test %>%
  mutate(
    Prediction = prediction_m2$mean,
    Lower_80 = prediction_m2$lower[, 1],
    Upper_80 = prediction_m2$upper[, 1],
    Lower_95 = prediction_m2$lower[, 2],
    Upper_95 = prediction_m2$upper[, 2]
  )

# Combine train, test, and prediction data
train2 <- train %>% mutate(Type = "Train", Prediction = NA, Lower_80 = NA, Upper_80 = NA, Lower_95 = NA, Upper_95 = NA)
test2 <- test2 %>% mutate(Type = "Test")
combined_data2 <- bind_rows(train2, test2)

#PLOT 
ggplot(data = combined_data2, aes(x = Date)) +
  geom_line(aes(y = exrate_rm, color = Type)) +
  geom_line(aes(y = Prediction, color = "Prediction")) +
  geom_ribbon(aes(ymin = Lower_80, ymax = Upper_80), fill = "pink", alpha = 0.2) +
  geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95), fill = "red", alpha = 0.1) +
  labs(
    title = "SARIMA using Number of Foreign Tourists",
    y = "Exchange Rate",
    x = "Date",
    color = "Series"
  ) + theme_minimal()


ggtsdisplay(residuals(sarima_model2))
checkresiduals(sarima_model2) 
autoplot(sarima_model2)


########## ALL FACTORS 

all_factor_train <- cbind(train$Log_PI, train$Difference_In_IR, train$VNINDEX_Score, train$VIX_Score, train$Num_Foreign_Tourists)
all_factor_test <- cbind(test$Log_PI, test$Difference_In_IR, test$VNINDEX_Score, test$VIX_Score, test$Num_Foreign_Tourists)

sarima_model3 <- auto.arima(train$exrate_rm, xreg =all_factor_train)
sarima_model3

prediction_m3 <- sarima_model3 %>%
  forecast(h = 56, xreg = all_factor_test) 

test3 <- test %>%
  mutate(
    Prediction = prediction_m3$mean,
    Lower_80 = prediction_m3$lower[, 1],
    Upper_80 = prediction_m3$upper[, 1],
    Lower_95 = prediction_m3$lower[, 2],
    Upper_95 = prediction_m3$upper[, 2]
  )

# Combine train, test, and prediction data
train3 <- train %>% mutate(Type = "Train", Prediction = NA, Lower_80 = NA, Upper_80 = NA, Lower_95 = NA, Upper_95 = NA)
test3 <- test3 %>% mutate(Type = "Test")
combined_data3 <- bind_rows(train3, test3)

# Plot using ggplot
ggplot(data = combined_data3, aes(x = Date)) +
  geom_line(aes(y = exrate_rm, color = Type)) +
  geom_line(aes(y = Prediction, color = "Prediction")) +
  geom_ribbon(aes(ymin = Lower_80, ymax = Upper_80), fill = "pink", alpha = 0.2) +
  geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95), fill = "red", alpha = 0.1) +
  labs(
    title = "SARIMA with All variables",
    y = "Exchange Rate",
    x = "Date",
    color = "Series"
  ) + theme_minimal()


