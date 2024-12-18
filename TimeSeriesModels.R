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
library(lubridate)
library(ggrepel)

#import data 

Data <- read_csv(here("DA401_Data.csv"))
#making sure data is all numeric (this is already done in the code file for data collection)
for (i in colnames(Data)){Data[[i]]<- as.numeric(Data[[i]])}


#Create a time series data set for future analysis using tsibble
data_tsibble <- Data %>%
  mutate(Date = yearmonth(paste(Year, Month))) %>%  # Create a Date column
  as_tsibble(index = Date)  # Convert to tsibble, using Date as the index

#BoxCox Transformation 
lambda <- data_tsibble %>%
  features(ExRate, features = guerrero) %>%
  pull(lambda_guerrero)

# Create a new transformed exrate using box-cox with the above lambda
data_tsibble <- data_tsibble |>
  mutate(ExRate_Transformed = box_cox(ExRate, lambda))

#Create the original exchange rate plot
ggplot(data_tsibble, aes(x = Date, y = ExRate)) +
  geom_line(color = "blue") +
  labs(y = "Line Plot of Exchange Rate (VND) ", x="Year", title = "Exchange Rate in VND from 2000-2024") +
  theme_minimal()

# Create the transformed exchange rate plot => good to use, the box-cox transformation did not alter the trend existing in the variable distribution 
ggplot(data_tsibble, aes(x = Year + (Month - 1) / 12, y = ExRate_Transformed)) +
  geom_line(color = "red") +
  labs(y = "Transformed Exchange Rate", x = "Year",
       title = paste("Box-Cox Transformed Exchange Rate with λ =", round(lambda, 2), "obtained through Guerrero")) +
  theme_minimal()

# TIME SERIES ANALYSIS
#SLT decomposition + components plots => No seasonality pattern, remainder shows sign of outliers 
slt_dcmp <- data_tsibble %>% model(stl = STL(ExRate_Transformed))

components(slt_dcmp) %>% autoplot()
plot.ts(components(slt_dcmp)$remainder)
qqnorm(components(slt_dcmp)$remainder)
qqline(components(slt_dcmp)$remainder)
shapiro.test(components(slt_dcmp)$remainder) #not normally distributed 

# STABLIZE MEAN using detrending 
trend <- components(slt_dcmp)$trend
data_tsibble$detrend <- data_tsibble$ExRate_Transformed - trend 

plot.ts(data_tsibble$detrend,
        ylab = "Exchange Rate",
        main = "Stablized Exchange Rate using Box-Cox transformation and Detrending",
        cex.main = 0.85
)
# Graph shows there are around 3 to 4 shocks happened

#Identify the shocks using mean and standard deviation of the detrended data. 
# Calculate the mean and sdn of the detrended data
mean_detrend <- mean(data_tsibble$detrend, na.rm = TRUE)
sd_detrend <- sd(data_tsibble$detrend, na.rm = TRUE)

# Threshold = 2 standard deviations from the mean
threshold <- 2 * sd_detrend
data_tsibble$shock <- abs(data_tsibble$detrend - mean_detrend) > threshold

# Filter the data to only show dates where shocks occurred
shocks_data <- data_tsibble[data_tsibble$shock == TRUE, ]

# Display the dates and values of the shocks
print(shocks_data[, c("Year" , "Month", "detrend")])
# Overlay shock points in red
shock_indices <- which(data_tsibble$shock == TRUE)

plot.ts(data_tsibble$detrend,
        ylab = "Stabilized Exchange Rate",
        main = "Stablized Exchange Rate using Box-Cox transformation and Detrending",
        cex.main = 0.85
)


shock_dates <- data_tsibble$Date[shock_indices]
shock_values <- data_tsibble$ExRate[shock_indices]

# Plot the exchange rate and add shocks with labels
ggplot(data_tsibble, aes(x = Date, y = ExRate)) +
  geom_line(color = "steelblue") +
  geom_point(data = data_tsibble[shock_indices, ], aes(x = Date, y = ExRate), color = "hotpink") +
  geom_text_repel(data = data_tsibble[shock_indices, ], 
                  aes(x = Date, y = ExRate, label = format(Date, "%b %Y")), 
                  box.padding = 0.5, max.overlaps = 10, 
                  color = "black") +
  labs(y = "Exchange Rate (VND)", x = "Date", title = "Exchange Rate with Identified Shocks") +
  theme_minimal()


#SLT decomposition for the detrended data 
slt_dcmp_detrend <- data_tsibble %>% model(stl = STL(detrend))

components(slt_dcmp_detrend) %>% autoplot() #no seasonality 
shapiro.test(components(slt_dcmp_detrend)$remainder) #not normally distributed 

# STABLIZE MEAN using differencing
differenced <- diff(data_tsibble$ExRate_Transformed, lag = 12) #monthly data => lag = 12 
differenced <- ts(differenced)

plot.ts(differenced)  #variance is not stablized. 
#Cannot run SLT on differenced data 

#removing outliers using tsclean
data_tsibble$exrate_rm <- forecast::tsclean(data_tsibble$ExRate_Transformed, replace.missing = TRUE, lambda = lambda)


#TEST FOR STATIONARY for every ExRate variable we just computed 
adf.test(data_tsibble$ExRate_Transformed) #only box-cox => not stationary 
adf.test(data_tsibble$exrate_rm) #only box-cox + removed outliers => also not stationary 
adf.test(data_tsibble$detrend) #box-cox + detrend, stationary 

slt_dcmp_rm1 <- data_tsibble %>% model(stl = STL(exrate_rm)) #there might be pattern every 3-4 years? 
slt_dcmp_rm2 <- data_tsibble %>% model(stl = STL(exrate_rm ~season(window = "periodic"))) 

components(slt_dcmp_rm1) %>% autoplot()#there might be pattern every 3-4 years? 
components(slt_dcmp_rm2) %>% autoplot()

#Polished graph of components(slt_dcmp_rm1)
components(slt_dcmp_rm1) %>%
  autoplot() +
  ggtitle("STL Decomposition of Transformed ExRate (Additive)") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 10)
  ) 

#Check seasonality for every subset of 6 consecutive year 
# Get a vector of unique years in your data
years <- unique(data_tsibble$Year)

# Create a list of data frames for every 3 consecutive years
six_year_dfs <- map(seq(1, length(years) - 2, by = 6), function(i) {
  start_year <- years[i]
  end_year <- start_year + 6
  
  # Filter the tsibble for the 3-year range
  data_tsibble %>%
    filter_index(sprintf("%d", start_year) ~ sprintf("%d-12-31", end_year))
})

#Graph of SLT decomp for the years 2000-2006 
data_subset <- six_year_dfs[[1]]

# Run STL decomposition on the 'exrate_rm' column
slt_dcmp_rm <- data_subset %>% model(stl = STL(exrate_rm)) 

components(slt_dcmp_rm)$season_year

# Generate the component plot and print it
autoplot(components(slt_dcmp_rm)) +
  ggtitle(paste("STL Decomposition for Subset of ExRate from 2000-2006")) +
  theme_minimal()


#Graph of SLT decomp for the years 2006-20012 
data_subset <- six_year_dfs[[2]]

# Run STL decomposition on the 'exrate_rm' column
slt_dcmp_rm <- data_subset %>% model(stl = STL(exrate_rm)) 

# Generate the component plot and print it
autoplot(components(slt_dcmp_rm)) +
  ggtitle(paste("STL Decomposition for Subset of ExRate from 2006-2012")) +
  theme_minimal()

#Graph of SLT decomp for the years 2012-2018 
data_subset <- six_year_dfs[[3]]

# Run STL decomposition on the 'exrate_rm' column
slt_dcmp_rm <- data_subset %>% model(stl = STL(exrate_rm)) 

# Generate the component plot and print it
autoplot(components(slt_dcmp_rm)) +
  ggtitle(paste("STL Decomposition for Subset of ExRate from 2006-2012")) +
  theme_minimal()

#Graph of SLT decomp for the years 2018-2024 
data_subset <- six_year_dfs[[4]]

# Run STL decomposition on the 'exrate_rm' column
slt_dcmp_rm <- data_subset %>% model(stl = STL(exrate_rm)) 

# Generate the component plot and print it
autoplot(components(slt_dcmp_rm)) +
  ggtitle(paste("STL Decomposition for Subset of ExRate from 2018-2024")) +
  theme_minimal()

## ARIMA MODEL 
# WITH DETREND DATA 
train <- head(data_tsibble$detrend, round(length(data_tsibble$detrend) * 0.7))
h <- length(data_tsibble$detrend) - length(train)
test <- tail(data_tsibble$detrend, h)


best_arima_model <- auto.arima(train)
print(best_arima_model) #3,0,2 

#test_start <- end(train)[1] + 1
#test_ts <- ts(test, start = test_start, frequency = frequency(train))

ex_rate.train <- Arima(train, order=c(3,0,2), 
                       lambda=lambda)

# Plot the forecast model with test data 
forecast_plot <- ex_rate.train %>%
  forecast(h = 90) %>%
  autoplot() + 
  autolayer(test_ts, series = "Real Exchange Rate") +  # Add test data as a new series
  ggtitle("Forecasts for Exchange Rate from ARIMA(3,0,2)") +
  xlab("Time") +
  ylab("Exchange Rate") +
  guides(
    color = guide_legend(title = "Series"), 
    fill = guide_legend(title = "Prediction Interval")
  ) +
  scale_fill_manual(values = c("Prediction Interval" = "blue")) 

# WITH outlier removed, box-cox transformed, but not detrended data 
#Train-test split 
train <- head(data_tsibble, round(length(data_tsibble$exrate_rm) * 0.7))
h <- length(data_tsibble$exrate_rm) - length(train)
test <- tail(data_tsibble$exrate_rm, h)

test_start <- end(train$Date)[1] + 1
test_ts <- ts(test, start = test_start, frequency = frequency(train)) #how to set time from 2000-2024...

#random walk 
ex_rate.train_rw <- Arima(train$exrate_rm, order=c(0,1,0) , lambda=lambda) #guerrero technique 

ex_rate.train_rw %>%
  forecast(h=90) %>%
  autoplot() + 
  xlab("Time") +
  ylab("Box-Cox Transformed Exchange Rate") +
  autolayer(test_ts, series = "Real Exchange Rate") +  # Add test data as a new series
  ggtitle("Forecasts for Exchange Rate from ARIMA(0,1,0) with drift (Random Walk)") +
  xlab("Time") +
  ylab("Exchange Rate") +
  guides(
    color = guide_legend(title = "Series"), 
    fill = guide_legend(title = "Prediction Interval")
  ) +
  scale_fill_manual(values = c("Prediction Interval" = "blue")) 

#bad model 

#find the best model using auto.arima 

best_arima_model <- auto.arima(data_tsibble$exrate_rm)
print(best_arima_model) #1,1,2 with drift


train <- head(data_tsibble, round(length(data_tsibble$exrate_rm) * 0.7))
h <- length(data_tsibble$Date) - length(train$Date)
test <- tail(data_tsibble, h)


train <- train %>% select(Date, exrate_rm) 
test <- test %>% select(Date, exrate_rm)  

ex_rate.train <- Arima(train$exrate_rm, order=c(1,1,2), 
                       lambda=lambda) 

forecast_result <- ex_rate.train %>% forecast(h=89)  

fit<-auto.arima(data_tsibble$exrate_rm, seasonal=FALSE)
autoplot(fit)

fit #AIC = 1607

#residuals check 
forecast::ggtsdisplay(residuals(fit), lag.max=45, main='(1,1,2) Model Residuals')
forecast::checkresiduals(fit)



####### POLISHED GRAPHS FOR POSTER PRESENTATIONS
#Select group of years
data_subset <- six_year_dfs[[2]]

slt_dcmp_rm <- data_subset %>% model(stl = STL(exrate_rm)) 
seasonal_df <- as_tibble(components(slt_dcmp_rm))

yearly_peaks <- seasonal_df %>%
  mutate(year = lubridate::year(Date),
         month = lubridate::month(Date, label = TRUE)) %>% 
  group_by(year) %>%
  slice_max(order_by = season_year, n = 1) %>%
  ungroup()

# Create the plot using the correct date column
ggplot(seasonal_df, aes(x = Date, y = season_year)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(data = yearly_peaks, 
             aes(x = Date, y = season_year),
             color = "hotpink", size = 3) +
  geom_text(data = yearly_peaks,
            aes(x = Date, label = month),
            vjust = -1.5,
            color = "black") +
  labs(title = "Seasonal Component of Exchange Rate (2006-2012)",
       x = "Date",
       y = "Seasonal Effect") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

##########


train <- head(data_tsibble, round(length(data_tsibble$exrate_rm) * 0.7))
h <- length(data_tsibble$Date) - length(train$Date)
test <- tail(data_tsibble, h)


train <- train %>% select(Date, exrate_rm) 
test <- test %>% select(Date, exrate_rm)  

ex_rate.train <- Arima(train$exrate_rm, order=c(1,1,2), 
                       lambda=lambda) 

forecast_result <- ex_rate.train %>% forecast(h=89)  



forecast_df <- data.frame(
  Date = test$Date,
  Mean = forecast_result$mean,
  Lower80 = forecast_result$lower[, 2],  # 80% lower bound
  Upper80 = forecast_result$upper[, 2],  # 80% upper bound
  Lower95 = forecast_result$lower[, 1],  # 95% lower bound
  Upper95 = forecast_result$upper[, 1]   # 95% upper bound
)

# Step 6: Plot the actual data, forecast mean, and prediction intervals
ggplot() +
  # Plot actual test data
  geom_line(data = train, aes(x = Date, y = exrate_rm), color = 'steelblue', size = 1) +
  # Testing data in another color
  geom_line(data = test, aes(x = Date, y = exrate_rm), color = 'green', size = 1) +
  geom_line(data = forecast_df, aes(x = Date, y = Mean), color = 'red', size = 1) +
  # Plot 80% prediction interval
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower80, ymax = Upper80), fill = 'pink', alpha = 0.2) +
  
  # Plot 95% prediction interval
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower95, ymax = Upper95), fill = 'red', alpha = 0.2) +
  
  # Customize the plot
  labs(title = "ARIMA(1,1,2) Forecast on Exchange Rate",
       x = "Date", y = "Exrate RM") +
  scale_color_manual(values = c('Train' = 'steelblue', 'Test' = 'green', 'Forecast' = 'red')) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






