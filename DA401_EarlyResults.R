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


#import data 
Data <- read_csv("DA401_Data.csv")
#making sure data is all numeric (this is already done in the code file for data collection)
for (i in colnames(Data)){Data[[i]]<- as.numeric(Data[[i]])}


#graph to see the change in exchange rate with respect to time. Create another variable called ExRate_Adjusted to account for inflation. We account for inflation by dividing the Exchange Rate (measured in VND) divide by the Consumer Price Index in Vietnam
Data$ExRate_Adjusted <- Data$ExRate / Data$VN_CPI * 100

# Create the two-layer line plot
ggplot(Data, aes(x = Year + (Month - 1) / 12)) +
  geom_line(aes(y = ExRate, color = "Normal ExRate")) +
  geom_line(aes(y = ExRate_Adjusted, color = "ExRate Adjusted by VN_CPI")) +
  labs(x = "Year", y = "Exchange Rate", 
       title = "Exchange Rate and Exchange Rate Adjusted by VN CPI") +
  scale_color_manual(values = c("Normal ExRate" = "blue", "ExRate Adjusted by VN_CPI" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())


#Create a time series data set for future analysis using tsibble
data_tsibble <- Data %>%
  mutate(Date = yearmonth(paste(Year, Month))) %>%  # Create a Date column
  as_tsibble(index = Date)  # Convert to tsibble, using Date as the index

# We will attempt to use box-cos transformation to hopefully being able to use additive decomposition later
# We find the optimal lambda using guerrero method
lambda <- data_tsibble %>%
  features(ExRate, features = guerrero) %>%
  pull(lambda_guerrero)

# Create a new transformed exrate using box-cox with the above lambda
data_tsibble <- data_tsibble |>
  mutate(ExRate_Transformed = box_cox(ExRate, lambda))

#Create the original exchange rate plot
plot1 <- ggplot(data_tsibble, aes(x = Year + (Month - 1) / 12, y = ExRate)) +
  geom_line(color = "blue") +
  labs(y = "Exchange Rate", x="Year", title = "Normal Exchange Rate") +
  theme_minimal()

# Create the transformed exchange rate plot
plot2 <- ggplot(data_tsibble, aes(x = Year + (Month - 1) / 12, y = ExRate_Transformed)) +
  geom_line(color = "red") +
  labs(y = "Transformed Exchange Rate", x = "Year",
       title = paste("Transformed Exchange Rate with λ =", round(lambda, 2))) +
  theme_minimal()

# Arrange plots side by side
grid.arrange(plot1, plot2, ncol=2)
#We see that the box-cox transformation only scaled down the Exchange Rate, it did not affect the distribution


#ARIMA Model using x_11 to filter out seasonal noise, making the model more robust. If X11 does not do a good job in capturing the seasonal trend, then we eed a more complex method, which we will move onto x13_seats
x11_dcmp <- data_tsibble %>% 
  model(x11= X_13ARIMA_SEATS(ExRate ~ x11())) %>% 
  components()

x11_dcmp %>% autoplot()
#No clear stable, periodic seasonality. Looks like there is outlier at the beginning (in irregular)
x11_dcmp %>% gg_subseries(seasonal)
#No seasonal pattern between the months in the last 20 years. 

#We try adding moving averages to see if it work so we could incoporate it back to x11 model 
xrate_ma <- data_tsibble |>
  mutate(
    `12-MA` = slider::slide_dbl(ExRate, mean,
                                .before = 5, .after = 6, .complete = TRUE),
    `4x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                  .before = 5, .after = 6, .complete = TRUE)
  )

xrate_ma %>% autoplot(ExRate) + 
  geom_line(aes(y=`12-MA`), color = 'red')  +
  geom_line(aes(y=`4x12-MA`), color = 'purple') + labs()

#No clear difference between 12 and 4x12, even though both are good at ignoring the minor fluctuations. 


#similar behavior for x13_seats. We move to SLT since it deals better with outliers and periodic seasonality. 
slt_dcmp <- data_tsibble %>% model(stl = STL(ExRate))
components(slt_dcmp) %>% autoplot()

#Looks like there are patterns between around every 4 years? Will check later***
#We create another SLT decomposition but specific a periodic seasonality and add robustness. If there is a seasonality, that means that we have clear outliers in our data 
slt_dcmp2 <- data_tsibble %>% 
  model(STL(ExRate ~season(window = "periodic"), 
                                    robust = TRUE)) %>%
  components()

slt_dcmp2 %>% autoplot()

#Seems like we really do have outliers. Let's check! 
# Find outliers 
# Calculate the month-over-month change in ExRate (initial rate set to zero)
data_tsibble <- data_tsibble %>%
  arrange(Date) %>%
  mutate(ExRate_Change = ExRate - lag(ExRate, default = first(ExRate)),
         ExRate_Change_From_Zero = cumsum(ExRate_Change))

# Find outliers based on the mean and sd of rate of change
mean_change <- mean(data_tsibble$ExRate_Change, na.rm = TRUE)
sd_change <- sd(data_tsibble$ExRate_Change, na.rm = TRUE)

# Identify observations that are outliers 
data_tsibble <- data_tsibble %>%
  mutate(outlier = abs(ExRate_Change - mean_change) > 2 * sd_change)

# Print
outlier_months <- data_tsibble %>%
  filter(outlier) %>%
  select(Date, ExRate, ExRate_Change)
print(outlier_months)

# Check outliers on graph (red dots)
ggplot(data_tsibble, aes(x = Date, y = ExRate_Change_From_Zero)) +
  geom_line(color = "blue") +
  geom_point(data = filter(data_tsibble, outlier), aes(y = ExRate_Change_From_Zero), color = "red", size = 2) +
  labs(title = "Cumulative Monthly Changes in Exchange Rate with Outliers Highlighted",
       y = "Cumulative Change in Exchange Rate",
       x = "Date") +
  theme_minimal()







