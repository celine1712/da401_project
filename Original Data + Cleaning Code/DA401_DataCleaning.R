#Import datasets 
library(readr)
library(tidyr)
library(dplyr)
library(here)
#exchange rate
here()
ExRate <- read_csv(here("Original Data + Cleaning Code/Exchange rate  - Monthly.csv"))

#Capital Flow 
US_Stock <- read_csv("Original Data + Cleaning Code/VIXCLS.csv")
VN_Stock <- read_csv("Original Data + Cleaning Code/VN Index Historical Data - VN Index Historical Data.csv.csv")


#Price Index 
CPI_VN <- read_csv("Original Data + Cleaning Code/Prices_Production_and_Labor - Monthly.csv")
CPI_US <- read_csv("Original Data + Cleaning Code/Prices_Production_and_Labor_US - Monthly.csv")

#Interest Rate 
VN_Interest_Rates <- read_csv("Original Data + Cleaning Code/Interest_Rates - Monthly.csv")
US_Interest_Rates <- read_csv("Original Data + Cleaning Code/Interest_Rates_US - Monthly.csv")


#Cleaning
#ExRate 

date <- ExRate$`Year + Month`
date <- as.data.frame(date)
date <- date %>% separate(date, into = c('Year', 'Month'), sep = "M")

ExchangeRate <- cbind(date, ExRate$`Domestic Currency per U.S. Dollar, Period Average`)
ExchangeRate <- ExchangeRate %>% rename('ExRate'='ExRate$`Domestic Currency per U.S. Dollar, Period Average`')

rm(ExRate, date)


#CPI 
#dropping rows with no 'M' (Quarter or annual reports)
CPI_US <- CPI_US[grepl("M", CPI_US$`Year+Month`),]

date <- CPI_US$`Year+Month`
date <- as.data.frame(date)
date <- date %>% separate(date, into = c('Year', 'Month'), sep = "M")

US_CPI <- cbind(date, CPI_US$CPI)
rm(CPI_US, date)

#same thing for VN CPI 
CPI_VN <- CPI_VN[grepl("M", CPI_VN$`Year+Month`),]

date <- CPI_VN$`Year+Month`
date <- as.data.frame(date)
date <- date %>% separate(date, into = c('Year', 'Month'), sep = "M")

VN_CPI <- cbind(date, CPI_VN$`2010=100`)
rm(CPI_VN, date)

#Capital Flow
#US
date <- US_Stock$DATE
date <- as.data.frame(date)
date <- date %>% separate(date, into = c('Year', 'Month', 'Day'), sep = "-")

US_Stock <- as.data.frame(cbind(date$Year, date$Month, US_Stock$VIXCLS))
US_Stock <- US_Stock %>% rename('Year' = V1, 'Month' = V2, 'VIX_Score' = V3)
rm(date)

#VN 
date<- VN_Stock$Date
date <- as.data.frame(date)
date <- date %>% separate(date, into = c('Month', 'Day','Year'), sep = "/")
VN_Stock <- as.data.frame(cbind(date$Year, date$Month, VN_Stock$Price))
VN_Stock <- VN_Stock %>% rename('Year' = V1, 'Month' = V2, 'VNINDEX_Score' = V3)
rm(date)


#Interest Rate 
#US
US_Interest_Rates <- US_Interest_Rates[grepl("M", US_Interest_Rates$`Year+Month`),]

date <- US_Interest_Rates$`Year+Month`
date <- as.data.frame(date)
date <- date %>% separate(date, into = c('Year', 'Month'), sep = "M")

US_Interest_Rates  <- cbind(date, US_Interest_Rates $`Interest Rates`)
rm(date)

#VN 
VN_Interest_Rates <- VN_Interest_Rates[grepl("M", VN_Interest_Rates$`Base Year`),]

date <- VN_Interest_Rates$`Base Year`
date <- as.data.frame(date)
date <- date %>% separate(date, into = c('Year', 'Month'), sep = "M")

VN_Interest_Rates  <- cbind(date, VN_Interest_Rates $`Interest Rate`)
rm(date)

#Foreigner tourists coming into vietnam 
VN_Tourists_Monthly <- read_csv(here("VN_Tourists_Monthly.csv"))

Data <- ExchangeRate%>%
  left_join(US_CPI, by = c('Year','Month')) %>%
  left_join(US_Interest_Rates, by = c('Year','Month')) %>%
  left_join(US_Stock, by = c('Year','Month')) %>%
  left_join(VN_CPI, by = c('Year','Month')) %>%
  left_join(VN_Interest_Rates, by = c('Year','Month')) %>%
  left_join(VN_Stock, by = c('Year','Month'))
   
for (i in colnames(Data)){Data[[i]]<- as.numeric(Data[[i]])}

Data <- Data %>% left_join(VN_Tourists_Monthly, by = c('Year','Month'))


Data <- Data %>% 
  rename( 'US_CPI' = `CPI_US$CPI`,
          'US_Interest_Rate'= `US_Interest_Rates$\`Interest Rates\``,
          'VN_CPI' = `CPI_VN$\`2010=100\``,
          'VN_Interest_Rate'= `VN_Interest_Rates$\`Interest Rate\``)

Data$Log_PI <- log(Data$VN_CPI/Data$US_CPI)
Data$Difference_In_IR <- as.numeric(Data$VN_Interest_Rate) - as.numeric(Data$US_Interest_Rate)

Data <- subset(Data, select = -c(US_CPI, VN_CPI, US_Interest_Rate, VN_Interest_Rate))

write.csv(Data, "DA401_Monthly_Data.csv", row.names = FALSE)




#For annual data 
library(dplyr)
# Calculate annual average exchange rate
Annual_Avg <- Data %>%
  group_by(Year) %>%                  
  summarise(Average_Exchange = mean(ExRate, na.rm = TRUE), 
            Average_VIX_Score = mean(VIX_Score, na.rm=TRUE), 
            Average_VNIXDEX_Score = mean(VNINDEX_Score, na.rm=TRUE),
            Average_VIX_Score = mean(VIX_Score, na.rm=TRUE), 
            Average_Inflation = mean(Log_PI, na.rm = TRUE), 
            Average_Diff_IR = mean(Difference_In_IR, na.rm = TRUE))

VN_CollegeIntStudents <- read_csv(here("Original Data + Cleaning Code/VN_CollegeIntStudents.csv"))
VN_Tourists_Year <- read_csv("Original Data + Cleaning Code/VN_Tourists_Year.csv")

Annual <- Annual_Avg %>%
  left_join(VN_CollegeIntStudents, by = 'Year') %>%
  left_join(VN_Tourists_Year , by = 'Year')

write.csv(Annual, "DA401_Annual_Data.csv", row.names = FALSE)
