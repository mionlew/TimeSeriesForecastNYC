library(tidyverse)
library(dplyr)
library(ggplot2)
library(TTR)


#Read in csv dataset for NYC spending and revenue
nyc <- read_csv("Downloads/nycrevenue.csv")
options(scipen = 999)
names(nyc) <- tolower(names(nyc))

#Filter dataset to only include revenue amount and fiscal year.
nycrev <- nyc %>%
  filter(revenue_or_spending == 'Revenue') %>%
  select(amount, fiscal_year)
#Reassign dataframe that groups the years and sums the revenue amount
nycrev <- aggregate(nycrev["amount"], by = nycrev["fiscal_year"], sum)

#Time series plot of the average revenue
ggplot(data = nycrev, mapping = aes(x = fiscal_year, y = amount)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Annual Revenue", x = 'Year', y = 'Revenu') +
  scale_y_continuous(labels = scales::label_comma())
 
#Add a column of consecutive numbers corresponding with each year
nycrev$Time <- 1:nrow(nycrev)

#Simple linear regression analysis to create a forecast regression equation
nycreg <- lm(amount ~ fiscal_year, data = nycrev)
summary(nycrev)




#Simple moving average method. n = 3 for 3 years, predicting 2024- separate vector
nyc_actuals <- nycrev$amount

nyc24 <- SMA(nyc_actuals, n = 3)

nyc_pred <- c(NA, nyc24[-length(nyc24)]) #2023 forecast = 19039687861

#Create functions to calculate accuracy measures
mae <- function(actual, pred){
  mae <- mean(abs(actual - pred), na.rm = TRUE)
  return(mae)
}

mse <- function(actual, pred){
  mse <- mean(abs(actual - pred)^2, na.rm = TRUE)
  return(mse)
}

rmse <- function(actual, pred){
  rmse <- sqrt(mean((actual - pred)^2, na.rm = TRUE))
  return(rmse)
}

mape <- function(actual, pred){
  mape <- mean(abs((actual - pred)/actual), na.rm = TRUE) * 100
  return(mape)
}



#Calculate accuracy measures for the simple moving average method.
mae(nyc_actuals, nyc_pred)
mse(nyc_actuals, nyc_pred)
rmse(nyc_actuals, nyc_pred)
mape(nyc_actuals, nyc_pred)






#Method 2 Exponential Moving Average with .2 default alpha
exp <- EMA(nyc_actuals, n = 1, ratio = .2)
exp_pred <- c(NA, exp[-length(exp)])

#Accurqacy measures for EMA
mae(nyc_actuals, exp_pred)
mse(nyc_actuals, exp_pred)
rmse(nyc_actuals, exp_pred)
mape(nyc_actuals, exp_pred)