# Load libraries
library (tidyverse)
library(dplyr)
library (zoo)
library (ggplot2)
library (forecast)
library (tseries)
library(scales)
library(urca)
library(stats)
library(lubridate)
library(TTR)
library(tsibble)
library(fable)
library(feasts)
library(lmtest)

# Data import and cleaning ####
# Import carbon emission csv
carbon_df <- read.csv("carbon.csv")

# Keep only the first row
carbon_df <- carbon_df[1, ]

# Rename year columns to keep only the years
colnames(carbon_df) <- gsub("^X|\\.\\.YR[0-9]+\\.", "", colnames(carbon_df))
print(carbon_df)

# Remove specific columns using select()
carbon_df <- carbon_df %>%
  select(-Country.Name, -Country.Code, -Series.Code)

# Convert year columns to numeric
carbon_df[, -1] <- lapply(carbon_df[, -1], as.numeric)

# Reshape from wide to long format
carbon_df <- carbon_df %>%
  pivot_longer(cols = -Series.Name, names_to = "Year", values_to = "Carbon") %>%
  mutate(Year = as.numeric(Year))  # Convert Year column to numeric

# Remove the Series.Name column and keep only Year and Income
carbon_df <- carbon_df %>%
  select(Year, Carbon)

# Check for outliers
carbon_df_iqr <- quantile(carbon_df$Carbon, probs = c(0.25, 0.75), na.rm = TRUE) + c(-1.5, 1.5) * IQR(carbon_df$Carbon, na.rm=TRUE)
carbon_df$Carbon[(carbon_df$Carbon< carbon_df_iqr[1]) | (carbon_df$Carbon > carbon_df_iqr[2])]

# No outliers and missing values after checking
carbon_df <- as.data.frame(carbon_df)  # Converts to a base R data frame for analysis


# Statistics####
# Basic statisitics
summary(carbon_df) # Standard Statistics
var(carbon_df$Carbon) # Variance
sd(carbon_df$Carbon) # Standard Deviation 
max(carbon_df$Carbon) # Maximum
min(carbon_df$Carbon) # Minimum
range(carbon_df$Carbon) # Range

# Time series plot
plot(carbon_df, type = "o", col = "blue", 
     xlab = "Year", ylab = "MtCO2e", 
     main = "Carbon Dioxide Emission from waste (1981-2023)",
     xaxt = "n")  # Disable x axis so we can customize it)

# Customize x-axis to display all the years and slant the labels
axis(1, at = seq(1981, 2023, by = 1), labels = seq(1981, 2023, by = 1), las = 2)  # Slanted labels

# Convert to time series object (incl 2023)
carbon_ts <- ts(carbon_df$Carbon, start=1981,end=2023, frequency = 1)

# Boxplot for normality test
boxplot(carbon_ts, main="Boxplot of CO2 Emission from waste", ylab= "MtCO2e")

# Plot the histogram
hist_model <- hist(carbon_ts, breaks = 20, probability = TRUE, plot = FALSE)

# Set ylim so that peak of curve does not cut off
hist(carbon_ts, probability = TRUE, col = "lightblue", main = "Histogram with Normal Curve", xlab = "MtCO2e",ylim = c(0, max(hist_model$density) * 1.2))

# Add the normal distribution curve
curve(dnorm(x, mean = mean(carbon_ts), sd = sd(carbon_ts)), col = "red", lwd = 2, add = TRUE)

# ACF 
acf(carbon_ts, main = "Autocorrelation of CO2 Emission Time Series")

# ACF will also show that our time series has no seasonality


# STL decomposition method
carbon_df = carbon_df %>% as_tsibble(index = Year) #convert df to tsibble
df_decom = carbon_df %>% model(stl = STL(Carbon ~ trend())) %>% components()
autoplot(df_decom) + scale_y_continuous(labels = label_comma()) + theme_minimal() +
  labs(title = "STL Decomposition of Carbon Emission 1981-2023", subtitle = NULL, x = "Year", y = "MtCO2e")

# No seasonality, explore trend and remainder
# Examine trend component
trend_df <- data.frame(
  year = 1981:2023,  # or the range of years in your time series
  trend = df_decom$trend
)

# Fit a linear regression model
trend_model <- lm(trend ~ year, data = trend_df)

# Plot the trend and the fitted linear regression line
plot(trend_df$year, trend_df$trend, main = "Trend Component with Linear Regression", xlab = "Year", ylab = "MtCO2e")
abline(trend_model, col = "red")  # Adds the regression line to the plot

# View the model summary
summary(trend_model)

# Fit a linear regression model to the data
lm_model_carbon <- lm(Carbon ~ Year, data = carbon_df)
summary(lm_model_carbon)

# Plot the trend line
ggplot(carbon_df, aes(x = Year, y = Carbon)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Trend Analysis with Linear Regression")

# Explore remainders from STL
summary(df_decom)

# ACF and PACF of residuals
acf(df_decom$remainder, main="ACF plot of remainders") #from the plot can see that remainder behaves like white noise random none crossed threshold
pacf(df_decom$remainder, main="Partial ACF plot of remainders")


carbon_df <- as.data.frame(carbon_df)  # Converts to a base R data frame

# Create time series for carbon data 1981 to 2022  (exc 2023)
carbon_ts_2022 <- ts(carbon_df$Carbon, start=1981, end=2022)

# Moving average####
carbon_ts_2022_ma3 <- carbon_ts_2022 #time series for ma order3

# SMA window size 3
window_size3 <- 3

# Calculate the 3year Simple Moving Average (SMA3) for population
sma3 <- zoo::rollapply(carbon_ts_2022_ma3, width = window_size3, FUN = mean, align = 'right', fill = NA)

# Create a vector to store the forecasted values
forecasted_values_ma3 <- numeric(2)  # 2 years forecast (2023-2024)

# Start the rolling forecast for 2023 to 2024
for (i in 1:2) {
  # Get the rolling moving average for the current window
  sma <- zoo::rollapply(carbon_ts_2022_ma3, width = window_size3, FUN = mean, align = 'right', fill = NA)
  
  # Forecast for next year is the last value of the moving average
  forecasted_values_ma3[i] <- tail(sma, n = 1)
  
  # Extend the time series to include the forecasted year
  carbon_ts_2022_ma3 <- ts(c(carbon_ts_2022_ma3, forecasted_values_ma3[i]), start = c(1981), frequency = 1)
}

# Combine the forecasted values with the original data
forecast_years_ma3 <- 2023:2024
forecast_data_ma3 <- data.frame(Year = forecast_years_ma3, Carbon = forecasted_values_ma3)
forecast_data_ma3
sma3


# Combine original data with forecasted data
carbon_forecast3_df <- rbind(carbon_df, forecast_data_ma3)

# SMA for order 5
carbon_ts_2022_ma5 <- carbon_ts_2022 #time series for ma order5

# SMA window size 3
window_size5 <- 5

# Calculate the 5year Simple Moving Average (SMA3) for population
sma5 <- zoo::rollapply(carbon_ts_2022_ma5, width = window_size5, FUN = mean, align = 'right', fill = NA)

# Create a vector to store the forecasted values
forecasted_values_ma5 <- numeric(2)  # 2 years forecast (2023-2024)

# Start the rolling forecast for 2023 to 2024
for (i in 1:2) {
  # Get the rolling moving average for the current window
  sma <- zoo::rollapply(carbon_ts_2022_ma5, width = window_size5, FUN = mean, align = 'right', fill = NA)
  
  # Forecast for next year is the last value of the moving average
  forecasted_values_ma5[i] <- tail(sma, n = 1)
  
  # Extend the time series to include the forecasted year
  carbon_ts_2022_ma5 <- ts(c(carbon_ts_2022_ma5, forecasted_values_ma5[i]), start = c(1981), frequency = 1)
}

# Combine the forecasted values with the original data
forecast_years_ma5 <- 2023:2024
forecast_data_ma5 <- data.frame(Year = forecast_years_ma5, Carbon = forecasted_values_ma5)
forecast_data_ma5


# Plot the original data 
plot(carbon_df$Year, carbon_df$Carbon, type = 'o', col = 'blue', 
     main = 'Carbon Dioxide Emission Forecast with SMA', xlab = 'Year', ylab = 'MtCO2e',
     xlim = c(1981, 2024), xaxt = 'n')  # Adjust xlim to show 1981 to 2028 range
axis(1, at = 1981:2024, labels = 1981:2024, las = 2)  # Set custom axis labels

# Add the 3-year moving average (SMA3) to the plot
lines(sma3, col = 'green', type = 'o')

# Add the 5-year moving average (SMA5) to the plot
lines(sma5, col = 'black', type = 'o')

# Add legend for original, sma3 and sma5
legend("topleft", legend = c("Original", "SMA-3", "SMA-5"), col = c("blue", "green","black"), lty = 1)

# Add the forecasted values for sma3
lines(forecast_years_ma3, forecasted_values_ma3, col = 'red', type = 'o')

# Add the forecasted values for sma5
lines(forecast_years_ma5, forecasted_values_ma5, col = 'orange', type = 'o')

# Add legend for all
legend("topleft", legend = c("Original", "SMA-3", "Forecast SMA3", "SMA-5", "Forecast SMA5"), col = c("blue", "green", "red", "black","orange"), lty = 1)

# Extract actual 2023 emission value
actual_2023 <- carbon_df[carbon_df$Year == 2023,"Carbon" ]
actual_2023 <- as.numeric(actual_2023) #Convert to numeric for calculation


# Extract 2023 from forecast
forecast_2023_sma3 <- forecasted_values_ma3[1]
forecast_2023_sma3 <- as.numeric(forecast_2023_sma3) #Convert to numeric for calculation

# Calculate MSD for SMA3
msd_sma3 <- mean((forecast_2023_sma3 - actual_2023)^2)
msd_sma3

# Calculate MAPE for SMA3
mape_sma3 <- mean(abs((forecast_2023_sma3 - actual_2023) / actual_2023)) * 100
mape_sma3

# Calculate MSD for SMA5
# Extract 2023 from forecast
forecast_2023_sma5 <- forecasted_values_ma5[1]
forecast_2023_sma5 <- as.numeric(forecast_2023_sma5)

# Calculate MSD for SMA5
msd_sma5 <- mean((forecast_2023_sma5 - actual_2023)^2)
msd_sma5

# Calculate MAPE for SMA5
mape_sma5 <- mean(abs((forecast_2023_sma5 - actual_2023) / actual_2023)) * 100
mape_sma5

# Simple Exponential Smoothing####
# Create time series for exponential smoothing
carbon_ts_2022_ses <- carbon_ts_2022

# Fit SES model
ses_model <- ses(carbon_ts_2022_ses, h=2)  # Forecast for the 2023 to 2024

# Evaluation
accuracy(ses_model)

# SES model fit
summary(ses_model)
# MAD
mean(abs(ses_model$residuals))
# MSD
mean(ses_model$residuals^2)
# AIC
ses_model$model$aic

# Extract 95CI bounds
lower_ci_ses <- ses_model$lower[, 2]  
upper_ci_ses <- ses_model$upper[, 2] 

# Extract time indices
time_original_ses <- as.numeric(time(carbon_ts_2022_ses))  # Convert to numeric
time_forecast_ses <- seq(max(time_original_ses) + 1, by = 1, length.out = length(ses_model$mean))  # Future years

# Total range for y-axis
range_total_ses <- range(carbon_ts_2022_ses, ses_model$mean,lower_ci_ses, upper_ci_ses)
range_total_ses

# Plot original data (incl 2023)
plot(carbon_ts, type = "o", col = "blue",lwd = 1, xlab = "Year", ylab = "MtCO2e",
     main = "Simple Exponential Smoothing Forecast", xlim=c(1981,2024), ylim= range_total_ses, xaxt = 'n')
axis(1, at = 1981:2024, labels = 1981:2024, las = 2)  # Set custom axis labels
# Add fitted values (smoothed historical data)
lines(ses_model$fitted, col = "green", lwd = 1, type="o")

# Add 95% CI
polygon(c(time_forecast_ses, rev(time_forecast_ses)), 
        c(upper_ci_ses, rev(lower_ci_ses)), 
        col = "lightgrey", border = NA) 

# Add forecasted values
lines(ses_model$mean, col = "red", lwd = 1, lty = 1, type="o")

# Add legend
legend("topleft", legend = c("Original", "Fitted", "Forecast"),
       col = c("blue", "green", "red"), lwd = 2, lty = c(1,1,1))

# Forecasted SES values
ses_model$mean


# ACF PACF on SES residuals
acf(ses_model$residuals, main= "ACF of Residuals from SES")
pacf(ses_model$residuals, main="Partial ACF Function of Residuals from SES")

# p-value SES residue
Box.test(ses_model$residuals, lag=10, type = "Ljung-Box")

# Extract 2023 SES forecast
forecast_2023_ses <- ses_model$mean [1]
forecast_2023_ses <- as.numeric(forecast_2023_ses)

# Calculate MSD for SES
msd_ses <- mean((forecast_2023_ses - actual_2023)^2)
msd_ses

# Calculate MAPE for SES
mape_ses <- mean(abs((forecast_2023_ses - actual_2023) / actual_2023)) * 100
mape_ses


# Holt Exponential Smoothing####
# Create time series to use for Holt ES
carbon_ts_2022_h <- carbon_ts_2022
# Holt Exponential smoothing
holt_model <- holt(carbon_ts_2022_h, h=2)

# Extract 95CI
lower_ci_h <- holt_model$lower[, 2]  
upper_ci_h <- holt_model$upper[, 2]  

# Total range 
range_total_h <- range(carbon_ts_2022_h, holt_model$mean,lower_ci_h, upper_ci_h)
range_total_h

# Extract time indices
time_original_h <- as.numeric(time(carbon_ts_2022_h))  # Convert to numeric
time_forecast_h <- seq(max(time_original_h) + 1, by = 1, length.out = length(holt_model$mean))  # Future years

# Plot original and forecast (incl 2023)
plot(carbon_ts, type = "o", col = "blue", lwd = 1, xlab = "Year", ylab = "MtCO2e",
     main = "Holt's Exponential Smoothing Forecast",
     xlim = c(1981, 2024), xaxt = 'n',
     ylim= range_total_h)  
axis(1, at = 1981:2024, labels = 1981:2024, las = 2) #custom axis

# Add fitted values (green line)
lines(time_original_h, holt_model$fitted, col = "green", lwd = 1, type="o")

# Add 95% CI(shaded grey area)
polygon(c(time_forecast_h, rev(time_forecast_h)), 
        c(upper_ci_h, rev(lower_ci_h)), 
        col = "lightgrey", border = NA) 
# Add forecasted values (red dashed line) with correct time index
lines(time_forecast_h, holt_model$mean, col = "red", lwd = 1, lty =1, type="o")

# Add a legend
legend("topleft", legend = c("Original", "Fitted", "Forecast"),
       col = c("blue", "green", "red"), lwd = 1, lty = c(1, 1, 1))

# Holt forecast values
holt_model$mean

# ACF PACF residues
acf(holt_model$residuals, main="ACF of residuals from Holt's Exponential Smoothing")
pacf(holt_model$residuals, main="PACF of residuals from Holt's Exponential Smoothing")

# p-value SES residue
Box.test(holt_model$residuals, lag=10, type = "Ljung-Box")

# qqplot of residuals
qqnorm(holt_model$residuals, main="QQ Plot of Holt Model Residuals")
qqline(holt_model$residuals, col="red", lwd=2)  # Add reference line

# Evaluation of SES model fit
accuracy(holt_model)
# MAD of model fit
mean(abs(holt_model$residuals))
# MSD of model fit
mean(holt_model$residuals^2)
# AIC of model fit
holt_model$model$aic

# Compare actual 2023 to holt 2023 forecast
forecast_2023_h <- holt_model$mean [1]
forecast_2023_h <- as.numeric(forecast_2023_h)

# Calculate MSD for Holt
msd_h <- mean((forecast_2023_h - actual_2023)^2)
msd_h

# Calculate MAPE for Holt
mape_h <- mean(abs((forecast_2023_h - actual_2023) / actual_2023)) * 100
mape_h


# Holt Winter Exponential smoothing####
# Create time series for HW
carbon_ts_2022_hw <- carbon_ts_2022

# Apply Holt-Winters (automatically selects best alpha, beta, gamma)
hw_model <- hw(carbon_ts_2022_hw, h = 2,seasonal = "additive")  # Forecast 2 years

#unable to proceed with HW as timeseries freq needs to be >1

# ARIMA####
# Create time series for arima
carbon_ts_2022_am <- carbon_ts_2022

# Stationary test - if p<0.05, reject null hypothesis, series is stationary
# Perform the Augmented Dickey-Fuller Test
adf.test(carbon_ts_2022_am)
# p=0.386, need to difference the data

# Check how many differences are needed
ndiffs(carbon_ts_2022_am)

#ndiff is 1 indicate first order differencing

# Perform differencing
carbon_am_diff <- diff(carbon_ts_2022_am, differences = 1)

#adf test on differenced ts
adf.test(carbon_am_diff)
#p<0.01 hence ts now stationary and can be used for arima

# ACF PACF on differenced data to find p,d,q
acf(carbon_am_diff, main= "ACF on differenced time series") 
pacf(carbon_am_diff, main= "PACF on differenced time series") 

#acf decays quickly to 0
#pacf decays slowly to 0 
#suggestive of MA (0,1,q) 
#acf cuts off after lag1 indicative of q=1

# For the model with ARIMA(0,1,1) assuming use the original ts and not differenced
manual_arima_model <- Arima(carbon_ts_2022_am, order = c(0, 1, 1))

# Evaluation of model fit
summary(manual_arima_model)
# MAD of model fit
mean(abs(manual_arima_model$residuals))
# MSD of model fit
mean(manual_arima_model$residuals^2)
# AIC of model fit
AIC(manual_arima_model)

# ARIMA forecast with the manual ari
forecast_values_am <- forecast::forecast(manual_arima_model, h=2)
forecast_values_am
forecast_values_am$mean #Forecasted values

# Extract 95CI
lower_ci_am <- forecast_values_am$lower[, 2]  
upper_ci_am <- forecast_values_am$upper[, 2]  

# Total range 
range_total_am <- range(carbon_ts_2022_am, forecast_values_am$mean,lower_ci_am, upper_ci_am)
range_total_am

# Extract time indices
time_original_am <- as.numeric(time(carbon_ts_2022_am))  # Convert to numeric
time_forecast_am <- seq(max(time_original_am) + 1, by = 1, length.out = length(forecast_values_am$mean))  # Future years

# Plot original and forecast (incl 2023)
plot(carbon_ts, type = "o", col = "blue", lwd = 1, xlab = "Year", ylab = "MtCO2e",
     main = "ARIMA forecast",
     xlim = c(1981, 2024), xaxt = 'n',
     ylim= range_total_h)  
axis(1, at = 1981:2024, labels = 1981:2024, las = 2) #custom axis

# Add fitted values (green line)
lines(time_original_am, manual_arima_model$fitted, col = "green", lwd = 1, type="o")

# Add 95% CI(shaded grey area)
polygon(c(time_forecast_am, rev(time_forecast_am)), 
        c(upper_ci_am, rev(lower_ci_am)), 
        col = "lightgrey", border = NA) 

# Add forecasted values (red dashed line) with correct time index
lines(time_forecast_am, forecast_values_am$mean, col = "red", lwd = 1, lty =1, type="o")

# Add a legend
legend("topleft", legend = c("Original", "Fitted", "Forecast"),
       col = c("blue", "green", "red"), lwd = 1, lty = c(1, 1, 1))

# Evaluation of coeff
summary(manual_arima_model)
pvalue(manual_arima_model$coef)

# ACF PACF on arima residuals
acf(manual_arima_model$residuals, main="ACF on residuals from Arima")
pacf(manual_arima_model$residuals, main="PACF on residuals from Arima")
checkresiduals(manual_arima_model)

# Box test for p-value of residuals
Box.test(manual_arima_model$residuals, lag=10, type = "Ljung-Box")

# Compare actual 2023 to arima 2023 forecast
# Extract 2023 forecast from ARIMA
forecast_2023_arima <- forecast_values_am$mean [1]
forecast_2023_arima <- as.numeric(forecast_2023_arima)

# Calculate MSD for arima
msd_arima <- mean((forecast_2023_arima - actual_2023)^2)
msd_arima

# Calculate MAPE for arima
mape_arima <- mean(abs((forecast_2023_sma3 - actual_2023) / actual_2023)) * 100
mape_arima

# Data import and cleaning of national income data ####
# Import national income csv
income_df <- read.csv("income.csv")

# Keep only the first row
income_df <- income_df[1, ]

# Rename year columns to keep only the years
colnames(income_df) <- gsub("^X|\\.\\.YR[0-9]+\\.", "", colnames(income_df))
print(income_df)

# Convert ".." to NA
income_df[income_df == ".."] <- NA  # Replaces ".." with NA

# Remove specific columns using select()
income_df <- income_df %>%
  select(-Country.Name, -Country.Code, -Series.Code)

# Convert year columns to numeric
income_df[, -1] <- lapply(income_df[, -1], as.numeric)

# Reshape from wide to long format
income_df <- income_df %>%
  pivot_longer(cols = -Series.Name, names_to = "Year", values_to = "Income") %>%
  mutate(Year = as.numeric(Year))  # Convert Year column to numeric

# Remove the Series.Name column and keep only Year and Income
income_df <- income_df %>%
  select(Year, Income)

# Check for missing values
income_df_missing <- is.na(income_df$Income)
income_df_missing

# Check for outliers
income_df_iqr <- quantile(income_df$Income, probs = c(0.25, 0.75), na.rm = TRUE) + c(-1.5, 1.5) * IQR(income_df$Income, na.rm=TRUE)
income_df$Income[(income_df$Income< income_df_iqr[1]) | (income_df$Income > income_df_iqr[2])]

# Ensure Year and Income are numeric
income_df$Year <- as.numeric(income_df$Year)
income_df$Income <- as.numeric(income_df$Income)

# Linear extrapolation
income_df$Income <- approx(income_df$Year, income_df$Income, xout = income_df$Year, rule = 2)$y

# Merge income and carbon df
merged_df <- full_join(carbon_df, income_df, by = "Year")

# View merged data
head(merged_df)

# Convert to base R df
merged_df <- as.data.frame(merged_df)

# Correlation between CO2 and income
cor(merged_df$Carbon, merged_df$Income, use = "complete.obs")

# Fit a linear regression model
model_carbon_income <- lm(Carbon ~ Income, data = merged_df)

# View the model summary
summary(model_carbon_income)

# Plot merged_df
ggplot(merged_df) +
  # Plot carbon emissions on the left y-axis
  geom_line(aes(x = Year, y = Carbon, color = "Carbon Emissions"), size = 1) +
  scale_y_continuous(name = "Carbon Emissions", sec.axis = sec_axis(~ ., name = "Income per Capita")) +
  
  # Plot income on the right y-axis
  geom_line(aes(x = Year, y = Income / 1000, color = "Income per Capita"), size = 1) + # Scale income for better visibility
  
  # Customize the appearance of the plot
  labs(title = "Carbon Emissions and Income per Capita Over Time",
       x = "Year",
       y = "Carbon Emissions (units)",
       color = "Legend") +
  
  # Use a manual color scale for the legend
  scale_color_manual(values = c("Carbon Emissions" = "blue", "Income per Capita" = "red")) +
  
  # Customize the theme for better visibility
  theme_minimal() +
  theme(legend.position = "bottom")


# Log scale plot
ggplot(merged_df) +
  geom_line(aes(x = Year, y = Carbon, color = "MtCO2e"), size = 1) +
  scale_y_continuous(name = "MtCO2e", sec.axis = sec_axis(~ ., name = "Income per Capita (US$)")) +
  geom_line(aes(x = Year, y = log(Income), color = "Income per Capita (US$)"), size = 1) + 
  labs(title = "CO2 emission and Log-transformed Income per Capita Over Time", x = "Year", y = "MtCO2e", color = "Legend") +
  scale_color_manual(values = c("MtCO2e" = "blue", "Income per Capita (US$)" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Convert income df to time series
# Convert to time series object (incl 2023)
income_ts <- ts(income_df$Income, start=1981,end=2023, frequency = 1)

# Check if income ts is stationary
adf.test(income_ts) #p=0.41 need to difference

# Differece 
income_diff <- diff(income_ts, differences = 1)

# Check if differenced ts is stationary
adf.test(income_diff) #still non-stationary

# Difference again
income_diff2 <- diff(income_diff, differences = 1)
adf.test(income_diff2) # differenced data is now stationary

# Use stationary differenced carbon_am_diff
# Cross-Correlation Function (CCF)
ccf(carbon_am_diff, income_diff2, lag.max=20, main = "CCF: CO2 Emission vs Net Income Per Capita")

# Granger test to determine casuality
grangertest(income_diff2 ~ carbon_am_diff, order = 10)

