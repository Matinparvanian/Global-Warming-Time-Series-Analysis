library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)
library(fpp2)
library(dplyr)
library(car)
library(gridExtra) 
############################################################################################################
# Data presentation
############################################################################################################
df <- read.csv("data/global-co2-concentration.csv")
View(df)
attach(df)
head(df)
CO2 =  Monthly.concentration.of.atmospheric.carbon.dioxide
Date = Day
tsdisplay(CO2, lag.max = 600)
CO2_ts <- ts(CO2, frequency = 12, start = c(1979, 1))
seasonplot(CO2_ts, ylab = "Monthly concentration of Atmospheric Carbon Dioxide", xlab = "Month of Year",
           main = "Seasonal Plot: concentration of Atmospheric Carbon Dioxide", 
           year.labels = TRUE, year.labels.left = TRUE, 
           col = 1:40, pch = 19)


decomposition <- stl(CO2_ts, s.window = 'periodic')


components <- data.frame(
  Date = as.Date(time(CO2_ts)), # Convert time series to dates
  Observed = as.numeric(CO2_ts), # Original data
  Seasonal = decomposition$time.series[, "seasonal"],
  Trend = decomposition$time.series[, "trend"],
  Remainder = decomposition$time.series[, "remainder"]
)

p1 <- ggplot(components, aes(x = Date, y = Observed)) +
  geom_line(color = "blue") +
  labs(title = "Observed CO2", x = "", y = "CO2") +
  theme_minimal()

p2 <- ggplot(components, aes(x = Date, y = Seasonal)) +
  geom_line(color = "orange") +
  labs(title = "Seasonal Component", x = "", y = "Seasonal") +
  theme_minimal()

p3 <- ggplot(components, aes(x = Date, y = Trend)) +
  geom_line(color = "green") +
  labs(title = "Trend Component", x = "", y = "Trend") +
  theme_minimal()

p4 <- ggplot(components, aes(x = Date, y = Remainder)) +
  geom_line(color = "red") +
  labs(title = "Remainder", x = "Date", y = "Remainder") +
  theme_minimal()

grid.arrange(p1, p2, p3, p4, ncol = 1)


###################################################################################################################
# TSLM model  
###################################################################################################################
ts.plot(CO2_ts, type="o")

## we fit a linear model with the tslm function
TSLM_model<- tslm(CO2_ts~ trend + season, data = df)

###obviously it gives the same results of the first model
summary(TSLM_model)
accuracy(TSLM_model)

par(mfrow = c(1, 2)) 
par(cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.6) 

plot(CO2_ts, 
     xlab = "Time (Month of Year)", 
     ylab = "Monthly concentration of Atmospheric Carbon Dioxide",
     main = "Time Series with Fitted Values")
lines(fitted(TSLM_model), col = 2)


TSLM_fore <- forecast(TSLM_model, h = 12)
plot(TSLM_fore, 
     main = "Forecast for the Next 12 Months for Atmospheric Carbon Dioxide", 
     xlab = "Time (Month of Year)", 
     ylab = "Monthly concentration of Atmospheric Carbon Dioxide")

par(mfrow = c(1, 1))

TSLM_res<- residuals(TSLM_model)
par(mfrow = c(1,3))
par(cex.lab = 0.9, cex.axis = 0.9, cex.main = 0.9) 
plot(TSLM_res,xlab="Time(Month of Year)", ylab="residuals", main = "TSLM Model Residuals Plot")
Acf(TSLM_res, lag.max = 22*12,xlab="Time(Month of Year)", ylab="ACF for Atmospheric Carbon Dioxide" , main = "ACF for TSLM Model")
pacf(TSLM_res, lag.max = 22*12,xlab="Time(Month of Year)", ylab="PACF for Atmospheric Carbon Dioxide",main = "PACF for TSLM Model")
par(mfrow = c(1,1))

dwtest(TSLM_model)


###################################################################################################################
##Exponential Smoothing method
###################################################################################################################
### Holt-Winters
autoplot(CO2_ts)

fit1<- hw(CO2_ts, seasonal="additive")
fit2<- hw(CO2_ts, seasonal="multiplicative")

autoplot(CO2_ts, series = "Original Time Series") +
  autolayer(fit1, series = "Additive Holt-Winters", PI = FALSE) +
  autolayer(fit2, series = "Multiplicative Holt-Winters", PI = FALSE) +
  ggtitle("Holt-Winters Forecasts with Different Seasonal Models") +
  xlab("Time (Month of Year)") +
  ylab("Monthly Concentration of Atmospheric Carbon Dioxide") +
  theme_minimal() +
  scale_color_manual(
    values = c("Original Time Series" = "black",
               "Additive Holt-Winters" = "blue", 
               "Multiplicative Holt-Winters" = "red")
  ) +
  theme(
    plot.title = element_text(size = 8),        
    axis.title.x = element_text(size = 8),      
    axis.title.y = element_text(size = 8),      
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 8),      
    legend.title = element_text(size = 8) 
  )

  
# Evaluate performance of fit1 and fit2
accuracy_fit1 <- accuracy(fit1)
accuracy_fit2 <- accuracy(fit2)

par(mfrow = c(2,2))
par(cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7) 
res_fit1 <- residuals(fit1)
Acf(res_fit1, main = "ACF of Residuals (Additive)", lag.max = 22*12)
pacf(res_fit1, main = "PACF of Residuals (Additive)",lag.max = 22*12)

res_fit2 <- residuals(fit2)
Acf(res_fit2, main = "ACF of Residuals (Multiplicative)",lag.max = 22*12)
pacf(res_fit2, main = "PACF of Residuals (Multiplicative)",lag.max = 22*12)
par(mfrow = c(1,1))

########################################################################################################
# ARIMA Models
########################################################################################################

# SARIMA
sarima_model <- Arima(CO2_ts, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 0), period = 12))
fit1<- fitted(sarima_model)
par(mfrow = c(1, 2))
par(cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7) 
plot(CO2_ts, 
     xlab = "Time (Month of Year)", 
     ylab = "Monthly concentration of Atmospheric Carbon Dioxide",
     main = "Time Series with Fitted Values")
lines(fitted(sarima_model), col = 2)
legend("topleft", 
       legend = c("Observed", "Fitted (SARIMA)"), 
       col = c("black", "red"), 
       lty = c(1, 1), 
       bty = "n", 
       cex = 0.8)
sarima_fore <- forecast(sarima_model, h = 12)
plot(sarima_fore, 
     main = "Forecast for the Next 12 Months for Atmospheric Carbon Dioxide", 
     xlab = "Time (Month of Year)", 
     ylab = "Monthly concentration of Atmospheric Carbon Dioxide")
legend("topleft", 
       legend = c("Forecast", "95% Prediction Interval"), 
       col = c("blue", "gray"), 
       lty = c(1, NA), 
       fill = c(NA, "gray"), 
       bty = "n", 
       cex = 0.8)
par(mfrow = c(1, 1))
par(mfrow = c(1,2))
par(cex.lab = 1, cex.axis = 1, cex.main = 1) 
res_fit3 <- residuals(sarima_model)
Acf(res_fit3, main = "ACF of Residuals(Sarima)", lag.max = 22*12)
pacf(res_fit3, main = "PACF of Residuals(Sarima)",lag.max = 22*12)
par(mfrow = c(1,1))
accuracy_arima_fit1 <- accuracy(sarima_model)


# Auto ARIMA
par(mfrow = c(1, 2))
par(cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7)
auto.a <- auto.arima(CO2_ts)
auto.a_fit1 <- fitted(auto.a)
summary(auto.a)
par(mfrow = c(1, 2))
par(cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7) 
plot(CO2_ts, 
     xlab = "Time (Month of Year)", 
     ylab = "Monthly concentration of Atmospheric Carbon Dioxide",
     main = "Time Series with Fitted Values")
lines(auto.a_fit1, col = 2)
legend("topleft", 
       legend = c("Observed", "Fitted (Auto ARIMA)"), 
       col = c("black", "red"), 
       lty = c(1, 1), 
       bty = "n", 
       cex = 0.8)

f2 <- forecast(auto.a, h = 12)
plot(f2, 
     main = "Forecast for the Next 12 Months for Atmospheric Carbon Dioxide", 
     xlab = "Time (Month of Year)", 
     ylab = "Monthly concentration of Atmospheric Carbon Dioxide")
legend("topleft", 
       legend = c("Forecast", "95% Prediction Interval"), 
       col = c("blue", "gray"), 
       lty = c(1, NA), 
       fill = c(NA, "gray"), 
       bty = "n", 
       cex = 0.8)
par(mfrow = c(1, 1))
par(mfrow = c(1,2))
par(cex.lab = 1, cex.axis = 1, cex.main = 1) 
res_fit3 <- residuals(auto.a)
Acf(res_fit3, main = "ACF of Residuals(ARIMA)", lag.max = 22*12)
pacf(res_fit3, main = "PACF of Residuals(ARIMA)",lag.max = 22*12)
par(mfrow = c(1,1))
accuracy_arima_fit2 <- accuracy(f2)
