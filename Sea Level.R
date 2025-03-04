library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)
library(fpp2)
library(dplyr)
############################################################################################################
# Data presentation
############################################################################################################
df <- read.csv("data/sea-level.csv")
View(df)
attach(df)
head(df)
sea_level =  Global.sea.level.according.to.Church.and.White..2011.[4:563]
Date = Day
tsdisplay(sea_level, lag.max = 600)
sea_level_ts <- ts(sea_level, frequency = 4, start = c(1881, 1))
seasonplot(sea_level_ts, ylab = "Quarterly Sea Level", xlab = "Quarterly Time Frame",
           main = "Seasonal Plot: Sea Level", 
           year.labels = TRUE, year.labels.left = TRUE, 
           col = 1:40, pch = 19)


sum(is.na(sea_level_ts))
sea_level_ts_new = na.omit(sea_level_ts) # removing missing values

decomposition = stl(sea_level_ts_new , s.window = 'periodic')

components <- data.frame(
  Date = as.Date(time(sea_level_ts_new)), # Convert time series to dates
  Observed = as.numeric(sea_level_ts_new), # Original data
  Seasonal = decomposition$time.series[, "seasonal"],
  Trend = decomposition$time.series[, "trend"],
  Remainder = decomposition$time.series[, "remainder"]
)

p1 <- ggplot(components, aes(x = Date, y = Observed)) +
  geom_line(color = "blue") +
  labs(title = "Observed Sea Level", x = "Quarterly Data", y = "Sea Level") +
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
ts.plot(sea_level_ts_new, type="o")
## we fit a linear model with the tslm function
TSLM_model<- tslm(sea_level_ts_new~ trend + season)
###obviously it gives the same results of the first model
summary(TSLM_model)
accuracy(TSLM_model)

par(mfrow = c(1, 2)) 
par(cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.2)
plot(sea_level_ts_new, 
     xlab = "Time (Quarterly)", 
     ylab = "Global Sea Level",
     main = "Time Series with Fitted Values")
lines(fitted(TSLM_model), col = 'red')
TSLM_fore <- forecast(TSLM_model, h = 4)
plot(TSLM_fore, 
     main = "Forecast for the Next 4 Months for Global Sea Level", 
     xlab = "Time (Quarterly)", 
     ylab = "Global Sea Level")
par(mfrow = c(1, 1))

TSLM_res<- residuals(TSLM_model)

par(mfrow = c(1,3))
par(cex.lab = 0.9, cex.axis = 0.9, cex.main = 0.9) 
plot(TSLM_res,xlab="Time (Quarterly)", ylab="residuals", main = "TSLM Model Residuals Plot")
Acf(TSLM_res, lag.max = 22*12,xlab="Time (Quarterly)", ylab="ACF for Global Sea Level" , main = "ACF for TSLM Model")
pacf(TSLM_res, lag.max = 22*12,xlab="Time (Quarterly)", ylab="PACF for Global Sea Level",main = "PACF for TSLM Model")
par(mfrow = c(1,1))

dwtest(TSLM_model)

####################################################################################################################
##Exponential Smoothing method
####################################################################################################################
### Holt-Winters
autoplot(sea_level_ts_new)

fit1<- hw(sea_level_ts_new, seasonal="additive")

autoplot(sea_level_ts_new, series = "Original Time Series") +
  autolayer(fit1, series = "Additive Holt-Winters", PI = FALSE) +
  ggtitle("Holt-Winters Forecasts with Different Seasonal Models") +
  xlab("Quarterly Time Frame") +
  ylab("Quarterly Sea Level") +
  theme_minimal() +
  scale_color_manual(
    values = c("Original Time Series" = "black",
               "Additive Holt-Winters" = "red")
  )+
  theme(
    plot.title = element_text(size = 11),        
    axis.title.x = element_text(size = 11),      
    axis.title.y = element_text(size = 11),      
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 14),      
    legend.title = element_text(size = 14) 
  )

  
# Evaluate performance of fit1 and fit2
accuracy_fit1 <- accuracy(fit1)

par(mfrow = c(3,1))
par(cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5) 
res_fit1 <- residuals(fit1)
plot(res_fit1, main = "Residuals for Additive Model")
Acf(res_fit1, main = "ACF of Residuals Additive Model", lag.max = 600)
pacf(res_fit1, main = "PACF of Residuals Additive Model",lag.max = 600)
par(mfrow = c(1,1))

###################################################################################################################
# Arima Models
####################################################################################################################
# SARIMA
sarima_model <- Arima(sea_level_ts_new, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 0), period = 12))

fit1<- fitted(sarima_model)

par(mfrow = c(1, 2))
par(cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.2) 
plot(sea_level_ts_new, 
     xlab = "Quarterly Time Frame", 
     ylab = "Quarterly Sea Level",
     main = "Time Series with Fitted Values")
lines(fitted(sarima_model), col = 2)
legend("topleft", 
       legend = c("Observed", "Fitted (SARIMA)"), 
       col = c("black", "red"), 
       lty = c(1, 1), 
       bty = "n", 
       cex = 0.8)


sarima_fore <- forecast(sarima_model, h = 8)
plot(sarima_fore, 
     main = "Forecast for the Next 2 years for Global Sea Level", 
     xlab = "Quarterly Time Frame", 
     ylab = "Quarterly Sea Level")
legend("topleft", 
       legend = c("Forecast", "95% Prediction Interval"), 
       col = c("blue", "gray"), 
       lty = c(1, NA), 
       fill = c(NA, "gray"), 
       bty = "n", 
       cex = 0.8) 

par(mfrow = c(1, 1))


par(mfrow = c(3,1))
par(cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.2) 
res_fit3 <- residuals(sarima_model)
plot(res_fit3, main = "Residuals for SARIMA Model")
Acf(res_fit3, main = "ACF of Residuals(Sarima)", lag.max = 600)
pacf(res_fit3, main = "PACF of Residuals(Sarima)",lag.max = 600)
par(mfrow = c(1,1))

accuracy_arima_fit1 <- accuracy(sarima_model)


# Auto ARIMA
auto.a <- auto.arima(sea_level_ts_new)
auto.a_fit1 <- fitted(auto.a)
summary(auto.a)

par(mfrow = c(1, 2))
par(cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.2) 
plot(sea_level_ts_new, 
     xlab = "Quarterly Time Frame", 
     ylab = "Quarterly Sea Level",
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
     main = "Forecast for the Next 2 years for Global Sea Level", 
     xlab = "Quarterly Time Frame", 
     ylab = "Quarterly Sea Level")
legend("topleft", 
       legend = c("Forecast", "95% Prediction Interval"), 
       col = c("blue", "gray"), 
       lty = c(1, NA), 
       fill = c(NA, "gray"), 
       bty = "n", 
       cex = 0.8)

par(mfrow = c(1, 1))


par(mfrow = c(3,1))
par(cex.lab = 1.3, cex.axis = 1.3, cex.main = 1.3) 
res_fit3 <- residuals(auto.a)
plot(res_fit3, main = "Residuals for ARIMA Model")
Acf(res_fit3, main = "ACF of Residuals(ARIMA)", lag.max = 22*12)
pacf(res_fit3, main = "PACF of Residuals(ARIMA)",lag.max = 22*12)
par(mfrow = c(1,1))

accuracy_arima_fit2 <- accuracy(f2)
