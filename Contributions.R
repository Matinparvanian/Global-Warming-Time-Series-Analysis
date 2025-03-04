library(readxl)
library(lmtest)
library(forecast)
library(DIMORA)
library(fpp2)
library(dplyr)
library(car)
#############################################################################################################################
# Data presentation
#############################################################################################################################
df <- read.csv("data/contributions-global-temp-change.csv")
View(df)
attach(df)
head(df)
GW = Share.of.contribution.to.global.warming
USA = GW[38926:39098]
China = GW[7613:7785]
European_Union = GW[12976:13148]
Russia = GW[30622:30794]
Brazil = GW[5364:5536]
India = GW[17128:17300]
United_kingdom = GW[38753:38925]
South_America = GW[34428:34600]
Africa = GW[174:346]
Date = Year


USA_ts <- ts(USA, frequency = 1, start = c(1850, 1))
China_ts <- ts(China, frequency = 1, start = c(1850, 1))
European_Union_ts <- ts(European_Union, frequency = 1, start = c(1850, 1))
Russia_ts <- ts(Russia, frequency = 1, start = c(1850, 1))
Brazil_ts <- ts(Brazil, frequency = 1, start = c(1850, 1))
India_ts <- ts(India, frequency = 1, start = c(1850, 1))
United_kingdom_ts <- ts(United_kingdom, frequency = 1, start = c(1850, 1))
South_America_ts <- ts(South_America, frequency = 1, start = c(1850, 1))
Africa_ts <- ts(Africa, frequency = 1, start = c(1850, 1))


countries_data <- data.frame(
  Year = c(time(USA_ts), time(China_ts), time(European_Union_ts), time(Russia_ts),
           time(Brazil_ts), time(India_ts), time(United_kingdom_ts), time(South_America_ts), time(Africa_ts)),
  Contribution = c(as.numeric(USA_ts), as.numeric(China_ts), as.numeric(European_Union_ts),
                   as.numeric(Russia_ts), as.numeric(Brazil_ts), as.numeric(India_ts),
                   as.numeric(United_kingdom_ts), as.numeric(South_America_ts), as.numeric(Africa_ts)),
  Country = factor(rep(c("USA", "China", "European Union", "Russia", "Brazil",
                         "India", "United Kingdom", "South America", "Africa"),
                       times = c(length(USA_ts), length(China_ts), length(European_Union_ts),
                                 length(Russia_ts), length(Brazil_ts), length(India_ts),
                                 length(United_kingdom_ts), length(South_America_ts), length(Africa_ts))))
)

ggplot(countries_data, aes(x = Year, y = Contribution, color = Country)) +
  geom_line(size = 1) +
  ggtitle("Yearly Contribution to Global Warming by Country") +
  xlab("Year") +
  ylab("Yearly Share of Contribution to Global Warming") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("USA" = "blue", "China" = "red", "European Union" = "green",
                                "Russia" = "purple", "Brazil" = "orange", "India" = "brown",
                                "United Kingdom" = "pink", "South America" = "cyan", "Africa" = "darkgreen"))

##########################################################################################################################
## Arima Model
##########################################################################################################################
perform_arima <- function(data, country_name, forecast_horizon = 5, lag=3) {
  ts_data <- ts(data, frequency = 1, start = c(1850, 1))
  arima_model <- auto.arima(ts_data)
  fc_arima <- forecast(arima_model, h = forecast_horizon)
  cat("5 Years Forecast for  ", country_name, ":\n")
  print(fc_arima)
  par(mfrow = c(3, 1))
  plot(fc_arima, main = paste("ARIMA Model for", country_name),
       xlab = "Year", ylab = "Contribution to Global Warming", col = "blue", lwd = 2)
  lines(fitted(arima_model), col = "red", lwd = 2)
  legend("topleft", legend = c("Forecast", "Fitted"), col = c("blue", "red"), lty = 1, lwd = 2)
  accuracy_arima <- accuracy(fc_arima)
  cat("Accuracy Metrics for ARIMA Model -", country_name, ":\n")
  print(accuracy_arima)
  residuals_arima <- residuals(arima_model)
  cat("Residual Summary for ARIMA Model -", country_name, ":\n")
  print(summary(residuals_arima))
  Acf(residuals_arima, main = paste("ACF of Residuals for ARIMA Model -", country_name), lag.max = 180)
  pacf(residuals_arima, main = paste("PACF of Residuals for ARIMA Model -", country_name), lag.max = 180)
  par(mfrow = c(1, 1))
  box_pierce <- Box.test(residuals_arima, type = "Box-Pierce", lag = lag)
  return(list(arima_model = arima_model, fc_arima = fc_arima, accuracy_arima = accuracy_arima, box_pierce_result = box_pierce))
}

USA_arima <- perform_arima(USA, "USA")
China_arima <- perform_arima(China, "China")
European_Union_arima <- perform_arima(European_Union, "European Union")
Russia_arima <- perform_arima(Russia, "Russia", lag = 1)
Brazil_arima <- perform_arima(Brazil, "Brazil", lag = 2)
India_arima <- perform_arima(India, "India")
United_kingdom_arima <- perform_arima(United_kingdom, "United Kingdom")
South_America_arima <- perform_arima(South_America, "South America", lag = 1)
Africa_arima <- perform_arima(Africa, "Africa")

##############################################################################################################################
## Arima model plot
##############################################################################################################################
combine_forecast_data <- function(actual_data, forecasts, country_names, start_year_actual = 1850) {
  combined_data <- data.frame()
  for (i in seq_along(forecasts)) {
    actual_years <- seq(start_year_actual, by = 1, length.out = length(actual_data[[i]]))
    actual_df <- data.frame(
      Year = actual_years,
      Contribution = actual_data[[i]],
      Country = country_names[i],
      Type = "Actual"
    )
    point_forecast <- if (is.list(forecasts[[i]])) forecasts[[i]]$mean else forecasts[[i]]
    forecast_years <- seq(max(actual_years) + 1, by = 1, length.out = length(point_forecast))
    forecast_df <- data.frame(
      Year = forecast_years,
      Contribution = as.numeric(point_forecast),
      Country = country_names[i],
      Type = "Forecast"
    )
    combined_data <- rbind(combined_data, actual_df, forecast_df)
  }
  return(combined_data)
}

# Data for ARIMA forecasts
combined_arima_data <- combine_forecast_data(
  actual_data = list(USA, China, European_Union, Russia, Brazil, India, United_kingdom, South_America, Africa),
  forecasts = list(USA_arima$fc_arima, China_arima$fc_arima, European_Union_arima$fc_arima,
                   Russia_arima$fc_arima, Brazil_arima$fc_arima, India_arima$fc_arima,
                   United_kingdom_arima$fc_arima, South_America_arima$fc_arima, Africa_arima$fc_arima),
  country_names = c("USA", "China", "European Union", "Russia", "Brazil", "India",
                    "United Kingdom", "South America", "Africa")
)

# Plot for ARIMA forecasts
ggplot(combined_arima_data, aes(x = Year, y = Contribution, color = Country, linetype = Type)) +
  geom_line(size = 1) +
  ggtitle("Yearly Contribution to Global Warming with ARIMA Point Forecasts") +
  xlab("Year") +
  ylab("Yearly Share of Contribution to Global Warming") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_linetype_manual(values = c("Actual" = "solid", "Forecast" = "dashed")) +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10))
