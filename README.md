# Seminar---Assets-Pricing-and-Financial-Markets

# The following code is used to create the HAR model estimations and forecasts:
# Define academic paper colors
NavyBlue_color <- rgb(0/255,20/255,40/255)
AntiqueWhite_color <- rgb(250/255,235/255,200/255)
DeepForestGreen_color <- rgb(0/255,100/255,0/255)
CharcoalGray_color <- rgb(80/255,80/255,80/255)
Maroon_color <- rgb(80/255, 0/255, 20/255)
GunmetalGray_color <- rgb(70/255, 80/255, 90/255)
DeepOrange_color <- rgb(204/255, 102/255, 0/255)

# Load the needed packages
library(readxl)
library(tbl2xts)
library(HARModel)

# Import data for the realized volatility from November 2018 until December 2023
my_data <- read_excel("INSERT FILE PATH TO DATA")
SP500RM <- tbl_xts(my_data, Colnames_Exact = FALSE)
SP500rv = sqrt(252*SP500RM$RV) # SP500rv = SP500RM$RV sqrt(252*SP500RM$RV)

################### HAR model estimation and fit ###################

# Estimate the HAR model:
FitHAR = HAREstimate(RM = SP500rv, periods = c(1,5,22))

# Show the model:
show(FitHAR)
# Extract the estimated coefficients:
# Plot the fitted values
plot(FitHAR)
# Calculate the Q-like loss-function:
mean(qlike(FitHAR))


################### Rolling Window Forecasting ###################

in_sample_period <- SP500RM["/2022-05-13"] # SP500RM["/2022-05-13"]
out_of_sample_period <- SP500RM["2022-05-14/"] # SP500RM["/2022-05-14"]
in_sample <- nrow(in_sample_period) # Size of in-sample period
out_of_sample <- nrow(out_of_sample_period) # Size of out-of-sample period

# Setting up the specification for the HAR forecast
ForecastHAR = HARForecast(SP500rv, periods = c(1,5,22), nRoll = out_of_sample,
                          nAhead = 1, type = "HAR", windowType = "rolling")

# Show the forecated series of realized volatility
show(ForecastHAR)

################### Performance Evaluation ###################

# Calculate the prediction performance metrics:
# Retrieve the series of actual realized volatility
HAR_f <- as.data.frame(ForecastHAR@data[["forecastComparison"]])
forecast.matrix = matrix(data = ForecastHAR@forecast, ncol = 1, byrow = TRUE) 
forecast.matrix <- as.data.frame(forecast.matrix)


# Compare in-sample and rolling sample volatility in one plot
# Organize data into xts format
harvolroll <- xts(forecast.matrix, order.by = as.Date(rownames(HAR_f)))
harvoloutsample <- xts(HAR_f$RV, order.by = as.Date(rownames(HAR_f)))
volplot <- plot(harvoloutsample, col = NavyBlue_color, lwd = 1, ylab="Annualised Standard Deviation in %")
volplot <- addSeries(harvolroll, col = DeepOrange_color, lwd = 1.5, on = 1)
volplot <- addLegend("topright", legend.names = c("Realized Volatility", "HAR Forecast"), col = c(NavyBlue_color,DeepOrange_color), lty = 1, lwd = c(1, 1.5), cex = 0.8)
plot(volplot)

# Define the prediction error, the sqrt is apllied to each series, as they are realized variances and we want realized volatility (sqrt(RV))
pred_error = (forecast.matrix-HAR_f$RV) # pred_error = (sqrt(HAR_f$RV) - sqrt(ForecastHAR@forecast))
MSE <- colMeans(pred_error^2)
MAE <- colMeans(abs(pred_error))
RMSE <- sqrt(MSE)

# Compile all prediction metrics in one table
Performance_evaluation <- rbind(MSE,MAE,RMSE)
rownames(Performance_evaluation) <- c("MSE", "MAE", "RMSE")
colnames(Performance_evaluation) <- c("Performance Measures")
Performance_evaluation

###### Plotted prediction errors #################
pred_error_plot <-xts(pred_error, order.by = as.Date(rownames(HAR_f)))
forecast_plot <- plot(pred_error_plot, col = NavyBlue_color, lwd = 1,type="h")
plot(forecast_plot)

########## LATEX Export ############
#sink(file = "HAR_fit.txt")
#show(FitHAR)
#summary(FitHAR)
#sink(file = NULL)


##############################
# The following code is used to create the EGARCH model estimations and forecasts:
# Define academic paper colors
NavyBlue_color <- rgb(0/255,20/255,40/255)
AntiqueWhite_color <- rgb(250/255,235/255,200/255)
DeepForestGreen_color <- rgb(0/255,100/255,0/255)
CharcoalGray_color <- rgb(80/255,80/255,80/255)
Maroon_color <- rgb(80/255, 0/255, 20/255)
GunmetalGray_color <- rgb(70/255, 80/255, 90/255)
DeepOrange_color <- rgb(204/255, 102/255, 0/255)

# Load in the needed packages
library(tidyverse)
library(readxl)
library(tbl2xts)
library(rugarch)
library(quantmod)

# Import daily returns S&P 500 from 2010 until December 2023
my_data <- read_excel("INSERT FILE PATH TO DATA")
SP500 <- tbl_xts(my_data, Colnames_Exact = FALSE)

# Calculate daily log-returns from S&P 500 index returns
returns <- (log(SP500) - log(lag(SP500)))*100
returns <- returns[!is.na(returns)]

# The selected GARCH model specification, in our case the EGARCH(1,1) model is applied 
garchspec <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                        variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                        distribution.model = "norm")
# show(garchspec)

# Fit the model to the data
garchfit <- ugarchfit(data = returns, spec = garchspec)
show(garchfit)


####### Rolling Window Forecasting ####### 

# Define the in-sample and out-of-sample periods and sample sizes for the rolling window forecast
in_sample_period <- returns["/2022-05-13"] # returns["/2022-05-13"]
out_of_sample_period <- returns["2022-05-14/"] # returns["/2022-05-14"]
in_sample_size <- nrow(in_sample_period)  
out_of_sample_size <- nrow(out_of_sample_period)

# Define the forecast horizon - One day ahead forecast
forecast_horizon <- 1 

# Perform the rolling window forecast starting from the in-sample period end until the end of the time series
garchroll <- ugarchroll(garchspec, data = returns, n.start = in_sample_size,
                        refit.window = "moving", refit.every = forecast_horizon)


############### Realized Volatility data ##############

SP500RM <- read_excel("/Users/jonasbarslund/Documents/Økonomi - 11 semester/Asset Pricing and Financial Markets/SP500_5min_R.xlsx")
SP500RM <- tbl_xts(SP500RM, Colnames_Exact = FALSE)
SP500rv = sqrt(252*SP500RM$RV) # SP500rv = SP500RM$RV sqrt(252*SP500RM$RV)
out_of_sample_period_RV <- SP500rv["2022-05-14/"] # SP500RM["/2022-05-14"]
out_of_sample_RV <- nrow(out_of_sample_period_RV) # Size of out-of-sample period

#######################################################

# EGARCH_Forecast_Results <- as.data.frame(garchroll)
EGARCH_Forecast_Results <- as.data.frame(garchroll@forecast$density)
EGARCH_Sigma_forecast <- EGARCH_Forecast_Results$Sigma*sqrt(252)

# Compare in-sample and rolling sample volatility in one plot
# Organize data into xts format
garchvolroll <- xts(EGARCH_Sigma_forecast, order.by = as.Date(rownames(EGARCH_Forecast_Results)))
garchvoloutsample <- xts(out_of_sample_period_RV, order.by = as.Date(rownames(EGARCH_Forecast_Results)))
volplot <- plot(garchvoloutsample, col = NavyBlue_color, lwd = 1, ylab="Annualised Standard Deviation in %")
volplot <- addSeries(garchvolroll, col = DeepOrange_color, lwd = 1.5, on = 1)
volplot <- addLegend("topright", legend.names = c("Realized Volatility", "EGARCH Forecast"), col = c(NavyBlue_color,DeepOrange_color), lty = 1, lwd = c(1, 1.5), cex = 0.8)
plot(volplot)


#### Performance measures ####
# MSE - Prediction error for volatility
pred_error <- (EGARCH_Sigma_forecast - out_of_sample_period_RV) # (sqrt(pred_error_mean^2) - sqrt(EGARCH_Forecast_Results$Sigma^2))
MSE_sigma = mean(pred_error^2)
MAE_sigma <- mean(abs(pred_error))
RMSE_sigma <- sqrt(mean(pred_error ^ 2))

Performance_evaluation_sigma <- rbind(MSE_sigma,MAE_sigma,RMSE_sigma)
rownames(Performance_evaluation_sigma) <- c("MSE", "MAE", "RMSE")
colnames(Performance_evaluation_sigma) <- c("Performance Measures")
Performance_evaluation_sigma

###### Plotted prediction errors #################
forecast_plot <- plot(pred_error, col = NavyBlue_color, lwd = 1,type="h")
plot(forecast_plot)

########## LATEX Export ############
#sink(file = "EGARCH_fit.txt")
#show(garchfit)
#sink(file = NULL)


###########################################
# The following code is used to create the VIX1D forecasts:
# Define academic paper colors
NavyBlue_color <- rgb(0/255,20/255,40/255)
AntiqueWhite_color <- rgb(250/255,235/255,200/255)
DeepForestGreen_color <- rgb(0/255,100/255,0/255)
CharcoalGray_color <- rgb(80/255,80/255,80/255)
Maroon_color <- rgb(80/255, 0/255, 20/255)
GunmetalGray_color <- rgb(70/255, 80/255, 90/255)
DeepOrange_color <- rgb(204/255, 102/255, 0/255)

# Load in the needed packages
library(tidyverse)
library(readxl)
library(tbl2xts)
library(quantmod)
library(forecast)

# Import data for the realized volatility from November 2018 until December 2023
my_data_RV <- read_excel("INSERT FILE PATH TO DATA")
SP500RM <- tbl_xts(my_data_RV, Colnames_Exact = FALSE)
SP500rv = sqrt(252*SP500RM$RV)

# Import VIX1D data from May 2022 until December 2023
my_data_VIX <- read_excel("INSERT FILE PATH TO DATA")
VIX1D <- tbl_xts(my_data_VIX, Colnames_Exact = FALSE)
VIX1D_lag <- lag(VIX1D$VIX1D)

# Load the series of realized volatility for comparison and forecast error estimation
Forecast_comparison <- cbind(SP500rv$RV,VIX1D$VIX1D, VIX1D_lag)
Forecast_comparison <- Forecast_comparison["2022-05-16/"]

forecast_plot <- plot(Forecast_comparison$RV, col = NavyBlue_color, lwd = 1, ylab="Annualised Standard Deviation in %")
forecast_plot <- addSeries(Forecast_comparison$VIX1D.1, col = DeepOrange_color, lwd = 1, on = 1)
forecast_plot <- addLegend("topright", legend.names = c("Realized Volatility", "VIX1D Forecast"), col = c(NavyBlue_color,DeepOrange_color), lty = 1, lwd = c(1, 1), cex = 0.8)
plot(forecast_plot)


# Define the prediction error, the sqrt is applied to each series, as they are realized variances and we want realized volatility (sqrt(RV))
pred_error = (Forecast_comparison$VIX1D.1-Forecast_comparison$RV)
MSE <- mean(pred_error^2)
MAE <- mean(abs(pred_error))
RMSE <- sqrt(MSE)

# Compile all prediction metrics in one table
Performance_evaluation <- rbind(MSE,MAE,RMSE)
rownames(Performance_evaluation) <- c("MSE", "MAE", "RMSE")
colnames(Performance_evaluation) <- c("Performance Measures")
Performance_evaluation

###### Plotted prediction errors #################
forecast_plot <- plot(pred_error, col = NavyBlue_color, lwd = 1,type="h")
plot(forecast_plot)


###########################################
# The following code compares the performances of the models and includes the Diebold-Mariano Test
# Define academic paper colors
NavyBlue_color <- rgb(0/255,20/255,40/255)
AntiqueWhite_color <- rgb(250/255,235/255,200/255)
DeepForestGreen_color <- rgb(0/255,100/255,0/255)
CharcoalGray_color <- rgb(80/255,80/255,80/255)
Maroon_color <- rgb(80/255, 0/255, 20/255)
GunmetalGray_color <- rgb(70/255, 80/255, 90/255)
DeepOrange_color <- rgb(204/255, 102/255, 0/255)


# Load in the needed packages
library(tidyverse)
library(readxl)
library(tbl2xts)
library(quantmod)
library(forecast)
library(rugarch)
library(HARModel)

############### HAR #################
# Import data for the realized volatility from November 2018 until December 2023
my_data <- read_excel("INSERT FILE PATH TO DATA")
SP500RM <- tbl_xts(my_data, Colnames_Exact = FALSE)
SP500rv = sqrt(252*SP500RM$RV) # SP500rv = SP500RM$RV sqrt(252*SP500RM$RV)

in_sample_period_HAR <- SP500RM["/2022-05-13"] # SP500RM["/2022-05-13"]
out_of_sample_period_HAR <- SP500RM["2022-05-14/"] # SP500RM["/2022-05-14"]
in_sample_HAR <- nrow(in_sample_period_HAR) # Size of in-sample period
out_of_sample_HAR <- nrow(out_of_sample_period_HAR) # Size of out-of-sample period

# Setting up the specification for the HAR forecast
ForecastHAR = HARForecast(SP500rv, periods = c(1,5,22), nRoll = out_of_sample_HAR,
                          nAhead = 1, type = "HAR", windowType = "rolling")

# Calculate the prediction performance metrics:
# Retrieve the series of actual realized volatility
HAR_f <- as.data.frame(ForecastHAR@data[["forecastComparison"]])
forecast.matrix = matrix(data = ForecastHAR@forecast, ncol = 1, byrow = TRUE) 
forecast.matrix <- as.data.frame(forecast.matrix)

# Define the prediction error, the sqrt is apllied to each series, as they are realized variances and we want realized volatility (sqrt(RV))
pred_error = (HAR_f$RV - forecast.matrix) # pred_error = (sqrt(HAR_f$RV) - sqrt(ForecastHAR@forecast))
MSE <- colMeans(pred_error^2)
RMSE <- sqrt(MSE)
MAE <- colMeans(abs(pred_error))

# Compile all prediction metrics in one table
Performance_evaluation <- rbind(MSE,MAE,RMSE)
rownames(Performance_evaluation) <- c("MSE", "MAE", "RMSE")
colnames(Performance_evaluation) <- c("Performance Measures")
Performance_evaluation

################# EGARCH ##################
# Import daily returns S&P 500 from 2010 until December 2023
my_data <- read_excel("INSERT FILE PATH TO DATA")
SP500 <- tbl_xts(my_data, Colnames_Exact = FALSE)

returns <- (log(SP500) - log(lag(SP500)))*100
returns <- returns[!is.na(returns)]

# The selected GARCH model specification, in our case the EGARCH(1,1) model is applied 
garchspec <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                        variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                        distribution.model = "norm")
garchfit <- ugarchfit(data = returns, spec = garchspec)

####### Rolling Window Forecasting ####### 

# Define the in-sample and out-of-sample periods and sample sizes for the rolling window forecast
in_sample_period <- returns["/2022-05-13"] # returns["/2022-05-13"]
out_of_sample_period <- returns["2022-05-14/"] # returns["/2022-05-14"]
in_sample_size <- nrow(in_sample_period)  
out_of_sample_size <- nrow(out_of_sample_period)

# Define the forecast horizon - One day ahead forecast
forecast_horizon <- 1 

# Perform the rolling window forecast starting from the in-sample period end until the end of the time series
garchroll <- ugarchroll(garchspec, data = returns, n.start = in_sample_size,
                        refit.window = "moving", refit.every = forecast_horizon)


# EGARCH_Forecast_Results <- as.data.frame(garchroll)
EGARCH_Forecast_Results <- as.data.frame(garchroll@forecast$density)
EGARCH_Sigma_forecast <- EGARCH_Forecast_Results$Sigma*sqrt(252)

#### Performance measures ####

# MSE - Prediction error for volatility
pred_error_sigma <- (HAR_f$RV - EGARCH_Sigma_forecast) # (sqrt(pred_error_mean^2) - sqrt(EGARCH_Forecast_Results$Sigma^2))
MSE_sigma = mean(pred_error_sigma^2)
MAE_sigma <- mean(abs(pred_error_sigma))
RMSE_sigma <- sqrt(mean(pred_error_sigma ^ 2))

Performance_evaluation_sigma <- rbind(MSE_sigma,MAE_sigma,RMSE_sigma)
rownames(Performance_evaluation_sigma) <- c("MSE", "MAE", "RMSE")
colnames(Performance_evaluation_sigma) <- c("Performance Measures")
Performance_evaluation_sigma


################# VIX1D ##################
# Import VIX1D data from May 2022 until December 2023
my_data_VIX <- read_excel("INSERT FILE PATH TO DATA")
VIX1D <- tbl_xts(my_data_VIX, Colnames_Exact = FALSE)
VIX1D_lag <- lag(VIX1D$VIX1D)

# Load the series of realized volatility fore comparison and forecast error estimation
Forecast_comparison <- cbind(SP500rv$RV,VIX1D$VIX1D, VIX1D_lag)
Forecast_comparison <- Forecast_comparison["2022-05-16/"]

# Define the prediction error, the sqrt is apllied to each series, as they are realized variances and we want realized volatility (sqrt(RV))
pred_error_VIX1D = (Forecast_comparison$RV - Forecast_comparison$VIX1D.1)
MSE_VIX1D <- mean(pred_error_VIX1D^2)
RMSE_VIX1D <- sqrt(MSE_VIX1D)
MAE_VIX1D <- mean(abs(pred_error_VIX1D))

# Compile all prediction metrics in one table
Performance_evaluation_VIX1D <- rbind(MSE_VIX1D,MAE_VIX1D,RMSE_VIX1D)
rownames(Performance_evaluation_VIX1D) <- c("MSE", "MAE", "RMSE")
colnames(Performance_evaluation_VIX1D) <- c("Performance Measures")
Performance_evaluation_VIX1D

############## Diebold-Mariano Test ##############
# EGARCH vs. HAR model
DM_EGARCH_HAR <- dm.test(pred_error_sigma, pred_error$V1, alternative ="two.sided", power = 2)
DM_EGARCH_HAR
# EGARCH vs. VIX1D 
DM_EGARCH_VIX1D <- dm.test(pred_error_sigma, pred_error_VIX1D$RV, alternative ="two.sided", power = 2)
DM_EGARCH_VIX1D
# HAR vs. VIX1D 
DM_HAR_VIX1D <- dm.test(pred_error$V1, pred_error_VIX1D$RV, alternative ="two.sided", power = 2) 
DM_HAR_VIX1D

# Compile all test statistics in one table
Diebold_Mariano_Test <- matrix(c(DM_EGARCH_HAR$statistic, DM_EGARCH_HAR$p.value, DM_EGARCH_VIX1D$statistic, DM_EGARCH_VIX1D$p.value, DM_HAR_VIX1D$statistic, DM_HAR_VIX1D$p.value), ncol=2, byrow=TRUE)
colnames(Diebold_Mariano_Test) <- c('Text Statistics','p-value')
rownames(Diebold_Mariano_Test) <- c("EGARCH(1,1) | HAR","EGARCH(1,1) | VIX1D", "HAR | VIX1D")
Diebold_Mariano_Test_Table <- as.table(Diebold_Mariano_Test)
Diebold_Mariano_Test_Table
