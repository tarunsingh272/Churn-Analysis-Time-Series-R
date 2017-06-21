#--------------------------------------------------------#

# devtools::install_github("twitter/AnomalyDetection") # AnomalyDetection
library(AnomalyDetection)
library(ggplot2)
library(Rcpp)
library(timeDate)
library(data.table)
library(tseries)
library(lubridate)
library(forecast)
# devtools::install_github("ellisp/forecastxgb-r-package/pkg") # forecastxgb
library(forecastxgb)
library(caret)
library(qlcMatrix)
library(xgboost)

#--------------------------------------------------------#

# load data
series <- read.csv("file_path//series.csv", header = TRUE, strip.white = TRUE)

#--------------------------------------------------------#

# convert date data type and rename columns

# convert date field to as.Date
series$Date <- as.Date(series$Date)

# remove . from colnames
setnames(series, old = c("Time.Entry", "Extrapolated.Time.Entry", "Employee.Requirement"), new = c("Time Entry", "Extrapolated Time Entry", "Employee Requirement"))

#--------------------------------------------------------#

# initial trend visualization

# visualzie trend in Employee Requirement per day
ggplot(series, aes(x=Date, y=`Employee Requirement`, color=`Employee Requirement`)) + geom_line() + geom_smooth(method = "lm")+ylab('Employee Requirement')+ggtitle("Trend in Employee Requirement")

#--------------------------------------------------------#

# anomaly detection

# create anomaly detection dataset
anom.series <- series

# convert date for anomaly detection
anom.series$Date <- as.POSIXct(anom.series$Date, format = "y-%m-%d")

# create anomaly detection subset
data.anomaly <- anom.series[,c("Date","Employee Requirement")]


# Apply anomaly detection
data_anomaly <- AnomalyDetectionTs(data.anomaly, max_anoms=0.1, direction="both", 
                                   plot=TRUE, e_value = T)

# visualize anomalies
data_anomaly$plot

#--------------------------------------------------------#

# prepare data for ARIMA time series analysis

# create time series objects
count.ts <- ts(series[, c('Employee Requirement')])
series$clean.cnt <- tsclean(count.ts)

#--------------------------------------------------------#

# adding ARIMA (auto-regressive integrated moving average)

# calculating weekly and monthly averages
series$`weekly requirement` <- ma(series$clean.cnt, order=7) 
series$`monthly requirement` <- ma(series$clean.cnt, order=30)

# plotting with weekly and monthly averages included
ggplot() +
  geom_line(data = series, aes(x = Date, y = clean.cnt, colour = "Counts")) +
  geom_line(data = series, aes(x = Date, y = `weekly requirement`,   colour = "Weekly Moving Average"))  +
  geom_line(data = series, aes(x = Date, y = `monthly requirement`, colour = "Monthly Moving Average"))  +
  ylab('Employee Requirement') + ggtitle("ARIMA Overlay")

# plotting weekly and monthly averages
ggplot() +
  geom_line(data = series, aes(x = Date, y = `weekly requirement`,   colour = "Weekly Moving Average"))  +
  geom_line(data = series, aes(x = Date, y = `monthly requirement`, colour = "Monthly Moving Average"))  +
  ylab('Employee Requirement') + ggtitle("ARIMA Only")

#--------------------------------------------------------#

# monthly ARIMA trend decomposition

# decomposition using monthly moving average
weekly.ARIMA <- ts(na.omit(series$`weekly requirement`), frequency=30)
monthly.ARIMA <- ts(na.omit(series$`monthly requirement`), frequency=30)
decomp <- stl(monthly.ARIMA, s.window="periodic")
decomp.week <- stl(weekly.ARIMA, s.window="periodic")
deseasonal_cnt.week <- seasadj(decomp.week)
deseasonal_cnt <- seasadj(decomp)
plot(decomp, main='Trends')

#--------------------------------------------------------#

# statistical testing for increase in employment requirement over time, stationarity, and auto correlation

# t-test to determine whether employee requirement increase between the first and second half of 2016 is statistically significant

# using monthly data
half.1 <- series[1:182,]
half.2 <- series[183:365,]

# perform t-test
t.test(half.1$`monthly requirement`, half.2$`monthly requirement`, var.equal=TRUE, paired=FALSE)

# testing whether data display stationarity
adf.test(monthly.ARIMA, alternative = "stationary", k=12) 

# auto correlation testing
Acf(monthly.ARIMA, main='Auto Correlation')

#--------------------------------------------------------#

# forecasting

# fit max liklihood forecast
fit.1 <- arima(deseasonal_cnt, order=c(1,1,4), method = "ML")
fcast.1 <- forecast(fit.1, 30) # fit.1 estimates
plot(fcast.1, main='Max Liklihood Forecast', ylab='Employee Requirement', xlab='Months Passed')
fcast.1$model

# fit drift forecast
fit.2 <- Arima(deseasonal_cnt, order=c(0, 1, 1), include.drift = TRUE)
fcast.2 <- forecast(fit.2, 30)
plot(fcast.2, main='Forcast with Drift', ylab='Employee Requirement', xlab='Months Passed')
fcast.2$model

# test drift forecast vs actuals using weekly ARIMA
hold <- window(ts(deseasonal_cnt.week), start=340)
fit_no_holdout <- Arima(ts(deseasonal_cnt.week[-c(340:365)]), order=c(0,1,1), include.drift = TRUE)
fcast_no_holdout <- forecast(fit_no_holdout, h=25)
plot(fcast_no_holdout, main="Drift Prediction vs. Weekly ARIMA", xlab='Days Passed', ylab='Employee Requirement')
lines(ts(deseasonal_cnt.week))

#--------------------------------------------------------#

# regression analysis

# add days passed column 
series$Days <- seq.int(nrow(series))
setnames(series, old = c("Employee Requirement"), new = c("Employee.Requirement"))

# reg employee requirement on days passed
lm.fit <- lm(Employee.Requirement~Days, data = series)
summary(lm.fit)

# ggplot function to visualize regression
ggplotRegression <- function (fit) {
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5))) + ylab("Employee Requirement")+xlab("Days Passed")
}

# plotting regression in ggplot
ggplotRegression(lm.fit)

# build regression on monthly moving average

# create monthly data set
series.monthly <- series[,c("Date","monthly requirement")]
series.monthly <- na.omit(series.monthly)
series.monthly$Days <- seq.int(nrow(series.monthly))
setnames(series.monthly, old = c("monthly requirement"), new = c("ARIMA"))

# biuld bi-variate regression
lm.fit.2 <- lm(ARIMA~Days, data = series.monthly)
summary(lm.fit.2)
ggplotRegression(lm.fit.2)

# isolate predictions
lm.fit.2 <- lm(ARIMA~Days, data = series.monthly)
fit.values <- data.frame(lm.fit.2$fitted.values)

# create new dataframe with actuals and predictions
reg.data <- cbind(series.monthly, fit.values)
reg.data$`percent difference` <- (reg.data$lm.fit.2.fitted.values - reg.data$ARIMA) / reg.data$ARIMA

# adding quadratic term
lm.fit.3 <- lm(ARIMA~poly(Days, 2), data = series.monthly)
summary(lm.fit.3)
fit.poly <- data.frame(lm.fit.3$fitted.values)
poly.data <- cbind(series.monthly, fit.poly)

# plotting actuals vs predictions
ggplot(poly.data, aes(Days)) + 
  geom_point(aes(y = ARIMA, colour = "ARIMA")) + 
  geom_line(aes(y = fit.poly, colour = "fit.poly")) + ggtitle("Polynomial Predictions vs. Monthly ARIMA") + ylab("Employee Requirement") + xlab("Days Passed")

#--------------------------------------------------------#

# predict future employee requirement values using linear and quadratic models

# using models to predict future Employee Requirement
new.data <- data.frame(Days = c(400, 450, 500))

# predictions
predict(lm.fit, newdata = new.data)
predict(lm.fit.2, newdata = new.data)
predict(lm.fit.3, newdata = new.data) 

#--------------------------------------------------------#

# xgboost modeling

# using the forecast xgb package

# XGB Forecasting
xgb.monthly <- series.monthly[,c("ARIMA", "Days")]
ARIMA.xgb <- ts(series$Employee.Requirement)
xgb.fit <- xgbar(ARIMA.xgb)
xgb.forecast <- forecast(xgb.fit, h = 30)
plot(xgb.forecast, main = 'XGB Weekly Moving Average Forecast') 

#--------------------------------------------------------#

# xgb using train and test splits to build custom model

# set seed
set.seed(101)

# splitting training set into train and test with a 70/30% split

trainIndex <- createDataPartition(xgb.monthly$ARIMA,
                                  p = .7,
                                  list = FALSE,
                                  times = 1)

# setting train and test sets
xgb.Train <- xgb.monthly[trainIndex,]
xgb.Test <- xgb.monthly[-trainIndex,]

# training the model

# creating sparse matrix for learning
sparse_matrix_train <- sparse.model.matrix(ARIMA~.-1, data = xgb.Train)

# getting label (outcome), ERP solution dummy vector
xgb.Train$outputVector <- xgb.Train$ARIMA
output_train_vector <- xgb.Train[, "outputVector"]

# building model on training data
bst <- xgboost(data = sparse_matrix_train, label = output_train_vector, max.depth = 10, eta = 1, nthread = 2, nround = 5, 
               objective = "reg:linear")

# using model on test set to benchmark accuracy

# saving test label
test.Label <- xgb.Test$ARIMA

# transforming test to sparse
sparse_test_matrix <- sparse.model.matrix(ARIMA~.-1, data=xgb.Test)

# getting label (outcome), ERP solution dummy vector from test
xgb.Test$outputVector <- xgb.Test$ARIMA
outputTestVector <- xgb.Test[, "outputVector"]

# making prediction on test data
pred <- predict(bst, sparse_test_matrix)

# set prediction and probabilities as columns 
prediction <- data.frame(pred)

# add columns to test data
xgb.test.final <- cbind(xgb.Test, prediction)

# reorder columns
xgb.test.final <- xgb.test.final[c(2,1,3,4)]
xgb.test.final$outputVector <- NULL

# add analytical columns
xgb.test.final$`Squared diff` <- (xgb.test.final$ARIMA - xgb.test.final$pred)^2
xgb.test.final$`percent error` <- abs((xgb.test.final$pred - xgb.test.final$ARIMA) / xgb.test.final$ARIMA)

# plotting xgb actuals vs predictions
ggplot(xgb.test.final, aes(Days)) + 
  geom_point(aes(y = ARIMA, colour = "ARIMA")) + 
  geom_line(aes(y = pred, colour = "pred")) + ggtitle("XGB Predictions vs. Monthly ARIMA") + ylab("Employee Requirement") + xlab("Days Passed")

# predicting future Employee Requirement

# days into future
future.requirement <- data.frame(Days = c(351:450))

# sparse matrix conversion
sparse_matrix_pred <- sparse.model.matrix(~.-1, data=future.requirement)

# making prediction
pred_new_data <- predict(bst, sparse_matrix_pred)
new.predictions <- data.frame(pred_new_data)

# grab xgb fitted predictions
sparse_matrix_full <- sparse.model.matrix(ARIMA~.-1, data = xgb.monthly)
xgb.pred.full <- predict(bst, sparse_matrix_full)
xgb.predictions <- data.frame(xgb.pred.full)

# saved preds
xgb.data.export <- cbind(series.monthly, xgb.predictions)

#--------------------------------------------------------#

# custom step Function with three domains of time

# create subsets to train best fit regressions
piece.1 <- series.monthly[1:125,]
piece.2 <- series.monthly[126:260,]
piece.3 <- series.monthly[261:335,]

# build piecewise regressions
regression.1 <- lm(ARIMA~Days, data = piece.1)
regression.2 <- lm(ARIMA~Days, data = piece.2)
regression.3 <- lm(ARIMA~Days, data = piece.3)

# capture fitted values
piece.fit.1 <- data.frame(regression.1$fitted.values)
piece.fit.2 <- data.frame(regression.2$fitted.values)
piece.fit.3 <- data.frame(regression.3$fitted.values)

# cbind columns
PW.1 <- cbind(piece.1, piece.fit.1)
PW.2 <- cbind(piece.2, piece.fit.2)
PW.3 <- cbind(piece.3, piece.fit.3)

# set column names
setnames(PW.1, old = c("regression.1.fitted.values"), new = c("fit"))
setnames(PW.2, old = c("regression.2.fitted.values"), new = c("fit"))
setnames(PW.3, old = c("regression.3.fitted.values"), new = c("fit"))

# crease final piece wise function
piece.wise <- rbind(PW.1, PW.2, PW.3)


# plotting actuals vs predictions
ggplot(piece.wise, aes(Days)) + 
  geom_point(aes(y = ARIMA, colour = "ARIMA")) + 
  geom_line(aes(y = fit, colour = "fit")) + ggtitle("Step Function Predictions vs. Monthly ARIMA") + ylab("Employee Requirement") + xlab("Days Passed")

#--------------------------------------------------------#

# 2016 analysis consolidation

xgb.out <- xgb.data.export[, c("Date", "xgb.pred.full")]
setnames(xgb.out, old = c("xgb.pred.full"), new = c("xgb fit"))
step.out <- piece.wise[, c("Date", "fit")]
setnames(step.out, old = c("fit"), new = c("step fit"))
poly.out <- poly.data[, c("Date", "lm.fit.3.fitted.values")]
setnames(poly.out, old = c("lm.fit.3.fitted.values"), new = c("poly fit"))
reg.out <- reg.data[, c("Date", "lm.fit.2.fitted.values")]
setnames(reg.out, old = c("lm.fit.2.fitted.values"), new = c("lm fit"))
month.out <- series.monthly[, c("Date", "ARIMA")]

# cbind data together
merged <- Reduce(function(x, y) merge(x, y, all=TRUE), list(month.out, reg.out, poly.out, step.out, xgb.out))

# grab cols from weekday data
week.out <- series[, c("Date", "Time Entry", "Extrapolated Time Entry", "Employee.Requirement", "weekly requirement", "Days")]
setnames(week.out, old = c("Time Entry", "Extrapolated Time Entry", "Employee.Requirement", "weekly requirement", "Days"),
         new = c("Time Entered", "Extrapolated Time", "Requirement per Day", "Weekly ARIMA", "Days Passed"))

# merge weekdays and merged
output <- merge(week.out, merged, by = c("Date"), all.x = TRUE)

# plotting actuals vs predictions
ggplot(output, aes(`Days Passed`)) + 
  geom_point(aes(y = ARIMA, colour = "ARIMA")) + 
  geom_line(aes(y = `lm fit`, colour = "lm fit")) + 
  geom_line(aes(y = `poly fit`, colour = "poly fit")) + 
  geom_line(aes(y = `step fit`, colour = "step fit")) + 
  geom_line(aes(y = `xgb fit`, colour = "xgb fit")) + 
  ggtitle("Monthly ARIMA Vs Predictions") + ylab("Employee Requirement")

# set date column for tableau
output$Date <- gsub("-", "/", output$Date)
output$Date <- sprintf('%s 12:00:00 AM', output$Date)

#--------------------------------------------------------#

# data processing for predictions dataframe

# creating dates vector
prediction.dates <- data.frame(Date = c(seq(as.Date("2016-01-01", format="%Y-%m-%d"), as.Date("2017-3-25", format="%Y-%m-%d"),"days")))
prediction.dates$Date <- as.character(prediction.dates$Date)
prediction.dates$`Days Passed` <- 1:nrow(prediction.dates)

# data type structuring
prediction.dates$Date <- as.character(prediction.dates$Date)

# date formatting
prediction.dates$Date <- gsub("-", "/", prediction.dates$Date)
prediction.dates$Date <- sprintf('%s 12:00:00 AM', prediction.dates$Date)

#--------------------------------------------------------#

# building predictions data frame

# future predictions

# R forecast predictions

# ML model
fcast.1 <- forecast(fit.1, 100) 
fcast.1.preds <- data.frame(fcast.1)
fcast.1.preds$days <- 1:nrow(fcast.1.preds)
fcast.1.preds$`Days Passed` <- fcast.1.preds$days + 350
ML.preds <- fcast.1.preds[, c("Point.Forecast", "days", "Days Passed")]
setnames(ML.preds, old = c("Point.Forecast", "days"), new = c("Max Liklihood Forecast", "Prediction Days"))

# drift model
fcast.2 <- forecast(fit.2, 100)
fcast.2.preds <- data.frame(fcast.2)
fcast.2.preds$days <- 1:nrow(fcast.2.preds)
fcast.2.preds$`Days Passed` <- fcast.2.preds$days + 350
Drift.preds <- fcast.2.preds[, c("Point.Forecast", "Days Passed")]
setnames(Drift.preds, old = c("Point.Forecast"), new = c("Drift Forecast"))

# merge max liklihood and drift forecasts
R.Forecasts <- merge(ML.preds, Drift.preds, by = c("Days Passed"))

# order columns
R.Forecasts <- R.Forecasts[,c(1,3,2,4)]

# vector of days on which to apply algorithms
prediction.data <- data.frame(Days = c(336:435))

# use models to build predictions 

# lm 1
lm.1.preds <- data.frame(predict(lm.fit, newdata = prediction.data))
lm.1.preds$`Prediction Days` <- 1:nrow(lm.1.preds)
setnames(lm.1.preds, old = c("predict.lm.fit..newdata...prediction.data."), new = c("Linear Model 1 Forecast"))

# lm 2
lm.2.preds <- data.frame(predict(lm.fit.2, newdata = prediction.data))
lm.2.preds$`Prediction Days` <- 1:nrow(lm.2.preds)
setnames(lm.2.preds, old = c("predict.lm.fit.2..newdata...prediction.data."), new = c("Linear Model 2 Forecast"))

# lm 3
lm.3.preds <- data.frame(predict(lm.fit.3, newdata = prediction.data))
lm.3.preds$`Prediction Days` <- 1:nrow(lm.3.preds)
setnames(lm.3.preds, old = c("predict.lm.fit.3..newdata...prediction.data."), new = c("Quadratic Forecast"))

# XGB
new.predictions$`Prediction Days` <- 1:nrow(new.predictions)
setnames(new.predictions, old = c("pred_new_data"), new = c("XGB Model"))
xgb.preds <- new.predictions

# step function reg 1
step.1.preds <- data.frame(predict(regression.2, newdata = prediction.data))
step.1.preds$`Prediction Days` <- 1:nrow(step.1.preds)
setnames(step.1.preds, old = c("predict.regression.2..newdata...prediction.data."), new = c("Step Function 1 Forecast"))

# step function reg 2
step.2.preds <- data.frame(predict(regression.3, newdata = prediction.data))
step.2.preds$`Prediction Days` <- 1:nrow(step.2.preds)
setnames(step.2.preds, old = c("predict.regression.3..newdata...prediction.data."), new = c("Step Function 2 Forecast"))

# merging algorithm predictions

# cbind data together
merged.preds <- Reduce(function(x, y) merge(x, y, all=TRUE), list(lm.1.preds, lm.2.preds, lm.3.preds, xgb.preds, step.1.preds, step.2.preds))
merged.preds$`Averaged Step Forecast` <- (merged.preds$`Step Function 1 Forecast` + merged.preds$`Step Function 2 Forecast`) / 2

#--------------------------------------------------------#

# merging all forecasts
full.predictions <- merge(R.Forecasts, merged.preds, by = c("Prediction Days"))

# merging full cycle of predictions
merged.predictions <- merge(prediction.dates, full.predictions, by = c("Days Passed"), all.x = TRUE)
final.predictions <- merge(prediction.dates, full.predictions, by = c("Days Passed"))
final.predictions$`Short Date` <- gsub("12:00:00 AM", "", final.predictions$Date)
final.predictions$`Short Date` <- gsub("/", "-", final.predictions$`Short Date`)
final.predictions$`Short Date` <- as.Date(final.predictions$`Short Date`)

# graphing predictions
ggplot(final.predictions, aes(`Short Date`)) + 
  geom_line(aes(y = `Max Liklihood Forecast`, colour = "Max Liklihood Forecast")) + 
  geom_line(aes(y = `Drift Forecast`, colour = "Drift Forecast")) + 
  geom_line(aes(y = `Linear Model 1 Forecast`, colour = "Linear Model 1 Forecast")) + 
  geom_line(aes(y = `Linear Model 2 Forecast`, colour = "Linear Model 2 Forecast")) + 
  geom_line(aes(y = `Quadratic Forecast`, colour = "Quadratic Forecast")) + 
  geom_line(aes(y = `XGB Model`, colour = "XGB Model")) + 
  geom_line(aes(y = `Step Function 1 Forecast`, colour = "Step Function 1 Forecast")) + 
  geom_line(aes(y = `Step Function 2 Forecast`, colour = "Step Function 2 Forecast")) + 
  geom_line(aes(y = `Averaged Step Forecast`, colour = "Averaged Step Forecast")) + 
  ggtitle("Employee Requirement Predictions") + ylab("Employee Requirement") + xlab("2017 Date")

#--------------------------------------------------------#

# keep only output in global envionrment
rm(list=ls()[! ls() %in% c("output","final.predictions")])

#--------------------------------------------------------#

# exporting results

# LFQ Analysis
write.csv(output, "file_path//2016 Fit.csv", row.names = FALSE)

# Q3 Predictions
write.csv(final.predictions, "file_path//Predictions Fit.csv", row.names = FALSE)


# end of script
