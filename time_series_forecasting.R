#consider inbuilt data set AirPassengers
data("AirPassengers")

#to know the format of data set here
class(AirPassengers)

#to know start of time series
start(AirPassengers)

#to know the end of time series
end(AirPassengers)

#to know frequency of time series, 12 means its on a monthly basis
frequency(AirPassengers)

#to know summary of the dataset i.e. mean, median, etc.
summary(AirPassengers)

#plot the time series model
plot(AirPassengers)

#To plot a best fit line which can be used for regression
abline(reg=lm(AirPassengers~time(AirPassengers)))

#To print the cycle across years
cycle(AirPassengers)

#Aggregate cycles and show a year to year trend
plot(aggregate(AirPassengers,FUN=mean))

#Using a box plot we will try to get a sense for a possible seasonal effect
boxplot(AirPassengers~cycle(AirPassengers), xlab = "Month", ylab = "AirPassengers", main = "Average Passengers per Month", names = month.abb, col = "green")


#We now need to check whether population is stationary or non-stationary. For that we use acf(autocorrelation) function
acf(log(AirPassengers))

#We now regress on the difference of logs rather than log directly. 
acf(diff(log(AirPassengers)))

#Finally, we fit an ARIMA model to our time series and predict the future 10 years. In addition, we will try to fit a seasonal component in the ARIMA formulation.
(fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))

#We now visualize the prediction along with the training data.
pred <- predict(fit, n.ahead = 10*12)
ts.plot(AirPassengers, 2.718^pred$pred, log="y", lty=c(1,3))

#explanations for the ts.plot arguments provided:
#2.718^APforecast$pred: we are  undoing the log from the values.In order to do that, we need to find the log inverse of what we have got.
#i.e. log(forecast) = APforecast$pred
#hence, forecast = e ^ APforecast$pred
#e= 2.718 
#log = "y' is to plot on a logarithmic scale
#lty = c(1,3) will set the LineTYpe to 1 (for solid) for the original time series and 3 (for dotted) for the predicted time series.
