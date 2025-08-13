
production<-read.csv(choose.files())

#Creating Time Series
production.ts<-ts(production$Output_tons, start = c(2019, 1), frequency = 12) 

plot.ts(production.ts, xlab = "Months since Jan 2019", ylab = "Output (tons)",
        main = "Monthly Production Output")

# Decompose the time series into its components 
production.dec<-decompose(production.ts)  
plot(production.dec)

# Moving Average models (L = 3, 6, 12)
ma_3  <- SMA(production.ts, n = 3)
ma_6  <- SMA(production.ts, n = 6)
ma_12 <- SMA(production.ts, n = 12)

#Ma-3
plot.ts(cbind(production.ts, ma_3),
        col = c("black", "red"),
        plot.type = "single",
        ylab = "Output(tons)",
        main = "Output and Moving Averages (L=3)")
legend("bottomright",
       legend = c("Original", "MA-3"),
       col = c("black", "red"),
       lty = 1)

#Ma-6
plot.ts(cbind(production.ts, ma_6),
        col = c("black", "red"),
        plot.type = "single",
        ylab = "Output(tons)",
        main = "Output and Moving Averages (L=6)")
legend("bottomright",
       legend = c("Original", "MA-6"),
       col = c("black", "red"),
       lty = 1)
#MA-12
plot.ts(cbind(production.ts, ma_12),
        col = c("black", "red"),
        plot.type = "single",
        ylab = "Output(tons)",
        main = "Output and Moving Averages (L=12)")
legend("bottomright",
       legend = c("Original", "MA-12"),
       col = c("black", "red"),
       lty = 1)

#Error metrics

ERRORS <- function(data,L){
  ma.data<-SMA(data,n=L)  #calculate moving average
  error<-NULL             #Creates an empty vector to store errors   
  for (i in 1:(length(data)-L)){  # Loop through the data to calculate the difference between actual and forecasted values
    error[i]<-data[i+L]-ma.data[i+L-1]
  } 
  error.p<-NULL 
  for (i in 1:(length(data)-L)){ 
    error.p[i]<-abs(error[i])/abs(data[i + L])
  }
  MSE<-mean(error^2)
  MAD<-mean(abs(error))
  MAPE<-mean(error.p)*100
  error.df <- data.frame(errors = c(MSE, MAD, MAPE), row.names = c("MSE", "MAD", "MAPE"))
  return(error.df)
}
# Run the function and display results
cat("MA-3 Error Metrics:\n")
print(ERRORS(production.ts, 3))

cat("\nMA-6 Error Metrics:\n")
print(ERRORS(production.ts, 6))

cat("\nMA-12 Error Metrics:\n")
print(ERRORS(production.ts, 12))


