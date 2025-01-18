library(RCurl)
library(dplyr)
library(ggplot2)
library(gridExtra)
#Stocks
apple = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/Apple%20Historical%20Stock%20Price.csv"))
cocacola = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/Coca%20Cola%20Historical%20Stock%20Price.csv"))
costco = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/Costco%20Historical%20Stock%20Price.csv"))
crm = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/CRM.csv"))
amd = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/AMD.csv"))
#ETFS
qqq = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/QQQ.csv"))
xly = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/XLY_historical.csv"))
#S&P
spx = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/SPX.csv"))

#filtering out unneeded columns
filter_column = function(stock, stockName) {
  stock = stock[, c(1,2,4)]
  colnames(stock)[c(2,3)] = c(paste(stockName, "Close"), paste(stockName, "Open"))
  stock[, c(2, 3)] = lapply(stock[, c(2, 3)], function(x) as.numeric(gsub("\\$", "", x)))
  return (stock)
}
crm = filter_column(crm,"CRM")
amd = filter_column(amd,"AMD")
apple = filter_column(apple, "AAPL")
cocacola = filter_column(cocacola, "KO")
costco = filter_column(costco, "COST")
qqq = filter_column(qqq, "QQQ")
xly = filter_column(xly, "XLY")
#so special spx! has no volumn
spx = spx[, c(1,2,3)]
colnames(spx)[c(2,3)] = c("SPX Close", "SPX Open")

#combining the files
datasets = list(amd, crm, apple, cocacola, costco, qqq, xly, spx)
merged_data = Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE, sort = FALSE), datasets)
merged_data = merged_data[c(9:761), ]
merged_data = merged_data[nrow(merged_data):1, ]


#Can do graphs now
all_stock_close = c("CRM Close", "AMD Close", "AAPL Close", "KO Close", "COST Close", "QQQ Close", "XLY Close", "SPX Close")
plot = function(data, y) {
  ggplot(data, aes(x = 1:nrow(data), y = get(y))) + 
    geom_line() +
    xlab("Trading Days") + 
    ylab("Value") +
    ggtitle(paste("Stock Closing Value for", y, "($)"))
}
plots = lapply(all_stock_close, function(stock_col) {
  plot(merged_data, stock_col)
})
grid.arrange(grobs = plots, ncol = 2)



##For covariance matrixes
closing_prices = merged_data[, all_stock_close]
cor(closing_prices)

closing_prices_consumer = merged_data[, c("KO Close", "COST Close", "XLY Close", "SPX Close")]
cor(closing_prices_consumer)

closing_prices_technology = merged_data[, c("AMD Close", "CRM Close","AAPL Close", "QQQ Close", "SPX Close")]
cor(closing_prices_technology)


#Autocorrelation stuff, will need to adjust the n here to just be over training data
calculate_autocorr = function(data, stock_col, max_lag = 100) {
  n = nrow(data)
  autocor_model = lm(data[[stock_col]][1:(n-1)] ~ data[[stock_col]][2:n])
  acf(autocor_model$residuals, lag.max = max_lag, main = paste("ACF of Residuals for", stock_col))
}
lapply(all_stock_close, function(stock) {
  calculate_autocorr(merged_data, stock)
  readline("Press [Enter] to continue to the next plot...")
})

