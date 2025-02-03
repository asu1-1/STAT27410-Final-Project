library(RCurl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(forecast)
library(tseries)
library(zoo)
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

all_stock_close = c("CRM Close", "AMD Close", "AAPL Close", "KO Close", "COST Close","QQQ Close","SPX Close","XLY Close")
Tech_ETF_close = c("QQQ Close","SPX Close")
Tech_ETF_data = merged_data[,Tech_ETF_close]
close_columns = c("Date",all_stock_close)
stock_data = merged_data[, close_columns]
stock_data$Date = as.Date(stock_data$Date, format = "%m/%d/%Y")

train_size = floor(2/3*nrow(stock_data))

stock_train = stock_data[1:train_size,]
stock_train$CRM_lag1 = lag(stock_train$`CRM Close`,1)
stock_train$CRM_lag2 = lag(stock_train$`CRM Close`,2)
stock_train$AMD_lag1 = lag(stock_train$`AMD Close`,1)
stock_train$AAPL_lag1 = lag(stock_train$`AAPL Close`,1)
stock_train$KO_lag1 = lag(stock_train$`KO Close`,1)
stock_train$COST_lag1 = lag(stock_train$`COST Close`,1)
stock_train$QQQ_lag1 = lag(stock_train$`QQQ Close`,1)
stock_train$SPX_lag1 = lag(stock_train$`SPX Close`,1)
# stock_train$XLY_lag1 = lag(stock_train$`XLY Close`,1)
# cov_matrix = cov(stock_train[, c('CRM_lag1', 'AMD_lag1', 'AAPL_lag1', 'KO_lag1', 'COST_lag1')],use="complete.obs") 
# stock_train$Cov_crm_amd = cov_matrix["CRM_lag1", "AMD_lag1"]
# stock_train$Cov_crm_aapl = cov_matrix["CRM_lag1", "AAPL_lag1"]
# stock_train$Cov_crm_ko = cov_matrix["CRM_lag1", "KO_lag1"]
# stock_train$Cov_crm_cost = cov_matrix["CRM_lag1", "COST_lag1"]
# stock_train$Cov_amd_aapl = cov_matrix["AMD_lag1", "AAPL_lag1"]
# stock_train$Cov_amd_ko = cov_matrix["AMD_lag1", "KO_lag1"]

names(stock_train) = gsub(" ", "_", names(stock_train))
# Fit the multiple linear regression model
model = lm(CRM_Close ~ CRM_lag1 + CRM_lag2 + QQQ_lag1 + SPX_lag1, data = stock_train)

steps = 80
predicted = numeric(steps)
curr = tail(stock_train,1)
for (i in 1:steps) {
  prediction = predict(model,curr)
  predicted[i] = prediction
  new_row = curr[1,]
  new_row$CRM_lag2 = new_row$CRM_lag1
  new_row$CRM_lag1 = new_row$CRM_Close
  new_row$CRM_Close = max(prediction,0)
  new_row$Date = as.Date(new_row$Date) + 1
  stock_train = rbind(stock_train, new_row)
  curr= new_row
}


actual = stock_data$'CRM Close'[train_size:(train_size+80)]

plot(actual, type = "l", col = "blue", ylim = c(min(c(actual, predicted),na.rm = TRUE), max(c(actual, predicted),na.rm = TRUE)), main = "Actual vs Predicted Prices for CRM")
lines(predicted, col = "red")
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1)


