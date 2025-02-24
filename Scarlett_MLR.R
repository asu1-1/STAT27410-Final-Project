library(RCurl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(forecast)
library(tseries)
library(zoo)
#Stocks
# apple = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/Apple%20Historical%20Stock%20Price.csv"))
# cocacola = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/Coca%20Cola%20Historical%20Stock%20Price.csv"))
# costco = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/Costco%20Historical%20Stock%20Price.csv"))
crm = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/CRM.csv"))
# amd = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/AMD.csv"))
#ETFS
qqq = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/QQQ.csv"))
# xly = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/XLY_historical.csv"))
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
crm = crm[c(1,2)]
# amd = filter_column(amd,"AMD")
# apple = filter_column(apple, "AAPL")
# cocacola = filter_column(cocacola, "KO")
# costco = filter_column(costco, "COST")
qqq = filter_column(qqq, "QQQ")
# xly = filter_column(xly, "XLY")
#so special spx! has no volumn
spx = spx[, c(1,2)]
colnames(spx)[c(2)] = c("SPX Close")

# combining the files
# datasets = list(amd, crm, apple, cocacola, costco, qqq, xly, spx)
# merged_data = Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE, sort = FALSE), datasets)
# merged_data = merged_data[c(9:761), ]
# merged_data = merged_data[nrow(merged_data):1, ]

# all_stock_close = c("CRM Close", "AMD Close", "AAPL Close", "KO Close", "COST Close","QQQ Close","SPX Close","XLY Close")
Tech_ETF_close = c("Date","QQQ Close","SPX Close")
Tech_ETF_data = Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE, sort = FALSE), list(qqq,spx))
Tech_ETF_data = Tech_ETF_data %>% 
  select(all_of(Tech_ETF_close))
to_combine = list(crm,Tech_ETF_data)
combined_df = na.omit(Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE, sort = FALSE), to_combine))
combined_df$Date <- as.Date(combined_df$Date, format = "%m/%d/%Y")
combined_df <- combined_df %>%
  arrange(Date) %>%  # Ensure data is sorted by date
  mutate(prev_crm = lag(`CRM Close`),
         prev_qqq = lag(`QQQ Close`),
         prev_spx = lag(`SPX Close`),
         crm_lr = log(`CRM Close` / prev_crm),
         qqq_lr = log(`QQQ Close` / prev_qqq),
         spx_lr = log(`SPX Close` / prev_spx)
         ) %>% 
  select(Date,crm_lr,qqq_lr,spx_lr,`CRM Close`)
combined_df = combined_df[2:nrow(combined_df),]

train_size = floor(2/3*nrow(combined_df))
crm_train = combined_df[1:train_size,]
crm_train$CRM_lag1 = lag(crm_train$crm_lr,1)
crm_train$CRM_lag2 = lag(crm_train$crm_lr,2)
crm_train$QQQ_lag1 = lag(crm_train$qqq_lr,1)
crm_train$SPX_lag1 = lag(crm_train$spx_lr,1)
names(crm_train) = gsub(" ", "_", names(crm_train))
model = lm(crm_lr ~ CRM_lag1 + CRM_lag2 + QQQ_lag1 + SPX_lag1, data = crm_train)


# close_columns = c("Date",all_stock_close)
# stock_data = merged_data[, close_columns]
# stock_data$Date = as.Date(stock_data$Date, format = "%m/%d/%Y")

# stock_train$AMD_lag1 = lag(stock_train$`AMD Close`,1)
# stock_train$AAPL_lag1 = lag(stock_train$`AAPL Close`,1)
# stock_train$KO_lag1 = lag(stock_train$`KO Close`,1)
# stock_train$COST_lag1 = lag(stock_train$`COST Close`,1)
# stock_train$XLY_lag1 = lag(stock_train$`XLY Close`,1)
# cov_matrix = cov(stock_train[, c('CRM_lag1', 'AMD_lag1', 'AAPL_lag1', 'KO_lag1', 'COST_lag1')],use="complete.obs") 
# stock_train$Cov_crm_amd = cov_matrix["CRM_lag1", "AMD_lag1"]
# stock_train$Cov_crm_aapl = cov_matrix["CRM_lag1", "AAPL_lag1"]
# stock_train$Cov_crm_ko = cov_matrix["CRM_lag1", "KO_lag1"]
# stock_train$Cov_crm_cost = cov_matrix["CRM_lag1", "COST_lag1"]
# stock_train$Cov_amd_aapl = cov_matrix["AMD_lag1", "AAPL_lag1"]
# stock_train$Cov_amd_ko = cov_matrix["AMD_lag1", "KO_lag1"]


steps = 80
predicted = numeric(steps)
curr = tail(crm_train,1)
for (i in 1:steps) {
  prediction = predict(model,curr)
  predicted[i] = prediction
  new_row = curr[1,]
  new_row$CRM_lag2 = new_row$CRM_lag1
  new_row$CRM_lag1 = new_row$crm_lr
  new_row$crm_lr = max(prediction,0)
  new_row$Date = as.Date(new_row$Date) + 1
  crm_train = rbind(crm_train, new_row)
  curr = new_row
}


actual = combined_df$crm_lr[train_size:(train_size+80)]
plot(actual, type = "l", col = "blue", ylim = c(min(c(actual, predicted),na.rm = TRUE), max(c(actual, predicted),na.rm = TRUE)), main = "Actual vs Predicted Log Return for CRM")
lines(predicted, col = "red")
legend("bottomleft", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1)

# turn log return to price
last_price <- crm_train[train_size,]$CRM_Close
last_date <- crm_train[train_size,]$Date
forecasted_prices <- numeric(length(predicted) + 1)
forecasted_prices[1] <- last_price
for (i in 2:length(forecasted_prices)) {
  forecasted_prices[i] <- forecasted_prices[i - 1] * exp(predicted[i - 1])
}
forecasted_prices <- forecasted_prices[-1]
forecast_dates <- seq(from = last_date + 1, by = "day", length.out = length(forecasted_prices))
forecast_df <- data.frame(
  Date = forecast_dates,
  Close = forecasted_prices
)

plot(combined_df$Date[1:(train_size+80)], combined_df$"CRM Close"[1:(train_size+80)], type = "l", col = "blue",
     xlab = "Date", ylab = "Close Price", main = "CRM price and predicted price with log return MLR model")
lines(forecast_df$Date, forecast_df$Close, col = "red")
legend("topleft", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1)

