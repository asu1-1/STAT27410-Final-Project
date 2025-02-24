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

all_stock_close = c("CRM Close", "AMD Close", "AAPL Close", "KO Close", "COST Close")

close_columns = c("Date",all_stock_close)
stock_data = merged_data[, close_columns]
stock_data$Date = as.Date(stock_data$Date, format = "%m/%d/%Y")

train_size = floor(2/3*nrow(stock_data))



ts_stock = function(stock,idx) {
  after = stock %>% 
    select(1,idx) %>% 
    slice(1:train_size)
  start_year = as.numeric(format(min(stock$Date), "%Y"))
  start_day = as.numeric(format(min(stock$Date), "%d"))
  price_zoo = zoo(after[,2], order.by = after$Date)
  # Convert to ts object (assuming 252 trading days per year)
  price_ts = ts(price_zoo, start = c(start_year, start_day), frequency = 252)
}

crm = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/CRM.csv"))
crm = filter_column(crm,"CRM")
crm$Date <- as.Date(crm$Date, format = "%m/%d/%Y")
crm <- crm %>%
  arrange(Date) %>%  # Ensure data is sorted by date
  mutate(Previous_Close = lag(`CRM Close`),
         LR = log(`CRM Close` / Previous_Close)) %>% 
  select(Date,LR,'CRM Close')
crm = crm[2:nrow(crm),]
train_idx = nrow(crm)*2/3
crm_train = crm[1:train_idx,]
crm_test = crm[train_idx+1:nrow(crm),]
lr_ts <- ts(crm_train$LR, start = c(2020, as.numeric(format(min(crm$Date), "%j"))), frequency = 252)
plot(lr_ts, main = "Time Series of log return", xlab = "Time", ylab = "LR", col = "blue", type = "l")
adf.test(lr_ts, alternative = "stationary")
par(mfrow = c(1, 2))
acf(lr_ts)
pacf(lr_ts)

crm_106 <- arima(lr_ts,order = c(1,0,6))
summary(crm_106)
tsdiag(crm_106)
crm_601 = arima(lr_ts, order = c(6,0,1))
summary(crm_601)
tsdiag(crm_601)

forecast_values = forecast(crm_601,h=252)
log_returns <- forecast_values$mean
last_price <- tail(crm_train$'CRM Close', 1)
last_date <- as.Date(tail(crm_train$Date, 1))
forecasted_prices <- numeric(length(log_returns) + 1)
forecasted_prices[1] <- last_price
for (i in 2:length(forecasted_prices)) {
  forecasted_prices[i] <- forecasted_prices[i - 1] * exp(log_returns[i - 1])
  
}
forecasted_prices <- forecasted_prices[-1]
forecast_dates <- seq(from = last_date + 1, by = "day", length.out = length(forecasted_prices))
forecast_df <- data.frame(
  Date = forecast_dates,
  Close = forecasted_prices
)
crm_train_to_combine = crm_train %>% 
  select(Date,'CRM Close')
colnames(crm_train_to_combine)[which(names(crm_train_to_combine) == "CRM Close")] <- "Close"
colnames(crm)[which(names(crm) == "CRM Close")] <- "Close"

crm_combined <- rbind(crm_train_to_combine, forecast_df)
plot(crm_train_to_combine$Date, crm_train_to_combine$Close, type = "l", col = "blue",
     xlab = "Date", ylab = "Close Price", main = "CRM price and predicted price with log return ARIMA model",
     xlim = range(crm_combined$Date))
lines(forecast_df$Date, forecast_df$Close, col = "red")


# price_diff_1 = diff(CRM_ts)
# plot(price_diff_1, main = "First Difference of Stock Prices", ylab = "Differenced Prices")
# CRM_model_1 = arima(CRM_ts,order = c(0,1,1))
# CRM_model_2 = arima(CRM_ts,order = c(1,1,1))
# CRM_model_3 = arima(CRM_ts,order = c(2,1,1))
# CRM_model_4 = arima(CRM_ts,order = c(0,2,1))
# summary(CRM_model_3)
# forecast_values = forecast(CRM_model_2,h=252)
# autoplot(forecast_values) + 
#   ggtitle("ARIMA(2,1,1) Forecast for Next Year for CRM") +
#   xlab("Time") +
#   ylab("Stock Price") +
#   theme_minimal() 
# 
# 
# AMD_ts = ts_stock(stock_data,3)
# plot(AMD_ts)
# auto.arima(AMD_ts)
# price_diff_1 = diff(AMD_ts)
# plot(price_diff_1, main = "First Difference of Stock Prices", ylab = "Differenced Prices")
# AMD_model_1 = arima(AMD_ts,order = c(0,1,1))
# AMD_model_2 = arima(AMD_ts,order = c(1,1,0))
# AMD_model_3 = arima(AMD_ts,order = c(1,1,1))
# summary(AMD_model_1)
# summary(AMD_model_2)
# summary(AMD_model_3)
# forecast_values = forecast(AMD_model_2,h=252)
# autoplot(forecast_values) + 
#   ggtitle("ARIMA(0,1,1) Forecast for Next Year") +
#   xlab("Time") +
#   ylab("Stock Price") +
#   theme_minimal() 
# 
# 
# 
# 
# 
# library(xts)
# library(PerformanceAnalytics)
# library(PortfolioAnalytics)
# library(ROI)
# library(ROI.plugin.quadprog)
# library(ROI.plugin.glpk)
# 
# 
# closing_price = stock_data[,-1]
# closing_prices_xts = xts(closing_price,order.by = stock_data$Date)
# 
# daily_returns = na.omit(Return.calculate(closing_prices_xts))
# 
# portfolio_spec = portfolio.spec(assets = colnames(daily_returns))
# portfolio_spec = add.constraint(portfolio = portfolio_spec, type = "full_investment")
# portfolio_spec = add.constraint(portfolio = portfolio_spec, type = "long_only")
# 
# portfolio_spec = add.objective(portfolio = portfolio_spec, type = "return", name = "mean")
# portfolio_spec = add.objective(portfolio = portfolio_spec, type = "risk", name = "StdDev")
# 
# optimized_portfolio = optimize.portfolio(
#   R = daily_returns,
#   portfolio = portfolio_spec,
#   optimize_method = "ROI",
#   maxSR = TRUE,
#   trace = TRUE
# )
# 
# print(optimized_portfolio)
# 
# # Step 5: Plot risk-return tradeoff chart
# chart.RiskReward(optimized_portfolio, risk.col = "StdDev", return.col = "mean")
