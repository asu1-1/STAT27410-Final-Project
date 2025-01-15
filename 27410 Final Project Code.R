library(RCurl)
library(dplyr)
library(ggplot2)
#Stocks
apple = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/Apple%20Historical%20Stock%20Price.csv"))
cocacola = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/Coca%20Cola%20Historical%20Stock%20Price.csv"))
costco = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/Costco%20Historical%20Stock%20Price.csv"))
nvidia = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/Nvidia-Historical-Stock-Price.csv"))
#ETFS
qqq = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/QQQ.csv"))
xly = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/XLY_historical.csv"))
#S&P
spx = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/SPX.csv"))

#filtering out unneeded columns
apple = apple[, c(1,2,4)]
colnames(apple)[c(2,3)] = c("AAPL Close", "AAPL Open")

cocacola = cocacola[, c(1,2,4)]
colnames(cocacola)[c(2,3)] = c("KO Close", "KO Open")

costco = costco[,c(1,2,4)]
colnames(costco)[c(2,3)] = c("COST Close", "COST Open")

nvidia = nvidia[, c(1,2,4)]
colnames(nvidia)[c(2,3)] = c("NVDA Close", "NVDA Open")

qqq = qqq[, c(1,2,4)]
colnames(qqq)[c(2,3)] = c("QQQ Close", "QQQ Open")

xly = xly[, c(1,2,4)]
colnames(xly)[c(2,3)] = c("XLY Close", "XLY Open")

spx = spx[, c(1,2,3)]
colnames(spx)[c(2,3)] = c("SPX Close", "SPX Open")

#combining the files
datasets = list(apple, cocacola, costco, nvidia, qqq, xly, spx)
merged_data = Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE, sort = FALSE), datasets)


merged_data <- merged_data %>%
  mutate(across(c("AAPL Close", "AAPL Open", "KO Close", "KO Open", "COST Close", 
                  "COST Open", "NVDA Close", "NVDA Open"), ~ gsub("\\$", "", .))) %>%
  mutate(across(c("AAPL Close", "AAPL Open", "KO Close", "KO Open", "COST Close", 
                  "COST Open", "NVDA Close", "NVDA Open"), as.numeric))

head(merged_data)
merged_data = merged_data[c(9:761), ]
merged_data = merged_data[nrow(merged_data):1, ]


#Can do graphs now
n = length(merged_data$Date)

ggplot(merged_data, aes(x = 1:n, y = `AAPL Close`[1:n])) + geom_line() +
  xlab("Trading Day") + ylab("Value of AAPL at Close ($)")

ggplot(merged_data, aes(x = 1:n, y = `KO Close`[1:n])) + geom_line() +
  xlab("Trading Day") + ylab("Value of KO at Close ($)")

ggplot(merged_data, aes(x = 1:n, y = `COST Close`[1:n])) + geom_line() +
  xlab("Trading Day") + ylab("Value of COST at Close ($)")

ggplot(merged_data, aes(x = 1:n, y = `NVDA Close`[1:n])) + geom_line() +
  xlab("Trading Day") + ylab("Value of NVDA at Close ($)")

ggplot(merged_data, aes(x = 1:n, y = `QQQ Close`[1:n])) + geom_line() +
  xlab("Trading Day") + ylab("Value of QQQ at Close ($)")

ggplot(merged_data, aes(x = 1:n, y = `XLY Close`[1:n])) + geom_line() +
  xlab("Trading Day") + ylab("Value of XLY at Close ($)")

ggplot(merged_data, aes(x = 1:n, y = `SPX Close`[1:n])) + geom_line() +
  xlab("Trading Day") + ylab("Value of SPX at Close ($)")


##For covariance matrixes
closing_prices = merged_data[, c("AAPL Close", "KO Close", "COST Close", "NVDA Close", "QQQ Close", "XLY Close", "SPX Close")]

cor(closing_prices)

closing_prices_consumer = merged_data[, c("KO Close", "COST Close", "XLY Close", "SPX Close")]
cor(closing_prices_consumer)

closing_prices_technology = merged_data[, c("AAPL Close", "NVDA Close", "QQQ Close", "SPX Close")]
cor(closing_prices_technology)



#Autocorrelation stuff, will need to adjust the n here to just be over training data
aapl_autocor = lm(`AAPL Close`[1:n-1]~`AAPL Close`[2:n], data = merged_data)
ko_autocor = lm(`KO Close`[1:n-1]~`KO Close`[2:n], data = merged_data)
cost_autocor = lm(`COST Close`[1:n-1]~`COST Close`[2:n], data = merged_data)
nvda_autocor = lm(`NVDA Close`[1:n-1]~`NVDA Close`[2:n], data = merged_data)
qqq_autocor = lm(`QQQ Close`[1:n-1]~`QQQ Close`[2:n], data = merged_data)
xly_autocor = lm(`XLY Close`[1:n-1]~`XLY Close`[2:n], data = merged_data)
spx_autocor = lm(`SPX Close`[1:n-1]~`SPX Close`[2:n], data = merged_data)
acf(aapl_autocor$res, lag.max = 100)
acf(ko_autocor$res, lag.max = 100) 
acf(cost_autocor$res, lag.max = 100)
acf(nvda_autocor$res, lag.max = 100) #has high lag ~43 and ~15
acf(qqq_autocor$res, lag.max = 100)
acf(xly_autocor$res, lag.max = 100)
acf(spx_autocor$res, lag.max = 100)
