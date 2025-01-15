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
ggplot(merged_data, aes(x = 1:n, y = merged_data$"SPX Open"[1:n])) + geom_line() +
  xlab("Trading Day") + ylab("Value of SPX at Open ($)")
