library(RCurl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggcorrplot)
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
marrangeGrob(grobs = plots, ncol = 2, nrow = 4)
grid_plot = grid.arrange(grobs = plots, ncol = 2)
ggsave(filename = "/Users/scarlett_hjq/Desktop/STAT27410/STAT27410-Final-Project/Closings.png", 
       plot = grid_plot, width = 12, height = 16)
grid.arrange(grobs = plots, ncol = 2)
ggsave(filename = "/Users/scarlett_hjq/Desktop/STAT27410/STAT27410-Final-Project/Closings.png")


##For covariance matrixes
cor_all = cor(merged_data[, all_stock_close])
cor_consumer = cor(merged_data[, c("KO Close", "COST Close", "XLY Close", "SPX Close")])
cor_technology = cor(merged_data[, c("AMD Close", "CRM Close","AAPL Close", "QQQ Close", "SPX Close")])

plot_correlation_matrix <- function(corr_matrix, title, filename) {
  # Create the heatmap
  p <- ggcorrplot(corr_matrix, 
                  lab = TRUE, 
                  lab_size = 3, 
                  colors = c("blue", "white", "red"), 
                  title = title, 
                  ggtheme = theme_minimal())
  
  # Save the plot
  ggsave(filename = filename, plot = p, width = 8, height = 6)
}

plot_correlation_matrix(cor_all, "Correlation Matrix: All Stocks", "all_stocks_correlation.png")
plot_correlation_matrix(cor_consumer, "Correlation Matrix: Consumer Stocks", "consumer_stocks_correlation.png")
plot_correlation_matrix(cor_technology, "Correlation Matrix: Technology Stocks", "technology_stocks_correlation.png")


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
