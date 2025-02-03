library(RCurl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(corrplot)
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

# Load necessary libraries
library(keras)
library(tensorflow)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reticulate)
library(lubridate)

# Ensure reticulate uses a valid Python environment
use_condaenv("env1", required = TRUE)

# Suppress interactive prompts
options(reticulate.ask = FALSE)

# Simulate stock data for 5 stocks
set.seed(42)

# Normalize the data
normalize = function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
normalized_data = stock_data %>%
  mutate(across(ends_with("Close"), normalize))

# Prepare data for LSTM (look-back period of 30 days)
prepare_lstm_data = function(data, look_back) {
  X = list()
  y = list()
  
  for (i in (look_back + 1):nrow(data)) {
    X[[i - look_back]] = as.matrix(data[(i - look_back):(i - 1), ])
    y[[i - look_back]] = as.numeric(data[i, ])
  }
  
  return(list(X = array(unlist(X), dim = c(length(X), look_back, ncol(data))),
              y = do.call(rbind, y)))
}

look_back = 30
data_prepared = prepare_lstm_data(normalized_data %>% select(-Date), look_back)

X = data_prepared$X
y = data_prepared$y

# Split into training (2 years) and testing (1 year)
train_size = floor(2 / 3 * dim(X)[1])
X_train = X[1:train_size,,]
y_train = y[1:train_size,]
X_test = X[(train_size + 1):dim(X)[1],,]
y_test = y[(train_size + 1):dim(y)[1],]

# Build the LSTM model
model = keras_model_sequential() %>%
  layer_lstm(units = 50, return_sequences = TRUE, 
             input_shape = c(look_back, dim(X_train)[3])) %>%
  layer_dropout(rate = 0.2) %>%
  layer_lstm(units = 50, return_sequences = FALSE) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = ncol(y_train))


model %>% compile(
  optimizer = 'adam',
  loss = 'mean_squared_error'
)

# Train the model
history = model %>% fit(
  X_train, y_train,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

# Make predictions on the test set
predictions = model %>% predict(X_test)

# Inverse transform predictions (if necessary)
inverse_normalize = function(x, original_min, original_max) {
  return(x * (original_max - original_min) + original_min)
}

test_actual_stock_price_data = stock_data[(train_size + 1):nrow(stock_data), -1]
min_vals = apply(stock_data[, -1], 2, min, na.rm = TRUE)  # Min values for each stock
max_vals = apply(stock_data[, -1], 2, max, na.rm = TRUE)  # Max values for each stock

predicted_prices_original = inverse_normalize(predictions, min_vals, max_vals)
true_prices_original = inverse_normalize(y_test, min_vals, max_vals)


plot(true_prices_original[, 1], type = "l", col = "blue", lwd = 2,
     main = paste("Stock", 1, "Predicted vs True"),
     xlab = "Time", ylab = "Price")
lines(predicted_prices_original[, 1], col = "red", lwd = 2)
legend("topright", legend = c("True Prices", "Predicted Prices"),
       col = c("blue", "red"), lwd = 2, cex = 0.8)


par(mfrow = c(2, 3))  # Set up a grid for multiple plots (2 rows x 3 columns)
for (i in 1:ncol(predicted_prices_original)) {
  plot(true_prices_original[, i], type = "l", col = "blue", lwd = 2,
       main = paste("Stock", i, "Predicted vs True"),
       xlab = "Time", ylab = "Price")
  lines(predicted_prices_original[, i], col = "red", lwd = 2)
  legend("topright", legend = c("True Prices", "Predicted Prices"),
         col = c("blue", "red"), lwd = 2, cex = 0.8)
}

# Generate portfolio weights based on predictions
generate_portfolio_weights = function(predictions) {
  row_sums = rowSums(predictions)
  weights = predictions / row_sums
  return(weights)
}

portfolio_weights = generate_portfolio_weights(predictions)


# create_blocks = function(stock_column, min_block_size = 30, threshold) {
#   n_days = length(stock_column)
# 
#   blocks = list()
#   start = 1  # Start from day 1
#   
#   while (start <= n_days) {
#     # Ensure we have at least a 30-day window (or another window size)
#     if (start + 29 <= n_days) {
#       # Calculate the variance from 'start' to 'start + 30'
#       current_block_variance = var(stock_column[start:start+29])
#       
#       # Handle missing values by replacing NAs with 0
#       if (is.na(current_block_variance)) {
#         current_block_variance = 0
#       }
#       
#       # If the variance exceeds the threshold
#       if (current_block_variance > threshold) {
#           start = start + 1
#           next
#       } else {
#         addition = 30
#         
#         while (current_block_variance <= threshold && start+addition < n_days) {
#           current_block_variance = var(stock_column[start:start+addition])
#           print(current_block_variance)
#           print(threshold)
#           print(start+addition)
#           print(n_days)
#           
#           # if (is.na(current_block_variance)) {
#           #   current_block_variance = 0  # Replace NA with 0
#           # }
#           
#           addition = addition + 1
#         }
#         blocks[[length(blocks) + 1]] = list(start = start, end = start + addition - 1, variances = current_block_variance)
#         start = start + addition
#         next
#       }
#     }
#     break
#   }
#   
#   return(blocks)
# }
# 
# blocks_list = apply(portfolio_weights, 2, function(column) {
#   create_blocks(column, 30, 0.2)
# })
# 
# calculate_block_weights = function(weights_matrix, blocks) {
#   adjusted_weights = matrix(0, nrow = nrow(weights_matrix), ncol = ncol(weights_matrix))
#   
#   for (block in blocks) {
#     start_day = block$start
#     end_day = block$end
#     
#     # Calculate average weights for the block
#     avg_weights = rowMeans(weights_matrix[start_day:end_day, 0.95])
#     
#     # Assign average weights to each day in the block
#     adjusted_weights[start_day:end_day, ] = cbind(avg_weights)
#   }
#   
#   return(adjusted_weights)
# }
# 
# 
# # Calculate adjusted block-based weights
# adjusted_weights = calculate_block_weights(predicted_weights, blocks)

adjust_by_rolling_avg = function(column, window_size = 30) {
  avg_values = rep(NA, length(column))  # Placeholder for adjusted values
  
  for (i in seq_along(column)) {
    # Define the rolling window
    start_index = max(1, i - (i - 1) %% window_size)  # Start of the 30-row group
    end_index = min(length(column), start_index + window_size - 1)  # End of the 30-row group
    
    # Compute the mean of the 30-row group
    avg_values[i] = mean(column[start_index:end_index])
  }
  
  return(avg_values)
}

portfolio_weights_adjusted = apply(portfolio_weights, 2, adjust_by_rolling_avg)

evaluate_portfolio_performance = function(weights, actual_prices) {
  # Ensure inputs are numeric matrices
  weights = as.matrix(weights)
  actual_prices = as.matrix(actual_prices)
  
  # Calculate returns (row-wise percentage change)
  returns = diff(actual_prices) / actual_prices[-nrow(actual_prices), ]
  
  # Calculate portfolio returns (weighted sum of individual stock returns)
  portfolio_returns = rowSums(weights[-nrow(weights), ] * returns)
  
  # Calculate cumulative returns
  cumulative_returns = cumprod(1 + portfolio_returns) - 1
  
  return(cumulative_returns)
}

actual_prices = normalized_data[(train_size + look_back + 1):nrow(normalized_data), -1]
cumulative_returns = evaluate_portfolio_performance(portfolio_weights_adjusted, actual_prices)


uniform_weights = matrix(1/ ncol(actual_prices), nrow = nrow(actual_prices), ncol = ncol(actual_prices))
cumulative_returns2 = evaluate_portfolio_performance(uniform_weights, actual_prices)

# cumulative_returns3 = evaluate_portfolio_performance(portfolio_weights, actual_prices)
# Plot cumulative returns
plot(cumulative_returns, type = "l", col = "blue", lwd = 2,
     main = "Cumulative Returns of Portfolio",
     xlab = "Time", ylab = "Cumulative Returns")
lines(cumulative_returns2, col = "red", lwd = 2)
legend("bottomright", legend = c("Uniform Weights", "Adjusted Weights"),
       col = c("red", "blue"), lwd = 2,cex = 0.6)

