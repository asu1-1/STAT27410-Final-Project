crm = read.csv(text = getURL("https://raw.githubusercontent.com/asu1-1/STAT27410-Final-Project/refs/heads/main/CRM.csv"))
filter_column = function(stock, stockName) {
  stock = stock[, c(1,2,4)]
  colnames(stock)[c(2,3)] = c(paste(stockName, "Close"), paste(stockName, "Open"))
  stock[, c(2, 3)] = lapply(stock[, c(2, 3)], function(x) as.numeric(gsub("\\$", "", x)))
  return (stock)
}
crm = filter_column(crm,"CRM")
crm = crm[c(1,2)]
crm$Date = as.Date(crm$Date, format = "%m/%d/%Y")
crm <- crm[order(crm$Date), ]
crm <- crm %>%
  mutate(prev_crm = lag(`CRM Close`),
         crm_lr = log(`CRM Close` / prev_crm)
  ) %>% 
  select(Date,"CRM Close",crm_lr)
crm = crm[2:nrow(crm),]
#define threshold
mean_return = mean(crm$crm_lr)
std_return = sd(crm$crm_lr)
up_threshold = mean_return + std_return
down_threshold = mean_return - std_return
crm$state = ifelse(
  crm$crm_lr > up_threshold, "Up",
  ifelse(crm$crm_lr < down_threshold, "Down", "Stagnant")
)
#create transition
crm = crm %>% 
  mutate(prev_state = lag(state)) %>% 
  filter(!is.na(prev_state))
transition_counts = table(crm$prev_state, crm$state)
transition_matrix <- prop.table(transition_counts, margin = 1)
print(transition_matrix)
#prepare data for stan
state_mapping <- c("Down" = 1, "Stagnant" = 2, "Up" = 3)
stan_data <- list(
  K = length(state_mapping),
  N = nrow(crm),
  prev_state = as.integer(factor(crm$prev_state, levels = names(state_mapping))),
  next_state = as.integer(factor(crm$state, levels = names(state_mapping)))
)
#fit stan
library(rstan)
fit <- stan(
  file = "markov_chain.stan",
  data = stan_data,
  iter = 2000,
  chains = 4,
  seed = 123
)
