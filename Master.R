#install.packages("fPortfolio")
#install.packages("caTools")
install.packages("ROI")
install.packages("ROI.plugin.quadprog")
library(caTools)
library(fPortfolio)
library(quantmod)
library(dplyr)
library(PerformanceAnalytics)
library(xlsx)
library(PortfolioAnalytics)
library(ggplot2)
library(quadprog)
library(tseries)
library(ROI)
library(ROI.plugin.quadprog)

stock_symbols <- c("TATACONSUM.BO", "PATANJALI.BO", "UNITDSPR.BO", "UBL.BO", "RADICO.BO", "HAL.BO", "INDIGO.BO", "BEL.BO", 
                   "HDFCBANK.BO","ICICIBANK.BO","SBIN.BO","SIEMENS.BO","HAVELLS.BO","CGPOWER.BO","ASIANPAINT.BO","PIDILITIND.BO", 
                   "BERGEPAINT.BO","ULTRACEMCO.BO","SHREECEM.BO","AMBUJACEM.BO","HONAUT.BO","DIXON.BO","VOLTAS.BO", "AGI.BO",
                   "UFLEX.BO", "JINDALPOLY.BO","TITAN.BO","KALYANKJIL.BO","RAJESHEXPO.BO","ITC.BO","GRASIM.BO","SRF.BO",
                   "POLYCAB.BO","KEI.BO","BAJFINANCE.BO","BAJAJFINSV.BO","HINDUNILVR.BO","NESTLEIND.BO",
                   "VBL.BO","METROBRAND.BO","RELAXO.BO","SUNPHARMA.BO","DIVISLAB.BO","CIPLA.BO","INDHOTEL.BO",
                   "EIHOTEL.BO","WESTLIFE.BO","GAIL.BO","GUJGAS.BO","IGL.BO","LT.BO","ADANIPORTS.BO","GMRINFRA.BO",
                   "HDFCLIFE.BO","SBILIFE.BO","CONCOR.BO","BLUEDART.BO","GRINDWELL.BO","TITAGARH.BO", "SUNTV.BO",
                   "ZEEL.BO","PVRINOX.BO","COALINDIA.BO","JSWSTEEL.BO","TATASTEEL.BO","ADANIGREEN.BO","NAUKRI.BO",
                   "RELIANCE.BO","ONGC.BO","IOC.BO", "JKPAPER.BO","WSTCSTPAPR.BO","ANDHRAPAP.BO","SUPREMEIND.BO","ASTRAL.BO",
                   "FINOLEXIND.BO","NTPC.BO","POWERGRID.BO","ADANIENSOL.BO","DLF.BO","LODHA.BO","GODREJPROP.BO","DMART.BO",
                   "TRENT.BO", "MANYAVAR.BO", "GRSE.BO","MAZDOCK.BO","TCS.BO", "INFY.BO","HCLTECH.BO","BHARTIARTL.BO",
                   "IDEA.BO","INDUSTOWER.BO","PAGEIND.BO")

# Initialize an empty data frame to store stock prices
stock_prices <- data.frame(Date = character(), stringsAsFactors = FALSE)

# Define the date range
start_date <- "2022-01-01"
end_date <- "2023-12-31"

# Loop through each stock symbol
for (symbol in stock_symbols) {
  # Get stock data
  stock_data <- quantmod::getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  
  # Extract adjusted closing prices
  closing_prices <- Cl(stock_data)
  
  # Combine with existing data frame
  if (nrow(stock_prices) == 0) {
    stock_prices <- data.frame(Date = index(closing_prices), symbol = closing_prices, stringsAsFactors = FALSE)
  } else {
    stock_prices <- merge(stock_prices, data.frame(Date = index(closing_prices), symbol = closing_prices, stringsAsFactors = FALSE), by = "Date", all = TRUE)
    colnames(stock_prices)[which(names(stock_prices) == "symbol")] <- symbol
  }
}

#Importing the metal prices from Metal Master
commodity_table <- read.csv("clipboard", header=TRUE, sep="\t")

#Identifying the structure of commodity and stock prices to merge the same
str(stock_prices)
str(commodity_table)
# Convert Date column to Date type with the desired format
commodity_table$Date <- as.Date(commodity_table$Date, format = "%d-%b-%y")
# Convert Date column to the desired format "YYYY-MM-DD"
commodity_table$Date <- format(commodity_table$Date, "%Y-%m-%d")

# Convert the Date column back to Date type
commodity_table$Date <- as.Date(commodity_table$Date)
str(commodity_table)
str(stock_prices)

#Merging the data
# Merge commodity_table and stock_prices based on Date
Master_data <- merge(commodity_table, stock_prices, by = "Date", all = TRUE)

# Remove rows with NA values
Master_data <- na.omit(Master_data)

portfolio_ret1 <- na.omit(ROC(Master_data))


#Calculate Corelation Matrix

correlation_matrix <- cor(portfolio_ret1) 
correlation_matrix

correlation_df <- as.data.frame(correlation_matrix)

# Set the column names of the correlation data frame
colnames(correlation_df) <- paste0(colnames(correlation_df), "_Correlation")

# Write the correlation data frame to an Excel file
write.xlsx(correlation_df, file = "correlation_table.xlsx", sheetName = "Correlation", row.names = TRUE)

?cov
varcovar <- cov(portfolio_ret1)*222

varcovar_df <- as.data.frame(varcovar)

# Set the column names of the correlation data frame
colnames(varcovar_df) <- paste0(colnames(varcovar_df), "_variancecovariance")

# Write the correlation data frame to an Excel file
write.xlsx(varcovar_df, file = "Variance_covariance1.xlsx", sheetName = "Variance_Covariance", row.names = TRUE)


#Calculate Maximum Sharpe Ratio..pending
# Assuming you have your dataframe portfolio_ret1 with 102 columns of individual stock returns

# Assuming you have your dataframe portfolio_ret1 with 102 columns of individual stock returns

# Step 1: Calculate daily returns
daily_returns <- portfolio_ret1

# Step 2: Calculate mean and standard deviation of daily returns for each stock
mean_returns <- apply(daily_returns, 2, mean)*222 
View(mean_returns)


returns_df <- as.data.frame(mean_returns)

# Set the column names of the returns data frame
colnames(returns_df) <- paste0(colnames(mean_returns), "_mean_returns")

# Write the returns data frame to an Excel file
write.xlsx(returns_df, file = "mean_returns.xlsx", sheetName = "Mean_returns", row.names = TRUE)
std_dev_returns <- apply(daily_returns, 2, sd)*sqrt(222)

# Step 3: Define vector of weights for your portfolio
# Example: Equally weighted portfolio
weights <- rep(1/102, 102)

# Step 4: Calculate portfolio return and standard deviation
portfolio_return <- sum(weights * mean_returns)
portfolio_std_dev <- sqrt(t(weights) %*% cov(daily_returns) %*% weights)

# Step 5: Calculate Sharpe ratio
risk_free_rate <- 0.1  # You can adjust this to reflect the current risk-free rate
sharpe_ratio <- (portfolio_return - risk_free_rate) / portfolio_std_dev

# Step 6: Print Sharpe ratio
print(sharpe_ratio)

##Sharpe Ratio comes 19.6337..In excel we are getting 
#Caculate the efficienct Frontier
portfolio_ret2 <-as.timeSeries(portfolio_ret1)
effFrontier <- portfolioFrontier(portfolio_ret2, constraints = "Longonly")
View(effFrontier) 
plot(effFrontier,c(1,2,3,4))
#Make a plot selection (or 0 to exit): 
  
#  1:   Plot Efficient Frontier
#2:   Add Minimum Risk Portfolio
#3:   Add Tangency Portfolio
#4:   Add Risk/Return of Single Assets
#5:   Add Equal Weights Portfolio
#6:   Add Two Asset Frontiers [LongOnly Only]
#7:   Add Monte Carlo Portfolios
#8:   Add Sharpe Ratio [Markowitz PF Only]

frontierweights <-getWeights(effFrontier)
frontierweights
colnames(frontierweights) <-tickers
risk_return <- frontierPoints(effFrontier)

write.csv(risk_return,"risk_return.csv")


# Create a dataframe for plotting
stocks_data <- data.frame(
  Stock = colnames(portfolio_ret1),
  Mean_Return = mean_returns,
  Std_Dev_Return = std_dev_returns
)

# Plot risk and return
ggplot(stocks_data, aes(x = Std_Dev_Return, y = Mean_Return)) +
  geom_point() +
  geom_text(aes(label = Stock), vjust = -0.5, size = 1.5) +
  labs(title = "Risk and Return Plot of Portfolio Stocks",
       x = "Standard Deviation of Daily Returns",
       y = "Mean Daily Return") +
  theme_minimal()

# Export plot to JPG
ggsave("risk_return_plot.jpg", plot = last_plot(), width = 12, height = 9, dpi = 300)


# Load required library
library(PortfolioAnalytics)

# Define your assets
assets <- colnames(portfolio_ret1)

# Calculate mean returns and standard deviations
mean_returns <- apply(portfolio_ret1, 2, mean) * 222
std_dev_returns <- apply(portfolio_ret1, 2, sd) * sqrt(222)

# Define your correlation matrix
correlation_matrix <- cor(portfolio_ret1)

#Define function for portfolio optimization
optimizePortfolio <- function(portfolio_spec, optimize_method = "random", search_size = 2000) {
  # Define portfolio constraints and objectives
  portfolio <- portfolio.spec(assets = assets)
  portfolio <- add.constraint(portfolio = portfolio, type = "full_investment")
  portfolio <- add.constraint(portfolio = portfolio, type = "long_only")
  portfolio <- add.constraint(portfolio = portfolio, type = "risk", name = "StdDev", multiplier = 0)
  portfolio <- add.objective(portfolio = portfolio, type = "return", name = "mean")
  
  # Additional constraints or objectives specific to each optimization goal can be added here
  
  # Perform portfolio optimization
  optimized_port <- optimize.portfolio(R = portfolio_ret1, portfolio = portfolio, 
                                       optimize_method = optimize_method, search_size = search_size,
                                       maxSR = TRUE, trace = TRUE)
  return(optimized_port)
}

# Define portfolio assets and returns (funds and portfolio_ret should be defined)

# Optimize portfolio for maximizing Sharpe Ratio
maxSRport.rp <- optimizePortfolio(portfolio_spec = init.portf)

# Optimize portfolio for maximizing ROI
maxSRport.roi <- optimizePortfolio(portfolio_spec = init.portf2)

# Extract weights
maxSR.weight.rp <- extractWeights(maxSRport.rp)
maxSR.weight.roi <- extractWeights(maxSRport.roi)

# Calculate portfolio returns
maxSr.ret.rp <- Return.portfolio(portfolio_ret, weights = maxSR.weight.rp, 
                                 wealth.index = FALSE, contribution = FALSE, 
                                 geometric = TRUE, rebalance_on = c(NA), 
                                 value = 1, verbose = FALSE)

maxSr.ret.roi <- Return.portfolio(portfolio_ret, weights = maxSR.weight.roi, 
                                  wealth.index = FALSE, contribution = FALSE, 
                                  geometric = TRUE, rebalance_on = c(NA), 
                                  value = 1, verbose = FALSE)

# Combine results
ret.df <- na.omit(cbind(maxSr.ret.roi, maxSr.ret.rp))

# Calculate Sharpe Ratios
SR.roi <- table.AnnualizedReturns(maxSr.ret.roi, scale = NA, Rf = 0, geometric = TRUE, digits = 4)
SR.rp <- table.AnnualizedReturns(maxSr.ret.rp, scale = NA, Rf = 0, geometric = TRUE, digits = 4)
Sr.table <- cbind(SR.roi, SR.rp)

# Display weights sorted by magnitude
weights <- sort(maxSR.weight.rp, decreasing = TRUE)

# Plot portfolio performance over time
charts.PerformanceSummary(ret.df, plot.engine = "plotly", main = "Profit Loss over Time")