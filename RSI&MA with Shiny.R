library(quantmod)
library(shiny)
getSymbols("^NSEI", from="2020-01-01",to="2023-06-03") #to download data from yahoo directly and data is stored in NSEI file. Cl for closing prices; Op for open, Hi for high, Lo for lowest price and ClCl for daily close-to-close return, Ad for daily adjusted closing price
head(Ad(NSEI))
getSymbols("INFY.NS", from="2020-01-01",to="2023-06-03")

chartSeries(INFY.NS, theme='white', type=c('line'))
addSMA(n=20, col='blue')
addSMA(n=50, col='red')

INFY=Ad(INFY.NS)
rsi=RSI(INFY,n=14)

plot(rsi, type='l')

#Adding Price column
rsi$prices <-INFY




# Calculating SMAs
sma_20 <- SMA(INFY, n = 20)
sma_50 <- SMA(INFY, n = 50)

# Determine if SMA of 20 days is greater than SMA of 50 days
SMA_change <- ifelse(sma_20 > sma_50, 0, 1)

rsi$SMA <- SMA_change

rsi <- data.frame(Date = index(rsi), RSI = coredata(rsi))
rsi$Date <- as.Date(rsi$Date)
colnames(rsi)

colnames(rsi) <- c("Date", "rsi", "prices", "SMA")
#Adding NA column
rsi$rsi[is.na(rsi$rsi)] <- 55
rsi$SMA[is.na(rsi$SMA)] <- 0

rsi$trade <- ifelse(rsi$rsi < 40 & rsi$SMA == 0, 1,
                                 ifelse(rsi$rsi > 80 & rsi$SMA == 1, -1, 0))

initial_cash <- 1000000
initial_shares <- 0

# Adding 'cash', 'shares', and 'fund_value' columns
rsi$shares <- initial_shares
rsi$cash <- initial_cash

rsi$fund_value <- rsi$cash + rsi$shares * INFY

# Calculate fund value for each row
# Calculate fund value for each row
for (i in 2:nrow(rsi)) {
  if (rsi$trade[i - 1] == 1 && rsi$cash[i - 1] >= INFY[i]) {
    # Buying shares if trade signal is 1
    rsi$shares[i] <- rsi$shares[i - 1] + floor(rsi$cash[i - 1] / INFY[i])
    rsi$cash[i] <- rsi$cash[i - 1] - rsi$shares[i] * INFY[i]
  } else if (rsi$trade[i - 1] == -1 && rsi$shares[i - 1] > 0) {
    # Selling shares if trade signal is -1
    rsi$cash[i] <- rsi$cash[i - 1] + rsi$shares[i - 1] * INFY[i]
    rsi$shares[i] <- 0
  } else {
    # No trade, maintaining cash and shares
    rsi$cash[i] <- rsi$cash[i - 1]
    rsi$shares[i] <- rsi$shares[i - 1]
  }
  
  # Calculate fund value
  rsi$fund_value[i] <- rsi$cash[i] + rsi$shares[i] * INFY[i]
}
tail(rsi)


ui <- fluidPage(
  titlePanel("RSI Line Graph"),
  sidebarLayout(
    sidebarPanel(
      # Input: Date range
      dateRangeInput("dates", "Date range:",
                     start = "2020-01-01",
                     end = "2023-06-03")
    ),
    mainPanel(
      # Output: Line graph
      plotOutput("rsiPlot")
    )
  )
)

library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Share Price and Fund Value"),
  
  sidebarLayout(
    sidebarPanel(
      # Empty for now, can add inputs if needed
    ),
    
    mainPanel(
      plotOutput("share_price_plot"),
      plotOutput("fund_value_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Plot for Share Price
  output$share_price_plot <- renderPlot({
    ggplot(rsi, aes(x = index(rsi), y = prices)) +
      geom_line() +
      labs(x = "Date", y = "Share Price", title = "Share Price Over Time")
  })
  
  # Plot for Fund Value
  output$fund_value_plot <- renderPlot({
    ggplot(rsi, aes(x = index(rsi), y = fund_value)) +
      geom_line(color = "blue") +
      labs(x = "Date", y = "Fund Value", title = "Fund Value Over Time")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
