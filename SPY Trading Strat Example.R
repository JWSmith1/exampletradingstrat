library(quantmod)
library(PerformanceAnalytics)
library(TTR)
install.packages("remotes")
remotes::install_github("braverock/quantstrat")

#Set dates
initDate <- "1999-01-01"
fromDate <- "2003-01-01"
toDate <- "2015-12-31"

#Time Zone and Currency
Sys.setenv(TZ = "UTC")
currency("USD")

# ETF Data for SPY & stock() to initialize SPY and Currency
getSymbols("SPY", from = fromDate, to = toDate, src = "yahoo", adjust = TRUE)
stock("SPY", currency = "USD", multiplier = 1)

#Initialization setting for trade size, initial equity, portfolio, account, orders, and strategy
tradesize <- 100000
initeq <- 100000

strategy.st <- portfolio.st <- account.st <- "firststrat"
initPortf(portfolio.st, symbols = "SPY", initDate = initDate, currency = "USD")
initAcct(account.st, portfolios = portfolio.st, initDate = initDate, currency = "USD", initEq = initeq)
initOrders(portfolio.st, initDate = initDate)
strategy(strategy.st, store = TRUE)

#SPY Close Prices
plot(Cl(SPY))

# Adding 200 day SMA to SPY
spy_sma <- SMA(x = Cl(SPY), n = 200)

#RSI with 3 day lookback period
spy_rsi <- RSI(price = Cl(SPY), n = 3)

# Adding 200-day SMA indicator to strategy.st
add.indicator(strategy = strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n = 200), label = "SMA200")

# Adding 50-day SMA to strategy.st
add.indicator(strategy = strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n = 50), label = "SMA50")

# RSI 3 added to strategy.st
add.indicator(strategy = strategy.st, name = "RSI", arguments = list(x = quote(Cl(mktdata)), n = 3), label = "RSI_3")

# RSI average function 
calc_RSI_avg <- function(price, n1, n2) {
  
  # RSI 1 takes an input of the price and n1
  RSI_1 <- RSI(price = price, n = n1)
  
  # RSI 2 takes an input of the price and n2
  RSI_2 <- RSI(price = price, n = n2)
  
  # RSI_avg is the average of RSI_1 and RSI_2
  RSI_avg <- (RSI_1 + RSI_2)/2
  
  # Rename output of RSI_avg to column name of RSI_avg
  colnames(RSI_avg) <- "RSI_avg"
  return(RSI_avg)
}

# Adding RSI average function to strategy with n1 = 3 and n2 = 4
add.indicator(strategy.st, name = "RSI", arguments = list(price = quote(Cl(mktdata)), n1 = 3, n2 = 4), label = "RSI_3_4")

# Writing the David Varadi Oscillator(DVO) function
DVO <- function(HLC, navg = 2, percentlookback = 126) {
  
  # Computing the ratio between closing prices to the average of high and low
  ratio <- Cl(HLC)/((Hi(HLC) + Lo(HLC))/2)
  
  # Smoothing the ratio outputs using a moving average
  avgratio <- SMA(ratio, n = 2)
  
  # Converting ratio into a 0-100 value using runPercentRank()
  out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  colnames(out) <- "DVO"
  return(out)
}

# Adding the DVO indicator to strategy.st
add.indicator(strategy = strategy.st, name = "DVO", 
              arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 126),
              label = "DVO_2_126")

# Use applyIndicators to test out indicators
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))

# Subset data between Sep. 1 and Sep. 5 of 2013 for test
test_subset <- test["2013-09-01/2013-09-05"]

# Adding a sigComparison which specifies that SMA50 must be greater than SMA200, called longfilter
add.signal(strategy.st, name = "sigComparison", 
           arguments = list(columns = c("SMA50", "SMA200"), relationship = "gt"), label = "longfilter")

# Adding a sigCrossover which specifies that the SMA50 is less than the SMA200 labeled filterexit
add.signal(strategy.st, name = "sigCrossover",
           arguments = list(columns = c("SMA50", "SMA200"), relationship = "lt"),label = "filterexit")

# Adding a sigThreshold which specifies that DVO must be less than 20, labeled longthreshold
add.signal(strategy.st, name = "sigThreshold", 
           arguments = list(column = "DVO", 
                            threshold = 20, 
                            relationship = "lt", 
                            cross = FALSE), 
           label = "longthreshold")

# Adding a sigThreshold signal for strategy.st that specifies that DVO must cross above 80 and labeled thresholdexit
add.signal(strategy.st, name = "sigThreshold", 
           arguments = list(column = "DVO", 
                            threshold = 80, 
                            relationship = "gt", 
                            cross = TRUE), 
           label = "thresholdexit")


# Adding a sigFormula signal specifying that both longfilter and longthreshold must be TRUE, labeled longentry
add.signal(strategy.st, name = "sigFormula",
           arguments = list(formula = "longfilter & longthreshold", 
                            cross = TRUE),
           label = "longentry")

# Creating test set for indicators and signals 
test_init <- applyIndicators(strategy.st, mktdata = OHLC(SPY))
test <- applySignals(strategy = strategy.st, mktdata = test_init)

# Entry rule to buy 1 share when all conditions line up to enter into a position
add.rule(strategy.st, name = "ruleSignal", 
         arguments=list(sigcol = "longentry", 
                        sigval = TRUE, 
                        orderqty = 1,
                        ordertype = "market",
                        orderside = "long",
                        replace = FALSE, 
                        prefer = "Open"),
         type = "enter")

# Adding a rule that uses an osFUN to size an entry position
add.rule(strategy = strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "longentry", sigval = TRUE, ordertype = "market",
                          orderside = "long", replace = FALSE, prefer = "Open",
                          
                          # Use of the osFUN called osMaxDollar
                          osFUN = osMaxDollar,
                          tradeSize = tradesize,
                          maxSize = tradesize),
         type = "enter")

# Exit Rule
add.rule(strategy.st, name = "ruleSignal", 
         arguments = list(sigcol = "filterexit", sigval = TRUE, orderqty = "all", 
                          ordertype = "market", orderside = "long", 
                          replace = FALSE, prefer = "Open"), 
         type = "exit")

#Analyzing strategy 

# Use applyStrategy() to apply your strategy. Saved to out
out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)

# Update portfolio (portfolio.st)
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]

# Update account (account.st)
updateAcct(account.st, daterange)
updateEndEq(account.st)

# Get the tradeStats for portfolio
tstats <- tradeStats(Portfolios = portfolio.st)
tstats

# chart.Posn to view system's performance on SPY
chart.Posn(Portfolio = portfolio.st, Symbol = "SPY")

# Compute SMA50, SMA200 and DVo for addition to performance chart
sma50 <- SMA(x = Cl(SPY), n = 50)
sma200 <- SMA(x = Cl(SPY), n = 200)
dvo <- DVO(HLC = HLC(SPY), navg = 2, percentlookback = 126)

# Recreate chart.Posn of the strategy 
chart.Posn(Portfolio = portfolio.st, Symbol = "SPY")

# Overlay the SMA50 on plot as a blue line
add_TA(sma50, on = 1, col = "blue")

# Overlay the SMA200 on plot as a red line
add_TA(sma200, on = 1, col = "red")

# Add the DVO_2_126 to plot in a new window
add_TA(dvo)

#P & L series for strategy performance
portpl <- .blotter$portfolio.firststrat$summary$Net.Trading.PL

#Compute sharpe ratio of P&L series
SharpeRatio.annualized(portpl, geometric=FALSE)

# Get instrument returns
instrets <- PortfReturns(portfolio.st)

# Compute Sharpe ratio from returns
SharpeRatio.annualized(instrets, geometric = FALSE)