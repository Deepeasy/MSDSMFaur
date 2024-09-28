install.packages("fPortfolio")
library(fPortfolio)
library(quantmod)
sym_list=c("INFY.NS","SBIN.NS","TATASTEEL.NS","LT.NS","ACC.NS")
getSymbols(sym_list,from="2019-01-01",periodicity='monthly')
# if you don't specify to= then it will take upto latest data
# default periodicity is daily data
stock=na.omit(merge(Ad(INFY.NS),Ad(SBIN.NS),Ad(TATASTEEL.NS),
                    Ad(LT.NS),Ad(ACC.NS)))
stock=as.timeSeries(stock)  # converting into simple timeSeries object
?returns
ret=returns(stock)
plot(portfolioFrontier(ret))
mvp=minvariancePortfolio(ret,spec=portfolioSpec(),constraints = 'LongOnly')
mvp
tangencyPf=tangencyPortfolio(ret,spec = portfolioSpec(),constraints = 'LongOnly')
tangencyPf
