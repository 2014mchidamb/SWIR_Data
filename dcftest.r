###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
require(TTR)
require(xts)
require(quantmod)
require(utils)

#con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
#    source(con)
require(RCurl)
sit = getURLContent('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', binary=TRUE, followlocation = TRUE, ssl.verifypeer = FALSE)
    con = gzcon(rawConnection(sit, 'rb'))
    source(con)
close(con)
#close(con)
 
    #*****************************************************************
    # Load historical fundamental and pricing data
    #****************************************************************** 
    #load.packages('quantmod') 
    tickers = spl('SWIR')
    tickers.temp = spl('NASDAQ:SWIR')
     
    # get fundamental data
    data.fund <- new.env()
    for(i in 1:len(tickers))
        data.fund[[tickers[i]]] = fund.data(tickers.temp[i], 10, 'annual')
             
    # get pricing data
    data <- new.env()
    getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
        for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)            
 
    # prepare data
    fund = data.fund[[tickers[1]]]
    fund.date = date.fund.data(fund)
    price = Cl(data[[tickers[1]]]['2011::'])
#*****************************************************************
# Extract Inputs for DCF Valuation
#******************************************************************                 
# Free Cash Flows
FCF = get.fund.data('free cash flow', fund, fund.date)
FCF[5] = 28
FCF[6] = -21
FCF[7] = 6
FCF[8] = 22
FCF[9] = 4
FCF[10] = 24
# Invested Capital
IC = get.fund.data('invested capital', fund, fund.date)
     
# Sales
SALE = get.fund.data('total revenue', fund, fund.date)
SALE[5] = 526
SALE[6] = 358
SALE[7] = 333.2
SALE[8] = 397.3
SALE[9] = 441.9
SALE[10] = 548.5
# Common Equity
CEQ = get.fund.data('total equity', fund, fund.date)
# Common Shares Outstanding
CSHO = get.fund.data('total common shares out', fund, fund.date)
 
# Growth Rate
CROIC = FCF/IC
# Average inputs
g = CROIC[7:10]#runMean(CROIC[7:10], 2)
cash = FCF[7:10]#runMean(FCF[7:10], 2)
#*****************************************************************
# Helper function to compute Intrinsic Value
#******************************************************************                 
compute.DCF.IV <- function(cash, eqity, shares, g, R) {
    if( cash <= 0 ) return(NA)
     
    if( len(R) == 1 ) R = rep(R, len(g))
     
    value = eqity + sum(cash * cumprod(1 + g) / cumprod(1 + R))
    return( value / shares )
}
#*****************************************************************
# Compute Intrinsic Value, assumptions:
# Company will grow for the first 3 years at current Growth Rate
# slowed down by 20% for the next 4 years, and slowed down by a further 20% for the next 3 years
# and finally 3% growth for the next 10 years
#
# The Discount Rate is 9%
#
# http://www.oldschoolvalue.com/blog/stock-analysis/apple-aapl-valuation/
#******************************************************************                 
dcf.price = NA * g
i.start = which(!is.na(g))[1] 
for(i in i.start : nrow(g)) {
    # Create Growth Rate scenario:      
    g.scenario = c(rep(g[i]+0.23,3), rep(g[i],4)*0.8, rep(g[i],3)*0.8*0.8, rep(3/100,10))
     
    # Compute Intrinsic Value
    dcf.price[i] = compute.DCF.IV(cash[i], CEQ[i], CSHO[i], g.scenario, 9/100)
}

#*****************************************************************
# Create Plots
#****************************************************************** 
png(filename="SWIRIntrinsic.png", width=720, height=540)
plota(price, type='l', log = 'y', col='blue', main=tickers[1],
    ylim=range(price,dcf.price,na.rm=T))
plota.lines(dcf.price, type='s', col='red', lwd=2)
plota.legend('Close,Intrinsic Value', 'blue,red', list(price, dcf.price))   
 
 
#plota(g, type='b', col='blue', pch=0, main='Growth Rate')   
 
 
#plota(cash, type='b', col='blue', pch=0, main='Free Cash Flows')
