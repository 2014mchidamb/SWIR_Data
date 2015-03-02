require(gridExtra)

calcCAGR <- function(vals, num_years) {
	return ((vals[2]/vals[1])^(1/num_years)-1)
}

forecast <- function(start, g) {
	projected <- c(start)
	for (i in 1:length(g)) {
		cur <- start*((1+g[i])^i)
		projected <- c(projected, cur)
	}
	return (projected)
}

DCF <- function(fcf, wacc) {
	sum <- 0
	for (i in 1:length(fcf)) {
		sum <- sum+fcf[i]/((1+wacc)^i)
	}
	return(sum)
}

etax <- 0.15
EBIT <- c(1.4, 17)#1.4+5.9) #6 years
cwcap <- c(464.8-139.9-422.9+125.6, 512.0-127.3-464.8+139.9) #2 years
capex <- c(9.1, 11.4) #2 years
cflows <- c(23.7, 48.7) #4 years

ebitcagr <- calcCAGR(EBIT, 6)
cwcapcagr <- calcCAGR(cwcap, 2)
capexcagr <- calcCAGR(capex, 2)
cflowscagr <- calcCAGR(cflows, 4)

egs <- c(ebitcagr, ebitcagr, ebitcagr, 0.8*ebitcagr, 0.8*ebitcagr)
cpgs <- c(capexcagr, capexcagr, capexcagr, 0.8*capexcagr, 0.8*capexcagr)
cfgs <- c(cflowscagr, cflowscagr, cflowscagr, 0.8*cflowscagr, 0.8*cflowscagr)

f_cflows <- forecast(cflows[2], cfgs)
f_capex <- forecast(capex[1], cpgs)
FCF <- f_cflows-f_capex
WACC <- 0.1238
FCFcagr <- calcCAGR(c(FCF[1], FCF[6]), 6)
(DCF(FCF, WACC)+(FCF[6]*(1+(0.8^2)*FCFcagr))/(WACC-0.3*FCFcagr))/31.52

#mat = matrix(2014:2019, f_cflows, f_capex, FCF)
df <- data.frame(f_cflows, f_capex, FCF)
rownames(df) <- (2014:2019)
png(filename = "FCF.png")
grid.table(df)
