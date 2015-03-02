require(TTR)
require(quantmod)

getSymbols('SWIR', from = '2012-01-01')
png(file = 'SWIRMACD.png', width=720, height=540)
chartSeries(
	SWIR,
	#theme = chartTheme("white"),
	TA = c(addMACD(12, 26, 9))
)
