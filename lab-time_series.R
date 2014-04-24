gdp<-WDI(country=c("US","CA","CN","JP","SG","IL","GB"),indicator=c("NY.GDP.PCAP.CD","NY.GDP.MKTP.CD"),
         start=1960,end=2011)
names(gdp)<-c("iso2c","country","Year","PerCapGDP","GDP")
head(gdp)
us<-gdp$PerCapGDP[gdp$country=="United States"]
us<-ts(us,start=min(gdp$Year),end=max(gdp$Year))
acf(us)
install.packages("forecast")
require(forecast)
usBest<-auto.arima(us)
predict(usBest,n.ahead=5)
forecast(usBest,h=5)
plot(forecast(usBest,h=5))
install.packages("vars")
require(vars)
install.packages("reshape2")
require(reshape2)
gdpCast<-dcast(Year~country,data=gdp,value.var="PerCapGDP")
head(gdpCast)
gdpts<-ts(gdpCast[,-1],start=min(gdpCast$Year),end=max(gdpCast$Year))
plot(gdpts,type="single")
ndiffs(gdpts)
gdpDiffed<-diff(gdpts,differences=1)
gdpVar<-VAR(gdpDiffed,lag.max=12)
length(gdpVar$Varresult)
names(gdpVar$Varresult)
require(coefplot)
install.packages("coefplot")
install.packages("quantmod")
require(quantmod)
gdpDiffed<-diff()


att<-getSymbols("T",auto.assign=FALSE)
class(att)
head(att)
plot(att)
chartSeries(att)
addBBands()
addMACD(12,50,12)
attClose<-att$T.Close
install.packages("rugarch")
require(rugarch)
attSpec <- ugarchspec(variance.model=list(model="sGARCH",
                                          garchOrder=c(1,1)),
                      mean.model=list(armaOrder=c(1,1)),
                      distribution.model="std")
attGarch <- ugarchfit(spec=attSpec,data=attClose)
attGarch
attPred<-ugarchboot(attGarch,n.ahead=50,method="partial")
plot(attPred,which=2)



