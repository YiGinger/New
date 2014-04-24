load(url("http://jaredlander.com/data/credit.rdata"))
require(rpart)
creditTree <- rpart(Credit~ CreditAmount + Age + CreditHistory + Employment,data=credit)
creditTree


install.packages("rpart.plot")
require(rpart.plot)
rpart.plot(creditTree, extra =4)
install.packages("randomForest")
require(randomForest)
require(useful)

creditX<- build.x(Credit~ CreditAmount + Age + CreditHistory + Employment,data=credit)
creditY<- build.y(Credit~ CreditAmount + Age + CreditHistory + Employment,data=credit)
head(credit)
creditForest <- randomForest(x=creditX,y=creditY)
creditForest

load(url("http://jaredlander.com/data/wifi.rdata"))
head(wifi)
ggplot(wifi,aes(x=x,y=y,color=Distance)+geom_point())+
  scale_color_gradient2(low="blue",mid="white",high="red",midpoint=mean(wifi$Distance))

geom_point(data=as.data.frame(t(coef(wifiModl))),aes(x=etax,y=betay),
           size=5,color="green")
)



data(diamonds)
diaSpline1 <- smooth.spline(x=diamonds$carat, y=diamonds$price)
diaSpline2 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df=2)
diaSpline3 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df=2)
diaSpline4 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df=2)
diaSpline5 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df=2)
diaSpline6 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df=2)
diaSpline1 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df=2)
diaSpline1 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df=2)
get.spline.info <- function(object){
  return (data.frame(x=object$x, y=object$y,df=object$df))
}



head(get.spline.info(diaSpline1))
head(get.spline.info(diaSpline2))

require(plyr)

splineDF <- ldply(diaSpline1, diaSpline2, diaSpline3,
                  diaSpline4, diaSpline5, diaSpline6)


g <- ggplot(diamonds, aes(x=carat, y =price))+ geom_point()



head(diaSpline1$x)
head(diaSpline1$y)

head(ns(diamonds$carat, df=1))



g+ stat_smooth(method="lm",formula=y ~ ns(x,6),color='blue')+stat_smooth(method="lm",formula=y ~ )

wifiModl <- nls(Distance ~ sqrt(betax-x)^2 + (betay-y)^2 ,data=wifi,start=list(betax=50,betay=50) )




