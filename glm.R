setwd("/Users/apple/Documents")
sita <- 1
x <- seq(0,4,length.out=100)
y <- sita*exp(-sita*x)
plot(x,y)
xx <- c(1,2,4)
yy <- prod(sita*exp(-sita*xx))
points(xx,rep(yy,3))


mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
newdata1$rankP <- predict(fit, newdata = newdata1, type = "response")
newdata2 <- with(mydata, data.frame(
  gre = rep(seq(from = 200, to = 800, length.out = 100),4),
  gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))
newdata3 <- cbind(newdata2, predict(fit, newdata = newdata2, type = "link",
                                    se = TRUE))
?plogis
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

ggplot(newdata3, aes(x = gre, y = PredictedProb)) +
geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = 0.2) 
+
geom_line(aes(colour = rank),size = 1)
