setwd("/Volumes//BOOTCAMP/Documents/New")
install.packages("fpc")
library(fpc)
library(fpc)
iris2 <- iris[,1:4]
pamk.result <- pamk(iris2)
# number of clusters
pamk.result$nc
# check clustering against actual species
table(pamk.result$pamobject$clustering, iris$Species)

layout(matrix(c(1,2),1,2)) # 2 graphs per page
plot(pamk.result$pamobject)
layout(matrix(1)) # change back to one graph per page
pam.result <- pam(iris2, 3)
table(pam.result$clustering, iris$Species)
layout(matrix(c(1,2),1,2)) # 2 graphs per page
plot(pam.result)
layout(matrix(1)) # change back to one graph per page


##hiera***
idx <- sample(1:dim(iris)[1], 40)
irisSample <- iris[idx,]
irisSample$Species <- NULL
hc <- hclust(dist(irisSample), method="ave")
plot(hc, hang = -1, labels=iris$Species[idx])
# cut tree into 3 clusters
rect.hclust(hc, k=3)
groups <- cutree(hc, k=3)


set.seed(3147)
x <- rnorm(10000)
summary(x)
# outliers
boxplot.stats(x)$out  
boxplot(x)

y <- rnorm(10000)
df <- data.frame(x, y)
rm(x, y)
head(df)

attach(df)
# find the index of outliers from x
(a <- which(x %in% boxplot.stats(x)$out))
# find the index of outliers from y
(b <- which(y %in% boxplot.stats(y)$out))
detach(df)
# outliers in both x and y
(outlier.list1 <- intersect(a,b))
plot(df)
points(df[outlier.list1,], col="red", pch="+", cex=2.5)

## LOF

library(DMwR)
# remove "Species", which is a categorical column
iris2 <- iris[,1:4]
par(mfrow=c(1,2))
outlier.scores <- lofactor(iris2, k=2)
## k is the parameter that indicates the number of neighbours that 
## will be used int he calculation of the local outlier factors
outlier.scores2 <- lofactor(iris2, k=20)

plot(density(outlier.scores2),sub="k = 20")
plot(density(outlier.scores),sub="k = 2")
# pick top 5 as outliers
outliers2 <- order(outlier.scores2, decreasing=T)[1:5]
outliers <- order(outlier.scores, decreasing=T)[1:5]
# who are outliers
print(outliers)
print(iris2[outliers,])
plot(iris2[,1],iris2[,2])
points(iris2[outliers,1],iris2[outliers,2],col="red",pch="+",cex=2)

n <- nrow(iris2)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(iris2), cex=.8, xlabs=labels)
## to biplot, the xlabs is different from the xlab in plot, 
## because the xlabs is the label for each observations, not for the x axis
## Here, xlabs is the index of observations, just like this picture which only shows the 
## indexs of those outliers

pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(iris2, pch=pch, col=col)



###-------detect by clustering 
# remove species from the data to cluster
iris2 <- iris[,1:4]
kmeans.result <- kmeans(iris2, centers=3)
# cluster centers
kmeans.result$centers

# cluster IDs
kmeans.result$cluster
# calculate distances between objects and cluster centers
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((iris2 - centers)^2))
# pick top 5 largest distances
outliers <- order(distances, decreasing=T)[1:5]
# who are outliers
print(outliers)
print(iris2[outliers,])

# plot clusters
plot(iris2[,c("Sepal.Length", "Sepal.Width")], pch="o", col=kmeans.result$cluster, cex=0.6)
# plot cluster centers
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=2)
# plot outliers
points(iris2[outliers, c("Sepal.Length", "Sepal.Width")], pch="+", col=4, cex=1.5)


###--------principal component analysis
### ???pca???????????????X??????????????????????????????????????????
require(graphics)

## The variances of the variables in the
## USArrests data vary by orders of magnitude, so scaling is appropriate
(pc.cr <- princomp(USArrests))  # inappropriate
princomp(USArrests, cor = TRUE) # =^= prcomp(USArrests, scale=TRUE)
## Similar, but different:
## The standard deviations differ by a factor of sqrt(49/50)

summary(pc.cr <- princomp(USArrests, cor = TRUE))
loadings(pc.cr)  ## note that blank entries are small but not zero
plot(pc.cr) # shows a screeplot.
biplot(pc.cr)

## Formula interface
princomp(~ ., data = USArrests, cor = TRUE)

## NA-handling
USArrests[1, 2] <- NA
pc.cr <- princomp(~ Murder + Assault + UrbanPop,
                  data = USArrests, na.action = na.exclude, cor = TRUE)
pc.cr$scores[1:5, ]

## (Simple) Robust PCA:
## Classical:
(pc.cl  <- princomp(stackloss))
## Robust:
(pc.rob <- princomp(stackloss, covmat = MASS::cov.rob(stackloss)))





x <- 1:3
y <- 3:8
x %in% y


