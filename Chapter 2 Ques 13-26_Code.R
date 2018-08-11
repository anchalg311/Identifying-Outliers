
StockPrice<- c(10,7,20,12,75,15,9,18,4,12,8,14)
StockPrice
summary(StockPrice)

result.mean<-mean(StockPrice)
result.median<-median(StockPrice)
print(result.mean)
print(result.median)

#Writing function for calculating mode of vector
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode<-getmode(StockPrice)
mode

#Standard Deviation of Stock Price.
result.sd<-sd(StockPrice)
print(result.sd)

#Find min-max normalized stock price for $20
result.minmaxnorm<- (20-min(StockPrice))/((max(StockPrice)-min(StockPrice)))
print(result.minmaxnorm)

#Midrange of Stock Price
midrange<- (max(StockPrice)+ min(StockPrice))/2
print(midrange)

#Z-score standardized stock price for $20
zscorePrice<- (20-mean(StockPrice))/result.sd
print(zscorePrice)

#Calculate Skewness of Stock Price data
result.skewness<-(3*(result.mean-result.median))/result.sd
print(result.skewness)

#Calculate decimal scaling for stock price $20
decimalScaling<- 20/(10**2)
print(decimalScaling)

#Identify outlier
hist(StockPrice,main= "Identify Outlier",xlab= "Stock Price")

zscoreOutlier<- (75-mean(StockPrice))/result.sd
print(zscoreOutlier)

result.IQR<-15.75-8.75
print(result.IQR)

lowIQR<-8.75-1.5*result.IQR
print(lowIQR)
highIQR<-15.75+1.5*result.IQ
print(highIQR)


#Identify all outliers using z-score method
result.z.scores<-(StockPrice -mean(StockPrice))/result.sd
isOutlier<-ifelse(abs(result.z.scores)>3,"yes","no")
subset.isOutlier<-subset(StockPrice, isOutlier=="yes")
subset.isOutlier
length(subset.isOutlier)

#Identify all outliers using IQR method
outlier.IQR<-ifelse(StockPrice>highIQR|StockPrice<lowIQR, "yes","no")
subset.IQR<-subset(StockPrice,outlier.IQR=="yes")
subset.IQR
length(subset.IQR)

#Calculate mean and median without outlier
sum<-0
for(i in StockPrice){
  sum<-sum+i
}
sum
meanWithoutOutlier<-(sum-75)/11
print(meanWithoutOutlier)

subset.Stockprice<-c(10,7,20,12,15,9,18,4,12,8,14)
median(subset.Stockprice)





