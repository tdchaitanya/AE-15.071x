#Problem 1.1
FluTrain <- read.csv("FluTrain.csv")
FluTrain$Week[which.max(FluTrain$ILI)]
FluTrain$Week[which.max(FluTrain$Queries)]
#Problem 1.2
hist(FluTrain$ILI)
#Problem 1.3
plot(log(FluTrain$ILI),FluTrain$Queries)
#Problem 2.2
FluTrend1 <- lm(log(ILI) ~ Queries,data = FluTrain)
summary(FluTrend1)
#Problem 2.3
cor(FluTrain$Queries,log(FluTrain$ILI))
#Problem 3.1
FluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
a <- PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]
#Problem 3.2
(FluTest$ILI[11] - a )/FluTest$ILI[11]
#Problem 3.3
SSE = sum((FluTest$ILI - PredTest1)^2)
RMSE = sqrt(SSE/nrow(FluTest))

#Problem 4.1
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain$ILILag2)
#Problem 4.2
plot(log(FluTrain$ILILag2),log(FluTrain$ILI))
#Problem 4.3
FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2),data = FluTrain)
summary(FluTrend2)
#Problem 5.1
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest$ILILag2)
#Problem 5.3
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]
#Problem 5.4
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
sqrt(mean((PredTest2-FluTest$ILI)^2))
