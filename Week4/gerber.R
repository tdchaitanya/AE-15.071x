gerber <- read.csv("gerber.csv")
#1.1
sum(gerber$voting)/nrow(gerber)
#1.2
sum(gerber$hawthorne[gerber$voting == 1])
sum(gerber$civicduty[gerber$voting == 1])
sum(gerber$neighbors[gerber$voting == 1])
sum(gerber$self[gerber$voting == 1])
#1.3
mod1 <- glm(voting ~ hawthorne + civicduty + neighbors + self , data = gerber , family = "binomial")
summary(mod1)
#1.4
pred = predict(mod1,type="response")
table(gerber$voting,pred >= 0.3)
(134513+51966)/(134513+51966+100875+56730)
#1.5
table(gerber$voting,pred >= 0.5)
235388/(108696+235388)
#1.6
library(ROCR)
ROCRpred = prediction(pred, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)
#2.1
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)
#2.2
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)
#2.4
CARTmodel3 = rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel3)
#3.1
treecontrol <- rpart(voting ~ control ,data = gerber,cp = 0.0)
prp(treecontrol,digits = 6)
abs(0.296638-0.34)
#3.2
treesex <- rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(treesex,digits = 6)
#3.3
LogModelSex <- glm(voting ~ sex + control,data = gerber,family = "binomial")
summary(LogModelSex)
#3.4
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")
abs(0.290456-0.2908065)
#3.5
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)
#3.6
predict(LogModel2, newdata=Possibilities, type="response")
abs(0.290456-0.290455)