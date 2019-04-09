data <- read.csv(file.choose(), header=T, sep=",")
head(data)
summary(data)
str(data)

#creating the model
data$rank <- as.factor(data$rank)
str(data)
names(data)
model1 <- glm(admit ~ gre + gpa + rank, data = data, family = "binomial")
summary(model1)

#global testing for the acceptance of the model
null <- glm(admit~1, family = binomial, data=data)
anova(null, model1, test="Chisq")

#predicting the probabilities
data$predprob <- round(fitted(model1),2)
head(data)

#classification and misclassification analysis
install.packages("gmodels")
library(gmodels)
tab <- table(data$admit, fitted(model1)>0.5)
tab
#correct classification rate
sum(diag(tab))/sum(tab)
#misclassification rate
1-sum(diag(tab))/sum(tab)

#check the trade off between sensitivity and specificity using different cut values
table(data$admit, fitted(model1)>0.1)
table(data$admit, fitted(model1)>0.2)
table(data$admit, fitted(model1)>0.3)
table(data$admit, fitted(model1)>0.4)
table(data$admit, fitted(model1)>0.5)

#goodness of fit using receiver Operational Curve
pred <- predict(model1,data,type="response")
install.packages("ROCR")
library(ROCR)
data$predprob<-fitted(model1)

rocrpred<-prediction(pred, data$admit)
rocrperf<-performance(rocrpred,"tpr","fpr")

#to check proper cut off point
p<-plot(rocrperf,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

#to check coefficients
coef(model1)
exp(coef(model1))
