#=========simple linear regression==================
# We make use of inbuilt dataset Orange and plot Orange$age and Orange$circumference.
library(datasets)
library(help='datasets')
View(Orange)
plot(Orange$age,Orange$circumference)
?lm

#We fit the model using lm command 
#We are using linear regression to find out correlation/linear relationship between the two variables.
lm_result<-lm(Orange$circumference~Orange$age)
lm_result
summary(lm_result)
attributes(lm_result)

#=========== multiple linear regression =========
index <- read.csv(file.choose(), header=T)
names(index)
pairs(~index+written+language+tech+gk, data=index) # ~ sign indicates dependency

#fit a model
model1<-lm(index~.,data=index) #index~. means there are no explanatory variables
model1
summary(model1)

index$pred<-fitted(model1)
head(index)
index$res<-residuals(model1)
head(index)

#to check multicolinearity
install.packages("car")
library(carData)
library(car)
vif(model1)

#to check heteroscedasticity
plot(index$pred, index$res, color="red")

#Perform Shapiro test to check normality of errors.
shapiro.test(index$res)

#Detecting heteroscedasticity of data using NCV test(non-constant variance score test)
library(car)
ncvTest(model1, ~written+language+tech+gk)

#Detecting autocorrelation using Durbin Watson Test d=2(1-r)
durbinWatsonTest(model1)

#influence plot
library(car)
influencePlot(model1)

#validation using Hold-Out Method
library("lattice")
library("ggplot2")
install.packages("caret")
library("caret")
index <- read.csv(file.choose(), sep=",", header=T)
summary(index)
data <- createDataPartition(index$empid, p=0.8, list = F)
head(data)
dim(data)
traindata<-index[data,]
testdata<-index[-data,]
dim(traindata)
dim(testdata)


#validation using k fold method
library(caret)
help(trainControl)
kfolds<-trainControl(method="cv", number=4)
modelkfold<-train(index~written+language+tech+gk, data=index, method="lm", trControl=kfolds)
modelkfold

#Model selection forward method
index<-read.csv(file.choose(), sep=",", header=T)
null<-lm(index~1, data=index)
full<-lm(index~., data=index)
names(index)
x = step(null, scope = list(lower=null, upper=full), direction="forward")
summary(x)

#Model selection with backward method
index<-read.csv(file.choose(), sep=",", header=T)
null<-lm(index~1, data=index)
full<-lm(index~., data=index)
names(index)
step(null, scope = list(lower=null, upper=full), direction="backward")