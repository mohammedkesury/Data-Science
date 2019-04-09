titanic<-read.csv(file.choose(), header = T, sep=",")
summary(titanic)
names(titanic)

install.packages("partykit")
install.packages("CHAID",repos="http://R-Forge.R-project.org",type="source")
library(CHAID) #Chi-Square automatic interaction detection
library(partykit) #Tool used for recursive partitioning
titanic$Survived<-as.factor(titanic$Survived)
titanic$SibSp<-as.factor(titanic$SibSp)
titanic$Parch<-as.factor(titanic$Parch)
titanic$Pclass<-as.factor(titanic$Pclass)
titanic$Sex<-as.factor(titanic$Sex)
titanic$Fare<-as.factor(titanic$Fare)
titanic$Embarked<-as.factor(titanic$Embarked)

tree<-chaid(formula=Survived~Pclass+Sex+SibSp+Parch+Embarked,data=titanic)

plot(tree, type="simple")

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')


library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=titanic, method="class")
plot(fit)
text(fit)


library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)
Prediction <- predict(fit, titanic, type="class")
Prediction