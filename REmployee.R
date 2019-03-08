Employee=read.csv(file="C:/Users/15DCS43/Desktop/Employee.csv")
Employee
summary(Employee)
head(Employee)
tail(Employee)
sum(Employee$Basic.Salary)
Total_Salary=sum(Employee$Basic.Salary)
Total_Salary
Empty=sum(Employee$Basic.Salary, na.rm=TRUE)
Empty
Mean_Salary=mean(Employee$Basic.Salary)
Mean_Salary
Median_Salary=median(Employee$Basic.Salary)
Median_Salary
Result1=subset(Employee, Location=="Kurla"&Basic.Salary>25000)
Result1
Employee[,c(2,3)]
Employee[c(1,3,5,7),]
Employee[c(1,3,5,7), c(2,5)]
Sort_Salary=Employee[order(Employee$Basic.Salary),]
Sort_Salary
Sort_Salary=Employee[order(-Employee$Basic.Salary),]
Sort_Salary
Sort_Loc=Employee[order(Employee$Location, decreasing = TRUE),]
Sort_Loc
Sort_Loc=Employee[order(Employee$Location, decreasing = FALSE),]
Sort_Loc
Sort_SalLoc=Employee[order(Employee$Location, decreasing = FALSE),c(2,3)]
Sort_SalLoc
Sort_SalLoc=Employee[order(Employee$Location, decreasing = TRUE),c(2,3)]
Sort_SalLoc
EmpBonus=read.csv(file="C:/Users/15DCS43/Desktop/EmpBonus.csv")
EmpBonus
Employee_Data=merge(Employee, EmpBonus, by="Emp_ID")
Employee_Data
Employee_Data_Left=merge(Employee, EmpBonus, by=c("Emp_ID"), all.x=TRUE)
Employee_Data_Left
Employee_Data_Right=merge(Employee, EmpBonus, by=c("Emp_ID"), all.y=TRUE)
Employee_Data_Right
Employee_Data_All=merge(Employee, EmpBonus, by=c("Emp_ID"), all=TRUE)
Employee_Data_All






"Test for normal Distribution"
Data1=read.csv(file.choose(),sep=",",header=T)
shapiro.test(Data1$C1)
help(shapiro.test)

"One Sample T Test"
Apple=read.csv(file.choose(),sep=",",header=T)
summary(Apple)
t.test(Apple$C1,alternative = "greater",mu=97)

"Paired T Test"
Data2=read.csv(file.choose(),sep=",",header = T)
t.test(Data2$Before_Fast,Data2$After_Fast,alternative = "greater", paired = T)

"T Test for Correlation"
Cor=read.csv(file.choose(),sep=",",header = T)
summary(Cor)
cor.test(Cor$aptitude,Cor$job_prof,alternative = "two.sided", paired = T)

"T Test for Correlation"
Data2=read.csv(file.choose(),sep=",",header = T)
summary(Data2)
cor.test(Data2$Before_Fast,Data2$After_Fast,alternative = "two.sided", paired = T)
help(cor.test)

iris
plot(iris)
summary(iris)
str(iris)
levels(iris$Species)
names(iris)
sum(is.na(iris))
iris_data=iris_data[1:100,]
samp=sample(1:100,80)
samp
iris
ir_test=iris[samp,]
ir_test
ir_ctrl=iris[-samp,]
ir_ctrl
Data2=read.csv(file.choose(),sep=",",header = T)
ind=sample(2,nrow(iris),replace=TRUE,prob=c(0.8,0.2))
ind
Training=iris[ind==1,]
Training

install.packages("ggplot2")
install.packages("GGally")

library(GGally)
ggpairs(ir_test)

install.packages("psych")
library(psych)
help("pairs.panels")

psych
pairs.panels(Training[1:4],
             gap=0,
             bg=c("red","yellow","blue")[Training$Species],
             pch=21)

pc=prcomp(Training[,-5],
          center=TRUE,
          scale=TRUE)
attributes(pc)
pc$scale
pc

pairs.panels(pc$x,
             bg=c("red","yellow","blue")[Training$Species],
             pch=21)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

G=ggbiplot(pc, obs.scale = 1,
           var.scale = 1,
           groups=Training$Species,
           ellipse=TRUE,
           circle=TRUE,
           ellipse.prob = 0.68)
G
