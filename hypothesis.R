bottles <- read.csv(file.choose(), sep=",", header=T)
summary(bottles)

#one-sample t test (two-tailed)
t.test(bottles$Volume, mu=500)

#one sample t test (one-tailed)
t.test(bottles$Volume, alternative = "less", mu=500)

#paired t test
data1 <- read.csv(file.choose(), sep=",", header=T)
summary(data1)
t.test(data1$pp, data1$Fasting, alternative = "greater", paired = T)

#t test for variance
var<-read.csv(file.choose(),sep="," , header = T) 
summary(var) 
var.test(var$Fasting,var$pp,alternative = "two.sided") 

#t test for correlation
corr<-read.csv(file.choose(),sep="," , header = T)  
summary(corr) 
cor.test(corr$aptitude_test_score, corr$job_proficiency,alternative = 
           "two.sided",method= "pearson") 

#paired t test using employee dataset to check whether aptitude score improves
data2 <- read.csv(file.choose(), sep=",", header=T)
summary(data2)
t.test(data2$apti_test_score_2, data2$aptitude_test_score, alternative = "greater", paired = T)



