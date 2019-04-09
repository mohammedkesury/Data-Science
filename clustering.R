#k-means clustering
data("iris")
names(iris)
new_data<-subset(iris,select=c(-Species))
new_data
cl<-kmeans(new_data,3)
cl

data<-new_data

wss<-sapply(1:15,
            function(k){kmeans(data,k)$tot.withinss})
wss

plot(1:15,wss,type="b",pch=19,frame=FALSE,xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

install.packages("cluster")
library(cluster)


#complete agglomerative clustering
cluster<-hclust(dist(iris[,3:4]))
plot(cluster)
clusterCut<-cutree(cluster,3)
table(clusterCut,iris$Species)
ggplot(iris,aes(Petal.Length,
                Petal.Width,
                color=iris$Species))+
  geom_point(alpha=0.4,size=3.5)+
  geom_point(col=clusterCut)+
  scale_color_manual(values=c('black','red','green'))

#avaerage agglomerative clustering
cluster<-hclust(dist(iris[,3:4]),method='average')
clusterCut1<-cutree(cluster,3)
cluster<-hclust(dist(iris[,3:4]))
clusterCut1<-cutree(cluster,3)
table(clusterCut1,iris$Species)
plot(cluster)
ggplot(iris,aes(Petal.Length,
                Petal.Width,
                color=iris$Species))+
  geom_point(alpha=0.4,size=3.5)+
  geom_point(col=clusterCut)+
  scale_color_manual(values=c('black','red','green'))