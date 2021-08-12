heart.data=read.csv("F:/00/heart.csv")
str(heart.data)
names(heart.data)
nrow(heart.data)
ncol(heart.data)
head(heart.data)


names(heart.data) <- c("Gender", "Age", "Thal","CP","Slope","BP","Cholestral")
heart.data$Gender <- factor(heart.data$Gender)
#levels(heart.data$Gender )<- c("Female", "Male")
heart.data$Thal <- factor(heart.data$Thal)
#levels(heart.data$Thal) <- c("normal","fixed","reversable")
heart.data$CP <- factor(heart.data$CP)
levels(heart.data$CP) <- c("1","2","3","4")
heart.data$Slope<- factor(heart.data$Slope)
#levels(heart.data$Slope) <- c("upsloping","flat","downsloping")
heart.data$BP <- factor(heart.data$BP)
#levels(heart.data$BP) <- c("80","120",">120")
heart.data$Cholestral<- factor(heart.data$Cholestral)
#levels(heart.data$Cholestral) <- c("normal","high","low")

summary(heart.data)


#Gender Chart
a=table(heart.data$Gender)
barplot(a,main="Using BarPlot to display Gender Comparision",
       ylab="Count",
       xlab="Gender",
       col=rainbow(2),
       legend=rownames(a))

# CP
a=table(heart.data$CP)
barplot(a,main="Chest Pain By Category",
       ylab="Count",
       xlab="Gender",
       col=rainbow(2),
       legend=rownames(a))

#k-MEans
library(purrr)
set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(heart.data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}
k.values <- 1:10
iss_values <- map_dbl(k.values, iss)
plot(k.values, iss_values,
    type="b", pch = 19, frame = FALSE, 
    xlab="Number of clusters K",
    ylab="Total ")

#cluster the data
k6<-kmeans(heart.data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6

pcclust=prcomp(heart.data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)


kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}
digCluster<-k6$cluster; dignm<-as.character(digCluster); # K-means clusters
plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))






