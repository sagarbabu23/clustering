install.packages("readxl")
install.packages("xlsx")
library(readxl)
library(xlsx)
EastWestAirlines <- read.excel("D://assignments//cluster/eatwestairway",sheet = 1)
View(EastWestAirlines)
mydata <- EastWestAirlines[,c(2:12)]
normalizedata <- scale(mydata[,1:11]) #Normalize the datset mydata whose rows are1:11, but if 0&1 are there then use x-minx/maxx-minx

d <- dist(normalizedata,method = "euclidean")
fit <- hclust(d,method = "complete")
plot(fit,hang = -1)

d1 <- dist(normalizedata,method = "manhattan")
fit <- hclust(d1,method = "complete")
plot(fit,hang = -1)

d2 <- dist(normalizedata,method = "maximum")
fit <- hclust(d2,method = "complete")
plot(fit,hang = -1)


#for visulization of clusters 
group <- cutree(fit,k=45)# general rule sqt(n/2)
rect.hclust(fit,k=45,border = "blue")

#assigning the cluster to main data frame
clustmember <- as.matrix(group)
final <- data.frame(mydata,clustmember)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
# to write into excel
write.table(final1,file ="eatwestairway.xls")


#Kmeans clustering
km <- kmeans(normalizedata,45)
str(km)

install.packages("animation") 
library(animation)
km1 <- kmeans.ani(normalizedata,45)

km$centers
km$cluster
wss <- (nrow(normalizedata)-1)*sum(apply(normalizedata,2,var))
for(i in 2:45) wss[i] <- sum(kmeans(normalizedata,centers = i)$withinss)
plot(1:45,wss,type ="b", xlab = "no of cluster",ylab = " within group sum of squares")

#other methods for k selection
install.packages("kselection")
library(kselection)
k <- kselection(normalizedata[,],fun_cluster = stats:: kmeans, k_threshold=0.85,max_centers = 45)
k


#based on scree plot no 17 gives hetreogeneous cluster and withness also decreased so using kmeans cluster code
#for Kselection its is given 9 as it makes simplicity(interpretations)
#we can use any of the numbers its subjective and the as clustering is used for minimizing u can use any.