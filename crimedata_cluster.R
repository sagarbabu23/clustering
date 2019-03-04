install.packages("csv")
crimedata <- read.csv("D://assignments//cluster//crime_data.csv")
View(crimedata)
normalizedata <- scale(crimedata[,2:5]) #Normalize the datset mydata whose rows are1:11, but if 0&1 are there then use x-minx/maxx-minx

d <- dist(normalizedata,method = "euclidean")
fit <- hclust(d,method = "centroid")
plot(fit,hang = -1)

d1 <- dist(normalizedata,method = "manhattan")
fit <- hclust(d1,method = "complete")
plot(fit,hang = -1)

d2 <- dist(normalizedata,method = "maximum")
fit <- hclust(d2,method = "complete")
plot(fit,hang = -1)
#for visulization of clusters 
group <- cutree(fit,k=5)# general rule sqt(n/2)
rect.hclust(fit,k=5,border = "blue")

#assigning the cluster to main data frame
clustmember <- as.matrix(group)
final <- data.frame(crimedata,clustmember)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
# to write into excel
write.table(final1,file ="crimedata.xls")


#Kmeans clustering
km <- kmeans(normalizedata,5)
str(km)

install.packages("animation") 
library(animation)
km1 <- kmeans.ani(normalizedata,5)

km$centers
km$cluster
wss <- (nrow(normalizedata)-1)*sum(apply(normalizedata,2,var))
for(i in 2:5) wss[i] <- sum(kmeans(normalizedata,centers = i)$withinss)
plot(1:5,wss,type ="b", xlab = "no of cluster",ylab = " within group sum of squares")

#other methods for k selection
install.packages("kselection")
library(kselection)
k <- kselection(normalizedata[,],fun_cluster = stats:: kmeans, k_threshold=0.85,max_centers = 5)
k
# based on animation and wss mrthofdthe appropriate clusters are 3 and by ks election method its 2

