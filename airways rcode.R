library(readxl)
EastWestAirlines <- read_excel("C:/Users/User/Downloads/EastWestAirlines.xlsx", 
                                 +     sheet = "data")
View(EastWestAirlines)
mydata <- EastWestAirlines[,c(2:12)]
normalizedata <- scale(mydata[,1:11]) #Normalize the datset mydata whose rows are1:11, but if 0&1 are there then use x-minx/maxx-minx

d <- dist(normalizedata,method = "euclidean")
fit <- hclust(d,method = "complete")
plot(fit,hang = -1)
#for visulization of clusters 
group <- cutree(fit,k=45)# general rule sqt(n/2)
rect.hclust(fit,k=45,border = "blue")

#assigning the cluster to main data frame
clustmember <- as.matrix(group)
final <- data.frame(mydata,clustmember)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
# to write into excel
write.table(final1,file ="airways.xls")


#Kmeans clustering
km <- kmeans(normalizedata,45)
str(km)

install.packages(animation) 
library(animation)
km1 <- kmeans.ani(normalizedata,45)

km$centers
km$cluster
wss <- (nrow(normalizedata)-1)*sum(apply(normalizedata,2,var))
for(i in 2:45) wss[i] <- sum(kmeans(normalizedata,centers = i)$withinss)
plot(1:45,type ="b", xlab = "no of cluster",ylab = " within group sum of squares")

#other methods for k selection
install.packages("kselection")
library(kselection)
k <- kselection(normalizedata[,],parallel=TRUE, k_treshold=0.85,max_centres=45)
k

