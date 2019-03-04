install.packages("xlxs")
library(xlxs)
 library(readxl)
 University <- read_excel("C:/Users/User/Downloads/University.xlsx")
 View(University)
mydata <- University[1:25,c(2:7)]
View(mydata)
d <- dist(mydata,method = "euclidean")
plot(d)
fit <- hclust(d,method = "complete")
plot(fit,hang = -1)
plot(fit)

d1<- dist(mydata,method = "manhattan")
plot(d1)
fit <- hclust(d1,method = "complete")
plot(fit,hang = -1)
plot(fit)

d2<- dist(mydata,method = "canberra")
plot(d2)
fit <- hclust(d2,method = "complete")
plot(fit,hang = -1)
plot(fit)

s
group <- cutree(fit,k=5)
?cut.dendrogram
rect.hclust(fit,k=5,border =" Blue")
?rect.hclust     
?as.matrix
membership <- as.matrix(group)
final <- data.frame(mydata,membership)
View(final)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)
write.csv(final1,file="final university")
 setwd("C:\\Users\\User\\desktop\\r workings")

