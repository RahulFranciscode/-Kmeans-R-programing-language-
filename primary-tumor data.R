library(factoextra)
water <- read.delim("helpda.txt", header=FALSE, sep = ",")

water <- read.delim("helpda.txt", header=FALSE, sep = ",",na.strings=c("?"," ","NA"))
View(water)

colnames(water) <- c(
  "class",
  "age",      
  "sex",     
  "his",    
  "degre",     
  "bone",    
  "bone_nar",     
  "lung",     
  "pleura",  
  "peri",     
  "liver",    
  "brain",   
  "skin",      
  "neck",   
  "super",    
  "axil",     
  "medias",    
  "abdominal"  
 
)
View(water)
# print total number of NAs in the data

sum(is.na(water))

# print the columns with NAs in the data

colnames(water)[colSums(is.na(water)) > 0]

str(water)

library(Amelia)

missmap(water)

water[is.na(water)] <- 50
## scale func.watertion standardizes the values






scaled.water <- scale(water)



View(scaled.water)


# Identifying the optimal number of clusters


library(NbClust)
?NbClust

set.seed(100)
nc.water <- NbClust(water, distance = "euclidean", method="kmeans")
table(nc.water$Best.n[1,])

barplot(table(nc.water$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")
fviz_nbclust(water, kmeans, method = "wss") + labs(subtitle = "Elbow method")

fviz_nbclust(water, kmeans, method = "silhouette")



?kmeans
kwater = kmeans(x=scaled.water, centers = 6, nstart = 10)
kwater

## plotting the clusters
##install.packages("fpc")
library(fpc)
plotcluster(scaled.water, kwater$cluster)


library(cluster)
?clusplot
clusplot(scaled.water, kwater$cluster, 
         color=TRUE, shade=TRUE, labels=2, lines=1)

#add a new column Cluster and assign values of clusters to individual data points
water$Clusters <- kwater$cluster
View(water)
aggr = aggregate(water,list(water$Clusters),mean)
profile.updated2 <- data.frame( Cluster=aggr[,1],
                               Freq=as.vector(table(water$Clusters)),
                               aggr)

View(profile.updated2)
aggr = aggregate(water,list(water$Clusters),mean)
profile.updated <- data.frame( Cluster=aggr[,1],
                               Freq=as.vector(table(water$Clusters)),
                               aggr)
View(profile.updated)
