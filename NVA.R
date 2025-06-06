library(ggplot2)
library(dplyr)
#loadig data set to r
library(readxl)
NVA <- read_excel("nike_V_adidias.xlsx")
summary(NVA)

#cleaning data 
NVA$Brand <- as.factor(NVA$Brand)
NVA$'Last Visited' <- as.Date(NVA$'Last Visited')
NVA$Rating <- as.numeric(NVA$Rating)
NVA$Discount <- as.factor(NVA$Discount)
#removing unused variables 
NVA$`Product ID`<- NULL
NVA$'Last Visited'<- NULL

#made all iterations of Adidas brand, "Adidas"
library(dplyr)
NVA <- NVA %>% 
  mutate(Brand= case_when 
         (Brand %in% c("Adidas Adidas ORIGINALS", "Adidas CORE / NEO", "Adidas ORIGINALS", "Adidas SPORT PERFORMANCE")~ "Adidas", 
           TRUE ~ Brand))

#Which brand has a higher overall rating
library(ggplot2)
ggplot(NVA, aes(x=Brand, y=Rating, fill=Brand)) +
  geom_boxplot() +
  scale_color_brewer(palette="Set1") +
  labs(fill="Brand") +
  xlab("Brand") +
  ylab("Ratings")

library(dplyr)
Nike <- (NVA %>% filter(Brand =="Nike"))
Adidas <- (NVA %>% filter(Brand =="Adidas"))

#How does the list price relate to the sale price 
ggplot(NVA, aes(x =`Listing Price` , y = `Sale Price` )) +
  geom_point(color = "red") +
  ggtitle("Scatter Plot") +
  xlab("List Price") +
  ylab("Sale Price") +
  theme_minimal()

#How do rating and sale pricerelate by brand 
#Nike
ggplot(Nike, aes(x =Rating , y = `Sale Price` )) +
  geom_point(color = "green") +
  ggtitle("Nike Pricing Correlations") +
  xlab("Rating") +
  ylab("Sale Price") +
  theme_minimal()

#Adidas
ggplot(Adidas, aes(x = factor(Discount), y = Rating)) +
  geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
  ggtitle("Adidas Discount vs Rating") +
  xlab("Discount (%)") +
  ylab("Rating") +
  theme_minimal()


install.packages("ggcorrplot")
library(ggcorrplot)

correlation_matrix <- cor(NVA[, c("Rating", "Reviews", "Sale Price")])
ggcorrplot(correlation_matrix, lab = TRUE)

#installing and loding packages for clustering 
install.packages("cluster")
install.packages("fpc")
install.packages("dbscan")

library(cluster)
library(dbscan)
library(fpc)
library(scales)

#Deleting categorical variables for clustering 
numNVA <- NVA[,c(-1,-5,-6)]
numNike<- Nike[,c(-1,-5,-6)]
numAdidas<- Adidas[,c(-1,-5,-6)]
summary(numNVA)
View(numNVA)

numNVA$`Listing Price` <- as.numeric(numNVA$`Listing Price`) 
numNVA$`Sale Price` <- as.numeric(numNVA$`Sale Price`)
numNVA$Discount <- as.numeric(numNVA$Discount)
numNVA$Rating <- as.numeric(numNVA$Rating)
numNVA$Reviews <- as.numeric(numNVA$Reviews)


numNike$`Listing Price` <- as.numeric(numNike$`Listing Price`)
numNike$`Sale Price` <- as.numeric(numNike$`Sale Price`)
numNike$Discount <- as.numeric(numNike$Discount)
numNike$Rating <- as.numeric(numNike$Rating)
numNike$Reviews <- as.numeric(numNike$Reviews)

numAdidas$`Listing Price` <- as.numeric(numAdidas$`Listing Price`)
numAdidas$`Sale Price` <- as.numeric(numAdidas$`Sale Price`)
numAdidas$Discount <- as.numeric(numAdidas$Discount)
numAdidas$Rating <- as.numeric(numAdidas$Rating)
numAdidas$Reviews <- as.numeric(numAdidas$Reviews)

#Normalization of variables
numNVA_norm <- numNVA 
numNike_norm <- numNike
numAdidas_norm <- numAdidas

numNVA_norm$`Listing Price` <-rescale(numNVA$`Listing Price`, to = c(0,1))
numNVA_norm$`Sale Price` <- rescale(numNVA$`Sale Price`, to = c(0,1))
numNVA_norm$Discount<- rescale(numNVA$Discount, to = c(0,1))
numNVA_norm$Rating <- rescale(numNVA$Rating, to = c(0,1))
numNVA_norm$Reviews <- rescale(numNVA$Reviews, to = c(0,1))

numNike_norm$`Listing Price` <- rescale(numNike_norm$`Listing Price`, to = c(0,1))
numNike_norm$`Sale Price` <- rescale(numNike_norm$`Sale Price`, to = c(0,1))
numNike_norm$Discount <- rescale(numNike_norm$Discount, to = c(0,1))
numNike_norm$Rating <- rescale(numNike_norm$Rating, to = c(0,1))
numNike_norm$Reviews <- rescale(numNike_norm$Reviews, to = c(0,1))

numAdidas_norm$`Listing Price` <- rescale(numAdidas_norm$`Listing Price`, to = c(0,1))
numAdidas_norm$`Sale Price` <- rescale(numAdidas_norm$`Sale Price`, to = c(0,1))
numAdidas_norm$Discount <- rescale(numAdidas_norm$Discount, to = c(0,1))
numAdidas_norm$Rating <- rescale(numAdidas_norm$Rating , to = c(0,1))
numAdidas_norm$Reviews <- rescale(numAdidas_norm$Reviews, to = c(0,1))

pairs(numAdidas_norm)
pairs(numNike_norm)
pairs(numNVA_norm)

library(dbscan)

#NIKE and ADIDAS

k <- 6  # based on expected clusters
knn_distances <- kNNdist(numNVA_norm, k)

# Plot the k-nearest neighbor distances to find  eps
plot(sort(knn_distances), type="b", pch=19, main="KNN Distance Plot for DBSCAN",
     xlab="Points Sorted by Distance", ylab="KNN Distance")

#DBSCAN clustering with eps = 0.175 and MinPts = 4
dbr <- dbscan(numNVA_norm, eps=0.175, MinPts=6)
dbr$cluster
pairs(numNVA_norm,col=dbr$cluster)

distm <- dist(numNVA_norm)
dbcv_vec <- 0
count <- 1
for (eps in seq(0.05, 0.3, by=0.05)) {
  dbr <- dbscan(numNVA_norm, eps=eps, MinPts=6)
  dbcv <- dbcv(numNVA_norm, dbr$cluster)
  dbcv_vec[count] <- dbcv$score
  count <- count+1
  print(eps)
}
plot(c(0.05,0.1,0.15,0.2,0.25,0.3),dbcv_vec)
lines(c(0.05,0.1,0.15,0.2,0.25,0.3),dbcv_vec)


#NIKE

k <- 6  # based on expected clusters
knn_distances <- kNNdist(numNike_norm, k)

# Plot the k-nearest neighbor distances to find  eps
plot(sort(knn_distances), type="b", pch=19, main="KNN Distance Plot for DBSCAN",
     xlab="Points Sorted by Distance", ylab="KNN Distance")

dbr <- dbscan(numNike_norm, eps=0.1, MinPts=6)
dbr$cluster
pairs(numNike_norm, col=dbr$cluster)

distm <- dist(numNike_norm)
dbcv_vec <- 0
count <- 1
for (eps in seq(0.05, 0.3, by=0.05)) {
  dbr <- dbscan(numNike_norm, eps=eps, MinPts=6)
  dbcv <- dbcv(numNike_norm, dbr$cluster)
  dbcv_vec[count] <- dbcv$score
  count <- count+1
  print(eps)
}
plot(c(0.05,0.1,0.15,0.2,0.25,0.3), dbcv_vec)
lines(c(0.05,0.1,0.15,0.2,0.25,0.3), dbcv_vec)



#ADIDAS 
k <- 6  # based on expected clusters
knn_distances <- kNNdist(numAdidas_norm, k)

# Plot the k-nearest neighbor distances to find eps
plot(sort(knn_distances), type="b", pch=19, main="KNN Distance Plot for DBSCAN",
     xlab="Points Sorted by Distance", ylab="KNN Distance")

dbr <- dbscan(numAdidas_norm, eps=0.15, MinPts=6)
dbr$cluster
pairs(numAdidas_norm, col=dbr$cluster)

distm <- dist(numAdidas_norm)
dbcv_vec <- 0
count <- 1
for (eps in seq(0.05, 0.3, by=0.05)) {
  dbr <- dbscan(numAdidas_norm, eps=eps, MinPts=6)
  dbcv <- dbcv(numAdidas_norm, dbr$cluster)
  dbcv_vec[count] <- dbcv$score
  count <- count+1
  print(eps)
}
plot(c(0.05,0.1,0.15,0.2,0.25,0.3), dbcv_vec)
lines(c(0.05,0.1,0.15,0.2,0.25,0.3), dbcv_vec)


#Visualization best clustering 
numNVA_norm <- numNVA 
numNVA_norm$`Listing Price` <-rescale(numNVA$`Listing Price`, to = c(0,1))
numNVA_norm$`Sale Price` <- rescale(numNVA$`Sale Price`, to = c(0,1))
numNVA_norm$Discount<- rescale(numNVA$Discount, to = c(0,1))
numNVA_norm$Rating <- rescale(numNVA$Rating, to = c(0,1))
numNVA_norm$Reviews <- rescale(numNVA$Reviews, to = c(0,1))

dbr <- dbscan(numNVA_norm, eps=0.175, MinPts=6)
dbr$cluster
numNVA_norm$Cluster <- as.factor(dbr$cluster)

ggplot(numNVA_norm, aes(x=Rating, color=Cluster, fill=Cluster)) + 
  geom_density(alpha=0.3) + 
  theme_minimal() +
  labs(title="Density Plot of Rating by Cluster", x="Rating", y="Density") 

ggplot(numNVA_norm, aes(x=Reviews, color=Cluster, fill=Cluster)) + 
  geom_density(alpha=0.3) + 
  theme_minimal() +
  labs(title="Density Plot of Reviews by Cluster", x="Reviews", y="Density") 

ggplot(numNVA_norm, aes(x=`Sale Price`, color=Cluster, fill=Cluster)) + 
  geom_density(alpha=0.3) + 
  theme_minimal() +
  labs(title="Density Plot of Sale Price by Cluster", x="Sale Price", y="Density") 


#Density Plot of Rating by Cluster for Nike

numNike_norm <- numNike
numNike_norm$`Listing Price` <- rescale(numNike_norm$`Listing Price`, to = c(0,1))
numNike_norm$`Sale Price` <- rescale(numNike_norm$`Sale Price`, to = c(0,1))
numNike_norm$Discount <- rescale(numNike_norm$Discount, to = c(0,1))
numNike_norm$Rating <- rescale(numNike_norm$Rating, to = c(0,1))
numNike_norm$Reviews <- rescale(numNike_norm$Reviews, to = c(0,1))


dbr <- dbscan(numNike_norm, eps=0.1, MinPts=6)
dbr$cluster
numNike_norm$Cluster <- as.factor(dbr$cluster)

#Nike Rating 
ggplot(numNike_norm, aes(x=Rating, color=Cluster, fill=Cluster)) + 
  geom_density(alpha=0.3) + 
  theme_minimal() +
  labs(title="Density Plot of Rating by Cluster for Nike ", x="Rating", y="Density")

#Nike Review
ggplot(numNike_norm, aes(x=Reviews, color=Cluster, fill=Cluster)) + 
  geom_density(alpha=0.3) + 
  theme_minimal() +
  labs(title="Density Plot of Reviews by Cluster for Nike", x="Reviews", y="Density") 

#Nike Sale Price
ggplot(numNike_norm, aes(x=`Sale Price`, color=Cluster, fill=Cluster)) + 
  geom_density(alpha=0.3) + 
  theme_minimal() +
  labs(title="Density Plot of Sale Price by Cluster Nike", x="Sale Price", y="Density") 


#Density Plot of Rating by Cluster for Adidas
numAdidas_norm <- numAdidas
numAdidas_norm$`Listing Price` <- rescale(numAdidas_norm$`Listing Price`, to = c(0,1))
numAdidas_norm$`Sale Price` <- rescale(numAdidas_norm$`Sale Price`, to = c(0,1))
numAdidas_norm$Discount <- rescale(numAdidas_norm$Discount, to = c(0,1))
numAdidas_norm$Rating <- rescale(numAdidas_norm$Rating , to = c(0,1))
numAdidas_norm$Reviews <- rescale(numAdidas_norm$Reviews, to = c(0,1))


dbr <- dbscan(numAdidas_norm, eps=0.0375, MinPts=6)
dbr$cluster
numAdidas_norm$Cluster <- as.factor(dbr$cluster)

ggplot(numAdidas_norm, aes(x=Rating, color=Cluster, fill=Cluster)) + 
  geom_density(alpha=0.3) + 
  theme_minimal() +
  labs(title="Density Plot of Rating by Cluster for Adidas ", x="Rating", y="Density")

ggplot(numAdidas_norm, aes(x=Reviews, color=Cluster, fill=Cluster)) + 
  geom_density(alpha=0.3) + 
  theme_minimal() +
  labs(title="Density Plot of Reviews by Cluster for Adidas", x="Reviews", y="Density") 

ggplot(numAdidas_norm, aes(x=`Sale Price`, color=Cluster, fill=Cluster)) + 
  geom_density(alpha=0.3) + 
  theme_minimal() +
  labs(title="Density Plot of Sale Price by Cluster for Adidas", x="Sale Price", y="Density") 



