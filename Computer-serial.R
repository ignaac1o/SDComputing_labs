library(plyr)
library(dplyr)
library(factoextra)
library(magrittr)


data5k=read.csv(file = "computers5k.csv",header = T)
data5k$id = NULL
data5k$cd %<>% mapvalues(from = c("yes","no"), to = c("1","0"))  %>% as.factor()
data5k$laptop %<>% mapvalues(from = c("yes","no"), to = c("1","0")) %>% as.factor()

summary(data5k)

#kmeans only work with numeric vectors
data_wo_factors = data5k %>% dplyr::select(c(-cd,-laptop))

# 1.- Construct the elbow graph and find the optimal clusters number (k).
set.seed(123)
par(mfrow=c(1,2))
fviz_nbclust(data_wo_factors, kmeans, method = "wss") + geom_vline(xintercept = 2, linetype = 2)
fviz_nbclust(data_wo_factors, kmeans, method = "silhouette") 

#both methods suggest 2 clusters as the optimal number.

# 2.- Implement the k-means algorithm

obtain_k_optimal=function(){
  knn=NULL
  for (i in 1:10) {
    knn[i]=list(kmeans(x = data_wo_factors,centers = i))
  }
  return(knn)
}

knn=obtain_k_optimal()

x=NULL
y=NULL
for (i in 1:10) {
  y[i]=knn[[i]]$tot.withinss
  x[i]=i
}

df=data.frame(x,y)

ggplot(data = df, aes(x=x,y=y)) + geom_point() + geom_line() 


# 3.- Cluster the data using the optimum value using k-means.

# 4.-Measure time

library(microbenchmark)
time=microbenchmark(kmeans(x = data_wo_factors,centers = 2,iter.max = 10))

# 5.- Plot the results of the elbow graph.

# 6.- Plot the first 2 dimensions of the clusters

fviz_cluster(knn, data = data_wo_factors,
             palette = c("#2E9FDF", "#E7B800","#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

# 7.- Find the cluster with the highest average price and print it.

#8.- Print a heat map using the values of the clusters centroids.


