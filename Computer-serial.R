library(plyr)

data5k=read.csv(file = "computers5k.csv",header = T)

data5k$cd %<>% mapvalues(from = c("yes","no"), to = c("1","0"))  %>% as.factor()
data5k$laptop %<>% mapvalues(from = c("yes","no"), to = c("1","0")) %>% as.factor()

summary(data5k)

# 1.- Construct the elbow graph and find the optimal clusters number (k).

# 2.- Implement the k-means algorithm

# 3.- Cluster the data using the optimum value using k-means.

# 4.-Measure time

# 5.- Plot the results of the elbow graph.

# 6.- Plot the first 2 dimensions of the clusters

# 7.- Find the cluster with the highest average price and print it. 

#8.- Print a heat map using the values of the clusters centroids.