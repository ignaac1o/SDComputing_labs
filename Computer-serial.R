library(plyr)
library(dplyr)
library(factoextra)
library(magrittr)
library(ggplot2)
library(microbenchmark)

set.seed(13)

data5k=read.csv(file = "computers5k.csv",header = T)
data5k$id = NULL
data5k$cd %<>% mapvalues(from = c("yes","no"), to = c("1","0"))  %>% as.factor()
data5k$laptop %<>% mapvalues(from = c("yes","no"), to = c("1","0")) %>% as.factor()

summary(data5k)

#kmeans only work with numeric vectors
data_wo_factors = data5k %>% dplyr::select(c(-cd,-laptop))
View(data_wo_factors)

############## K means DIY Process #####################

#Used to generate random numbers
generate_random=function(vector){
  return(runif(1,min(vector),max(vector)))
}

euclidian=function(a,b){
  sqrt(sum((a-b)^2))
}

knn_diy=function(data,k){
  
  #Scale data
  knn_data=as.data.frame(scale(data))
  
  #Generate random centroids
  X=matrix(nrow=k,ncol=ncol(knn_data)+1)
  clusters=letters[1:k]
  for (i in 1:nrow(X)) {
    for(j in 1:ncol(knn_data)){
      X[i,j]=generate_random(knn_data[,j]) 
    }
  }
  X[,ncol(knn_data)+1]=as.factor(letters[1:k])
  
  
  #Compute Distances
  x=c()
  knn_data$error=NULL
  knn_data$cluster=NULL
  for (i in 1:nrow(knn_data)) {
    for(j in 1:nrow(X)){
      x[j]=euclidian(X[j,-ncol(X)],knn_data[i,1:(ncol(knn_data)-2)])
    }
    knn_data$error[i]<-min(x)
    knn_data$cluster[i]<-which(x==min(x))
  }
  #
  print(head(knn_data))
  #
  
  #Check errors
  error=c(0,sum(knn_data$error))
  e=2
  
  #
  print(error)
  #
  
  while(round(error[e],2)!= round(error[e-1],2)){
    #Compute distances
    x=c()
    for (i in 1:nrow(knn_data)) {
      for(j in 1:nrow(X)){
        x[j]=euclidian(X[j,-ncol(X)],knn_data[i,1:(ncol(knn_data)-2)])
      }
      knn_data$error[i]<-min(x)
      knn_data$cluster[i]<-which(x==min(x))
    }
    
    #Write error
    error=c(error,sum(knn_data$error))
    
    #Recode Clusters
    centroids= knn_data %>% group_by(cluster) %>% 
      summarize(price=mean(price),
                speed=mean(speed),
                hd=mean(hd),
                ram=mean(ram),
                screen=mean(screen),
                cores=mean(cores),
                trend=mean(trend)) %>% 
      mutate(n_centroide=cluster) %>% 
      select(-cluster) %>% 
      ungroup() %>% as.data.frame(.)
    
    #Next iteration
    e=e+1
    #
    print(e)
    #
  }
  
  return(knn_data)
}


knn_data=knn_diy(data_wo_factors,2)




# 1.- Construct the elbow graph and find the optimal clusters number (k).


# 2.- Implement the k-means algorithm

obtain_k_optimal=function(kmax){
  knn=NULL
  for (i in 1:kmax) {
    knn[i]=list(knn_diy(data_wo_factors,i))
  }
  return(knn)
}

knn=obtain_k_optimal(5)

x=NULL
y=NULL
for (i in 1:length(knn)) {
  y[i]=sum(knn[[i]]$error)
  x[i]=i
}

df=data.frame(x,y)




# 3.- Cluster the data using the optimum value using k-means.

#The plot suggest k=5

# 4.-Measure time

microbenchmark(knn_diy(data_wo_factors,2), kmeans(data_wo_factors,2), times = 1)

# 5.- Plot the results of the elbow graph.

ggplot(data = df, aes(x=x,y=y)) + geom_point() + geom_line() 

# 6.- Plot the first 2 dimensions of the clusters

ggplot(knn_data,aes(x=price,y=speed,color=as.factor(cluster))) + geom_point()

# 7.- Find the cluster with the highest average price and print it.

ind1 <- which(knn_data$cluster==1)
price1 <- knn_data$price[ind1]
mean(price1)
ind2 <- which(knn_data$cluster==2)
price2 <- knn_data$price[ind2]
mean(price2)

hpricefun <- function(datos){
  x = list()
  n = ncol(datos)
  datos[,n] %<>% as.factor()
  k = length(levels(datos[,n]))
  for(i in 1:k){
    ind1 <- which(knn_data$cluster==i)
    price1 <- knn_data$price[ind1]
    x[i]=mean(price1)
  }
  return(x)
}

hpricefun(knn_data)


#8.- Print a heat map using the values of the clusters centroids.

datamatrix <- data_wo_factors %<>% as.matrix()
datosknnmatrix <- knn_data[,-c(8,9)] %<>% as.matrix()
par(mfrow = c(1, 2))
image(t(datamatrix)[, nrow(datamatrix):1], yaxt = "n", main = "Original Data")
image(t(datamatrix)[, order(knn_data$cluster)], yaxt = "n", main = "Clustered Data")


