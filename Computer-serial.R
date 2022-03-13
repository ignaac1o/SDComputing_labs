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
data5k$trend %<>% as.factor() 
summary(data5k)

#kmeans only works with numeric vectors
data_wo_factors = data5k %>% dplyr::select(c(-cd,-laptop,-trend))

############## K means DIY Process #####################

#Used to generate random numbers
generate_random=function(vector){
  return(runif(1,min(vector),max(vector)))
}

euclidian=function(a,b){
  sqrt(sum((a-b)^2))
}

kmeans_diy=function(data,k){
  
  #Scale data
  kmeans_data=as.data.frame(scale(data))
  n=ncol(kmeans_data)
  #Generate random centroids
  X=matrix(nrow=k,ncol=(n+1))
  #clusters=letters[1:k]
  for (i in 1:nrow(X)) {
    for(j in 1:n){
      X[i,j]=generate_random(kmeans_data[,j]) 
    }
  }
  X[,n+1]=as.factor(letters[1:k])
  
  
  #Compute Distances
  n=ncol(kmeans_data)
  m=nrow(X)
  nX=ncol(X)
  x=matrix(nrow = nrow(kmeans_data),ncol = m)
  for(i in 1:m){
    x[,i]=apply(X = kmeans_data,MARGIN = 1,FUN = euclidian,b=X[i,-nX])
  }
  for(i in 1:nrow(kmeans_data)){
    kmeans_data$error[i]<-min(x[i,])
    kmeans_data$cluster[i]<-which(x[i,]==min(x[i,]))
  }
  x=NULL

  #Check errors
  error=c(0,sum(kmeans_data$error))
  e=2
  
  while(round(error[e],0)!= round(error[e-1],0)){
    #Recode Clusters
    #kmeans_data$cluster %<>% as.factor() 
    X = kmeans_data %>% group_by(cluster) %>% 
      dplyr::summarize(price=mean(price),
                speed=mean(speed),
                hd=mean(hd),
                ram=mean(ram),
                screen=mean(screen),
                cores=mean(cores)) %>% 
      mutate(n_centroide=cluster) %>% 
      select(-cluster) %>% 
      ungroup() %>% as.data.frame(.)
    
    #Compute distances
    n=ncol(kmeans_data)-2
    m=nrow(X)
    nX=ncol(X)
    x=matrix(nrow = nrow(kmeans_data),ncol = m)
    for(i in 1:m){
      x[,i]=apply(X = kmeans_data[,-c(7,8)],MARGIN = 1,FUN = euclidian,b=X[i,-nX])
    }
    for(i in 1:nrow(kmeans_data)){
      kmeans_data$error[i]<-min(x[i,])
      kmeans_data$cluster[i]<-which(x[i,]==min(x[i,]))
    }
    x=NULL
    
    #Write error
    error=c(error,sum(kmeans_data$error))
  
    #Next iteration
    e=e+1
    #
    print(error)
    #
  }
  
  return(kmeans_data)
}

# 1.- Construct the elbow graph and find the optimal clusters number (k).

# 2.- Implement the k-means algorithm

obtain_k_optimal_serial=function(data,k){
  k_means=NULL
  for (i in 1:k) {
    k_means[i]=list(kmeans_diy(data,i))
  }
  return(k_means)
}

# 4.- Measure time

start=Sys.time()
k_means = obtain_k_optimal_serial(data_wo_factors,5)
stop=Sys.time()

# 5.- Elbow graph.

x=NULL
y=NULL
for (i in 1:length(k_means)) {
  y[i]=sum(k_means[[i]]$error)
  x[i]=i
}

df=data.frame(x,y)


# 4.-Measure time
print(stop-start)

# 5.- Plot the results of the elbow graph.
ggplot(data = df, aes(x=x,y=y)) + geom_point() + geom_line() #Elbow GRaph suggest 2 clusters


# 6.- Plot the first 2 dimensions of the clusters

ggplot(k_means[[2]],aes(x=price,y=speed,color=as.factor(cluster))) + geom_point()

# 7.- Find the cluster with the highest average price and print it.

hpricefun <- function(datos){
  x = list()
  n = ncol(datos)
  datos[,n] %<>% as.factor()
  k = length(levels(datos[,n]))
  for(i in 1:k){
    ind1 <- which(datos$cluster==i)
    price1 <- datos$price[ind1]
    x[i]=mean(price1)
  }
  return(x)
}

hpricefun(k_means[[2]])



#8.- Print a heat map using the values of the clusters centroids.
clustersum=k_means[[2]] %>% group_by(cluster) %>%  dplyr::summarize(price=mean(price),
                                                                speed=mean(speed),
                                                                hd=mean(hd),
                                                                ram=mean(ram),
                                                                screen=mean(screen),
                                                                cores=mean(cores)) %>% 
  dplyr::select(-1) %>% as.matrix()

gplots::heatmap.2(x=clustersum,scale = "none",cexRow = 0.7,trace="none",density.info = "none")
