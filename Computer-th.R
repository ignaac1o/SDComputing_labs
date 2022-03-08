
library(plyr)
library(dplyr)
library(factoextra)
library(magrittr)
library(ggplot2)
library(microbenchmark)
library(parallel)
library(doParallel)
library(foreach)

set.seed(13)

data5k=read.csv(file = "computers5k.csv",header = T)
data5k$id = NULL
data5k$cd %<>% mapvalues(from = c("yes","no"), to = c("1","0"))  %>% as.factor()
data5k$laptop %<>% mapvalues(from = c("yes","no"), to = c("1","0")) %>% as.factor()

summary(data5k)

#kmeans only work with numeric vectors
data_wo_factors = data5k %>% dplyr::select(c(-cd,-laptop))

generate_random=function(vector){
  return(runif(1,min(vector),max(vector)))
}

euclidian=function(a,b){
  sqrt(sum((a-b)^2))
}

clust=makeCluster(no_cores,type = "FORK")

knn_diy_th=function(data,k,cluster){
  
  #Scale data
  knn_data=as.data.frame(scale(data))
  
  #Generate random centroids
  X=matrix(nrow=k,ncol=ncol(knn_data))
  clusters=letters[1:k]
  for (j in 1:k) {
    X[j,]=foreach(i=1:(ncol(X)),.combine = cbind) %dopar% generate_random(knn_data[,i])
  }
  X=as.matrix(cbind(X,1:k))
  
  
  #Compute Distances
  x=c()
  knn_data$error=NULL
  knn_data$cluster=NULL
  for (i in 1:nrow(knn_data)) {
    x=foreach(j=1:nrow(X), .combine=c) %dopar% euclidian(X[j,],knn_data[i,-c(8,9)])
    knn_data$error[i]<-min(x)
    knn_data$cluster[i]<-which(x==min(x))
  }
  
  
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
      x=foreach(j=1:nrow(X), .combine=c) %dopar% euclidian(X[j,],knn_data[i,-c(8,9)])
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



obtain_k_optimal_th=function(k,data){
  knn=foreach(i=1:k) %dopar% knn_diy(data,i)
}

no_cores=detectCores()
clust=makeCluster(no_cores,type = "FORK")
registerDoParallel(clust)

#microbenchmark(knn=obtain_k_optimal_th(5,data_wo_factors),times=1)
knn=obtain_k_optimal_th(5,data_wo_factors)

stopCluster(clust)



#PLot elbow graph
x=NULL
y=c()
no_cores=detectCores()
clust=makeCluster(no_cores,type = "FORK")
registerDoParallel(clust)

y=foreach(i=1:length(knn),.combine=c) %dopar% sum(knn[[1]][[i]]$error)
x=1:length(knn[[1]])

stopCluster(clust)


x=NULL
y=NULL
for (i in 1:length(knn[[1]])) {
  y[i]=sum(knn[[1]][[i]]$error)
  x[i]=i
}

df=data.frame(x,y)


ggplot(data = df, aes(x=x,y=y)) + geom_point() + geom_line() 


#Classification
ggplot(knn[[1]][[2]],aes(x=price,y=speed,color=as.factor(cluster))) + geom_point()

