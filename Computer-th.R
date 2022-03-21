
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
data5k$trend %<>% as.factor()
summary(data5k)

#kmeans only work with numeric vectors
data_wo_factors = data5k %>% dplyr::select(c(-cd,-laptop,-trend))

no_cores=detectCores()

generate_random=function(vector){
  return(runif(1,min(vector),max(vector)))
}

euclidian=function(a,b){
  sqrt(sum((a-b)^2))
}

knn_diy_th=function(data,k){
  
  #Scale data
  knn_data=as.data.frame(scale(data))
  
  #Generate random centroids
  X=matrix(nrow=k,ncol=ncol(knn_data)+1)
  #clusters=letters[1:k]
  for (i in 1:nrow(X)) {
    for(j in 1:ncol(knn_data)){
      X[i,j]=generate_random(knn_data[,j]) 
    }
  }
  X[,ncol(knn_data)+1]=as.factor(letters[1:k])
  
  
  #Compute Distances
  n=ncol(knn_data)
  m=nrow(X)
  nX=ncol(X)
  clusts=makeCluster(no_cores,type = "FORK")
  registerDoParallel(clusts)
  x=foreach(i=1:m,.combine = cbind) %dopar% apply(X =knn_data,MARGIN = 1,FUN = euclidian,b=X[i,-nX])
  stopCluster(clusts)
  if(k==1){
    knn_data$error=x[i]
    knn_data$cluster=1
  }else{
  for(i in 1:nrow(knn_data)){
    knn_data$error[i]<-min(x[i,])
    knn_data$cluster[i]<-which(x[i,]==min(x[i,]))
  }
  }
  x=NULL

  #Check errors
  error=c(0,sum(knn_data$error))
  e=2
  
  while(round(error[e],0)!= round(error[e-1],0)){
    #Recode Clusters
    X= knn_data %>% group_by(cluster) %>% 
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
    n=ncol(knn_data)-2
    m=nrow(X)
    nX=ncol(X)
    clustes=makeCluster(no_cores,type = "FORK")
    registerDoParallel(clustes)
    x=foreach(i=1:m,.combine = cbind) %dopar% apply(X =knn_data,MARGIN = 1,FUN = euclidian,b=X[i,-nX])
    stopCluster(clustes)
    if(k==1){
      knn_data$error=x[i]
      knn_data$cluster=1
    }else{
      for(i in 1:nrow(knn_data)){
        knn_data$error[i]<-min(x[i,])
        knn_data$cluster[i]<-which(x[i,]==min(x[i,]))
      }
    }
    #x=NULL
    
    #Write error
    error=c(error,sum(knn_data$error))
    
    #Next iteration
    e=e+1
    #
    print(error)
    #
  }
  
  return(knn_data)
}

########### BEGIN PARALLEL W THREADS ###############


obtain_k_optimal_threads=function(data,k){
  knn=NULL
  for (j in 1:k) {
    knn[j]=list(knn_diy_th(data,j))
  }
  return(knn)
}

start=Sys.time()
knn=obtain_k_optimal_threads(data_wo_factors,5)
stop=Sys.time()

print(stop-start)

############# STOP THREADS #########################

x=NULL
y=NULL
for (i in 1:length(knn)) {
  y[i]=sum(knn[[i]]$error)
  x[i]=i
}
df=data.frame(x,y)

#Plot Elbow Graph
ggplot(data = df, aes(x=x,y=y)) + geom_point() + geom_line() 


#PLOT FIRST TWO DIMMENSIONS
ggplot(knn[[2]],aes(x=price,y=speed,color=as.factor(cluster))) + geom_point()

#FIND CLUSTER WITH HIGEST AVERAGE PRICE
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

hpricefun(knn[[2]])


#PRINT HEATMAP
clustersum=knn[[2]] %>% group_by(cluster) %>%  dplyr::summarize(price=mean(price),
                                                                speed=mean(speed),
                                                                hd=mean(hd),
                                                                ram=mean(ram),
                                                                screen=mean(screen),
                                                                cores=mean(cores)) %>% 
                    dplyr::select(-1) %>% as.matrix()

gplots::heatmap.2(x=clustersum,scale = "none",cexRow = 0.7,trace="none",density.info = "none")
