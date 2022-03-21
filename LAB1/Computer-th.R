
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

data=read.csv(file = "computers500k.csv",header = T)
data$id = NULL
data$cd %<>% mapvalues(from = c("yes","no"), to = c("1","0"))  %>% as.factor()
data$laptop %<>% mapvalues(from = c("yes","no"), to = c("1","0")) %>% as.factor()
data$trend %<>% as.factor()
summary(data)

#kmeans only work with numeric vectors
data_wo_factors = data %>% dplyr::select(c(-cd,-laptop,-trend))

no_cores=detectCores()

generate_random=function(vector){
  return(runif(1,min(vector),max(vector)))
}

euclidian=function(a,b){
  sqrt(sum((a-b)^2))
}

kmeans_diy_th=function(data,k){
  kmeans_data=as.matrix(scale(data_wo_factors))
  colX=ncol(kmeans_data)
  rowX=k
  X=matrix(ncol = colX,nrow = rowX)
  for(i in 1:rowX){
    X[i,]=apply(X=kmeans_data,MARGIN = 2,generate_random)
  }
  X=cbind(X,1:2)
  centroids_equal=FALSE
  count=0
  err=0
  nrowX=nrow(X)
  ncolX=ncol(X)
  nrowkmeans=nrow(kmeans_data)
  
  while(centroids_equal==FALSE){
    count=count+1
    #Threads
    clusts=makeCluster(no_cores,type = "FORK")
    registerDoParallel(clusts)
    x=foreach(i=1:nrowX,.combine = cbind) %dopar% apply(X =kmeans_data,MARGIN = 1,FUN = euclidian,b=X[i,-ncolX])
    stopCluster(clusts)
    #
    cluster=c()
    error=c()
    if(k==1){
      error=x
      cluster=rep(1,nrowkmeans)
    }else{
      for(i in 1:nrowkmeans){
        error[i]<-min(x[i,])
        cluster[i]<-which(x[i,]==min(x[i,]))
      }
    }
    kmeans_data=cbind(kmeans_data,error,cluster)
    
    X_new = kmeans_data %>% as.data.frame() %>% group_by(cluster) %>% 
      dplyr::summarize(price=mean(price),
                       speed=mean(speed),
                       hd=mean(hd),
                       ram=mean(ram),
                       screen=mean(screen),
                       cores=mean(cores)) %>% 
      mutate(n_centroide=cluster) %>% 
      select(-cluster) %>% 
      ungroup() %>% as.matrix()
    
    if(round(sum(error),0)==round(err,0)){
      centroids_equal=TRUE
    }else{
      X=X_new
      kmeans_data=kmeans_data[,-(7:8)]
      err=sum(error)
      X_new=NULL
      x=NULL
    }
    print(count)
  }
  return(as.data.frame(kmeans_data))
}


kmeans_diy=function(data,k){
  kmeans_data=as.matrix(scale(data_wo_factors))
  colX=ncol(kmeans_data)
  rowX=k
  X=matrix(ncol = colX,nrow = rowX)
  for(i in 1:rowX){
    X[i,]=apply(X=kmeans_data,MARGIN = 2,generate_random)
  }
  X=cbind(X,1:2)
  centroids_equal=FALSE
  x=matrix(ncol=k,nrow=nrow(kmeans_data))
  ncolX=ncol(X)
  nrowkmeans=nrow(kmeans_data)
  count=0
  err=0
  while(centroids_equal==FALSE){
    count=count+1
    x=matrix(ncol=k,nrow=nrow(kmeans_data))
    for(i in seq_len(k)){
      x[,i]=apply(X=kmeans_data,MARGIN = 1,FUN = euclidian,b=X[i,-ncolX])
    }
    cluster=c()
    error=c()
    for(i in 1:nrowkmeans){
      error[i]<-min(x[i,])
      cluster[i]<-which(x[i,]==min(x[i,]))
    }
    
    kmeans_data=cbind(kmeans_data,error,cluster)
    
    X_new = kmeans_data %>% as.data.frame() %>% group_by(cluster) %>% 
      dplyr::summarize(price=mean(price),
                       speed=mean(speed),
                       hd=mean(hd),
                       ram=mean(ram),
                       screen=mean(screen),
                       cores=mean(cores)) %>% 
      mutate(n_centroide=cluster) %>% 
      select(-cluster) %>% 
      ungroup() %>% as.matrix()
    
    
    #if(all_equal(round(X_new,3),round(X,3))==TRUE){
    if(round(sum(error),0)==round(err,0)){
      centroids_equal=TRUE
    }else{
      X=X_new
      kmeans_data=kmeans_data[,-(7:8)]
      err=sum(error)
      X_new=NULL
      x=NULL
    }
    print(count)
  } 
  return(as.data.frame(kmeans_data))
}

########### PARALLEL W THREADS  function ###############

obtain_k_optimal_threads=function(data,k){
  knn=NULL
  for (j in 1:k) {
    knn[j]=list(kmeans_diy_th(data,j))
  }
  return(knn)
}

start=Sys.time()
knn=obtain_k_optimal_threads(data_wo_factors,5)
stop=Sys.time()

#Time to compute knn_diy_th functions for different values of k
print(stop-start)

############# obtain k optimal using THREADS and original kmeans_diy #########################

clust=makeCluster(no_cores,type = "FORK")
registerDoParallel(clust)


#Do function to obtain elbow graph in parallel
obtain_k_optimal_th=function(k,data){
  knn=foreach(i=1:k) %dopar% kmeans_diy(data,i)
}

#MEASURE TIME
start=Sys.time()
kmeans=obtain_k_optimal_th(5,data_wo_factors)
stop=Sys.time()

stopCluster(clust)

#Execution time knn_diy for different values of k using threads
print(stop-start)



#Elbow Graph

x=NULL
y=NULL
for (i in 1:length(kmeans)) {
  y[i]=sum(kmeans[[i]]$error)
  x[i]=i
}
df=data.frame(x,y)

#Plot Elbow Graph
ggplot(data = df, aes(x=x,y=y)) + geom_point() + geom_line() 


#PLOT FIRST TWO DIMMENSIONS
ggplot(kmeans[[2]],aes(x=price,y=speed,color=as.factor(cluster))) + geom_point()

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

hpricefun(kmeans[[2]])


#PRINT HEATMAP
clustersum=kmeans[[2]] %>% group_by(cluster) %>%  dplyr::summarize(price=mean(price),
                                                                speed=mean(speed),
                                                                hd=mean(hd),
                                                                ram=mean(ram),
                                                                screen=mean(screen),
                                                                cores=mean(cores)) %>% 
                    dplyr::select(-1) %>% as.matrix()

gplots::heatmap.2(x=clustersum,scale = "none",cexRow = 0.7,trace="none",density.info = "none")
