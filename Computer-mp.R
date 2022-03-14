
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

data5k=read.csv(file = "computers500k.csv",header = T)
data5k$id = NULL
data5k$cd %<>% mapvalues(from = c("yes","no"), to = c("1","0"))  %>% as.factor()
data5k$laptop %<>% mapvalues(from = c("yes","no"), to = c("1","0")) %>% as.factor()
data5k$trend %<>% as.factor()

summary(data5k)

#kmeans only work with numeric vectors
data_wo_factors = data5k %>% dplyr::select(c(-cd,-laptop,-trend))

# 1.- Write a parallel version of you program using multiprocessing

############## K means DIY Process #####################

#Used to generate random numbers
generate_random=function(vector){
  return(runif(1,min(vector),max(vector)))
}

euclidian=function(a,b){
  sqrt(sum((a-b)^2))
}

kmeans_diy_mp=function(data,k){
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
    #kmeans_data$cluster=cluster
    #kmeans_data$error=error
    
    X_new = as.matrix(dplyr::ungroup(dplyr::select(plyr::mutate(.data = dplyr::summarize(.data=dplyr::group_by(.data = as.data.frame(kmeans_data),cluster),
                                                                                         price=mean(price),
                                                                                         speed=mean(speed),
                                                                                         hd=mean(hd),
                                                                                         ram=mean(ram),
                                                                                         screen=mean(screen),
                                                                                         cores=mean(cores)),
                                                                n_centroide=cluster),-cluster)))
    
    
    #if(all_equal(round(X_new,3),round(X,3))==TRUE){
    if(round(sum(error),0)==round(err,0)){
      centroids_equal=TRUE
    }else{
      X=X_new
      kmeans_data=kmeans_data[,-(7:8)]
      #kmeans_data$cluster=NULL
      #kmeans_data$error=NULL
      err=sum(error)
      X_new=NULL
      x=NULL
    }
    print(count)
  } 
  return(as.data.frame(kmeans_data))
}


no_cores=detectCores()
clust=makeCluster(no_cores)
clusterExport(clust,"data_wo_factors",envir = environment())
clusterExport(clust,"generate_random",envir = environment())
clusterExport(clust,"euclidian",envir = environment())

Start <- Sys.time()
k_means=parLapply(cl = clust,X = 1:5,fun = kmeans_diy_mp,data=data_wo_factors)
end <- Sys.time()


stopCluster(clust)


# 2.- Measure the time and optimize the program to get the fastest version you can.
time <- end - Start
print(time)

# 3.- Plot the first 2 dimensions of the clusters
ggplot(k_means[[2]],aes(x=price,y=speed,color=as.factor(cluster))) + geom_point()


# 4- Find the cluster with the highest average price and print it.

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

hpricefun(k_means[[2]]) #The highest price corresponds to the first element of the list (1 cluster).

# 5.- Print a heat map using the values of the clusters centroids.
clustersum=k_means[[2]] %>% group_by(cluster) %>%  dplyr::summarize(price=mean(price),
                                                                speed=mean(speed),
                                                                hd=mean(hd),
                                                                ram=mean(ram),
                                                                screen=mean(screen),
                                                                cores=mean(cores)) %>% 
  dplyr::select(-1) %>% as.matrix()

gplots::heatmap.2(x=clustersum,scale = "none",cexRow = 0.7,trace="none",density.info = "none")


# Elbow Graph (no lo pide)
x=NULL
y=NULL
for (i in 1:length(k_means)) {
  y[i]=sum(k_means[[i]]$error)
  x[i]=i
}

df=data.frame(x,y)
ggplot(df,aes(x=x,y=y))+geom_point()+geom_line()


