
library(plyr)
library(dplyr)
library(factoextra)
library(magrittr)
library(ggplot2)
library(microbenchmark)
library(parallel)
library(doParallel)
library(foreach)

set.seed(15)

data5k=read.csv(file = "computers5k.csv",header = T)
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

kmeans_diy=function(data,k){
  
  #Scale data
  kmeans_data=as.data.frame(scale(data))
  
  #Generate random centroids
  X=matrix(nrow=k,ncol=ncol(kmeans_data)+1)
  clusters=letters[1:k]
  for (i in 1:nrow(X)) {
    for(j in 1:ncol(kmeans_data)){
      X[i,j]=generate_random(kmeans_data[,j]) 
    }
  }
  X[,ncol(kmeans_data)+1]=as.factor(letters[1:k])
  
  
  #Compute Distances
  n=ncol(kmeans_data)
  m=nrow(X)
  nX=ncol(X)
  x=matrix(nrow = nrow(kmeans_data),ncol = m)
  for(i in 1:m){
    x[,i]=apply(X =kmeans_data,MARGIN = 1,FUN = euclidian,b=X[i,-nX])
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

    
    X=as.data.frame(dplyr::ungroup(dplyr::select(plyr::mutate(.data = dplyr::summarize(.data=dplyr::group_by(.data = kmeans_data,cluster),
                                                                                       price=mean(price),
                                                                                       speed=mean(speed),
                                                                                       hd=mean(hd),
                                                                                       ram=mean(ram),
                                                                                       screen=mean(screen),
                                                                                       cores=mean(cores)),
                                                              n_centroide=cluster),-cluster)))
    
    
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
    print(e)
    #
  }
  
  return(kmeans_data)
}



no_cores=detectCores()
clust=makeCluster(no_cores)
clusterExport(clust,"data_wo_factors",envir = environment())
clusterExport(clust,"generate_random",envir = environment())
clusterExport(clust,"euclidian",envir = environment())

Start <- Sys.time()
k_means=parLapply(cl = clust,X = 1:5,fun = kmeans_diy,data=data_wo_factors)
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


