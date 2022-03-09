
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
    
    X=as.data.frame(dplyr::ungroup(dplyr::select(plyr::mutate(.data = dplyr::summarize(.data=dplyr::group_by(.data = knn_data,cluster),
                                                                    price=mean(price),
                                                                    speed=mean(speed),
                                                                    hd=mean(hd),
                                                                    ram=mean(ram),
                                                                    screen=mean(screen),
                                                                    cores=mean(cores)),
                                                                    #trend=mean(trend)),
                                                  n_centroide=cluster),-cluster)))
    
    #Next iteration
    e=e+1
    #
    print(e)
    #
  }
  
  return(knn_data)
}



no_cores=detectCores()
clust=makeCluster(no_cores)
clusterExport(clust,"data_wo_factors",envir = environment())
clusterExport(clust,"generate_random",envir = environment())
clusterExport(clust,"euclidian",envir = environment())

knn=parLapply(cl = clust,X = 1:5,fun = knn_diy,data=data_wo_factors)



stopCluster(clust)


# 2.- Measure the time and optimize the program to get the fastest version you can.
Start <- Sys.time()
knn=parLapply(cl = clust,X = 1:5,fun = knn_diy,data=data_wo_factors)
end <- Sys.time()
time <- end - Start
print(time)

# 3.- Plot the first 2 dimensions of the clusters
ggplot(knn[[2]],aes(x=price,y=speed,color=as.factor(cluster))) + geom_point()


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

hpricefun(knn[[2]]) #The highest price corresponds to the first element of the list (1 cluster).

# 5.- Print a heat map using the values of the clusters centroids.
datamatrix <- data_wo_factors %<>% as.matrix()
heatmap(x = datamatrix, scale="none", col = knn[[2]]$cluster, cexRow = 0.7)




# Elbow Graph (no lo pide)
x=NULL
y=NULL
for (i in 1:length(knn)) {
  y[i]=sum(knn[[i]]$error)
  x[i]=i
}

df=data.frame(x,y)
ggplot(df,aes(x=x,y=y))+geom_point()+geom_line()


