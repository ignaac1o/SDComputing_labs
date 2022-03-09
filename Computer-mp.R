
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
#microbenchmark(parLapply(cl = clust,X = 1:5,fun = knn_diy,data=data_wo_factors),times = 1)
knn=knn_diy(data_wo_factors,2)

stopCluster(clust)


x=NULL
y=NULL
for (i in 1:length(knn)) {
  y[i]=sum(knn[[i]]$error)
  x[i]=i
}

df=data.frame(x,y)

#Elbow Graph
ggplot(df,aes(x=x,y=y))+geom_point()+geom_line()

#Plot
#ggplot(knn[[2]],aes(x=price,y=speed,color=as.factor(cluster))) + geom_point()

ggplot(knn,aes(x=price,y=speed,color=as.factor(cluster))) + geom_point()

