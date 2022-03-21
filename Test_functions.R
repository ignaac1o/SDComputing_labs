##############################3
# R serial script BIN
##############################3

rndm_centroids=function(k,data){
  X=matrix(nrow=k,ncol=ncol(data)+1)
  clusters=letters[1:k]
  for (i in 1:nrow(X)) {
    for(j in 1:ncol(data)){
      X[i,j]=generate_random(data[,j]) 
    }
  }
  X[,ncol(data)+1]=as.factor(letters[1:k])
  #X = data.frame(X, stringsAsFactors = FALSE)
  #Now we have one centroid for each column on X
  return(X)
}

compute_distances=function(X,data){
  x=c()
  data$error=NULL
  data$cluster=NULL
  for (i in 1:nrow(data)) {
    for(j in 1:nrow(X)){
      x[j]=euclidian(X[j,-8],data[i,1:7])
    }
    data$error[i]<-min(x)
    data$cluster[i]<-which(x==min(x))
  }
  assign("knn_data",data,.GlobalEnv)
}

recode_clusters=function(data){
  centroids= data %>% group_by(cluster) %>% 
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
  
}

index=which(data_wo_factors$price<1500)
data_wo_factors$cluster=1
data_wo_factors[index,8]="A"
data_wo_factors[-index,8]="B"
names(data_wo_factors)
data_wo_factors %<>% rename("cluster"="V8")



data=group_by(.data = data_wo_factors,cluster)
data=summarize(.data=group_by(.data = data_wo_factors,cluster),
          price=mean(price),
          speed=mean(speed),
          hd=mean(hd),
          ram=mean(ram),
          screen=mean(screen),
          cores=mean(cores),
          trend=mean(trend))
mutate(.data = summarize(.data=group_by(.data = data_wo_factors,cluster),
                              price=mean(price),
                              speed=mean(speed),
                              hd=mean(hd),
                              ram=mean(ram),
                              screen=mean(screen),
                              cores=mean(cores),
                              trend=mean(trend)),
            n_centroide=cluster)
select(mutate(.data = summarize(.data=group_by(.data = data_wo_factors,cluster),
                                     price=mean(price),
                                     speed=mean(speed),
                                     hd=mean(hd),
                                     ram=mean(ram),
                                     screen=mean(screen),
                                     cores=mean(cores),
                                     trend=mean(trend)),
                   n_centroide=cluster),-cluster)
centroids=as.data.frame(dplyr::ungroup(select(mutate(.data = summarize(.data=group_by(.data = knn_data,cluster),
                                                            price=mean(price),
                                                            speed=mean(speed),
                                                            hd=mean(hd),
                                                            ram=mean(ram),
                                                            screen=mean(screen),
                                                            cores=mean(cores),
                                                            trend=mean(trend)),
                                                              n_centroide=cluster),-cluster)))
#Run

knn_data=as.data.frame(scale(data_wo_factors))
X=rndm_centroids(2,knn_data)
compute_distances(X,knn_data)


error=c(0,sum(knn_data$error))

i=2
while(round(error[i],2)!= round(error[i-1],2)){
  compute_distances(X,knn_data)
  error=c(error,sum(knn_data$error))
  X=recode_clusters(knn_data)
  i=i+1
}

#Elbow graph with existing function
obtain_k_optimal=function(kkmax){
  knn=NULL
  for (i in 1:kmax) {
    knn[i]=list(kmeans(x = data_wo_factors,centers = i))
  }
  return(knn)
}

knn=obtain_k_optimal()

x=NULL
y=NULL
for (i in 1:10) {
  y[i]=knn[[i]]$tot.withinss
  x[i]=i
}

df=data.frame(x,y)

ggplot(data = df, aes(x=x,y=y)) + geom_point() + geom_line() 


##########################################
##########################################

##############################3
# R parallel MP script BIN
##############################3
k=2
knn_data=as.data.frame(scale(data_wo_factors))

getDoParWorkers()
no_cores=detectCores()

#Generate random centroids
X=matrix(nrow=k,ncol=ncol(knn_data))

clust=makeCluster(no_cores)
for (i in 1:nrow(X)) {
  X[i,]=apply(X=knn_data,MARGIN = 2,FUN = generate_random)
}
stopCluster(clust)

X=as.matrix(cbind(X,1:k))


#Distances
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


knn_data$error=NULL
knn_data$cluster=NULL

clust=makeCluster(no_cores)

x=data.frame()
clusterExport(clust,"x",envir=environment())
clusterExport(clust,"X",envir=environment())
clusterExport(clust,"knn_data",envir=environment())
for(i in nrow(knn_data)){
  x[i,]=parApply(MARGIN = 1,X = X[,-nrow(X)],FUN = euclidian,a=knn_data[i,-c(8,9)],cl = clust)
  #knn_data$error[i]<-min(x)
  #knn_data$cluster[i]<-which(x==min(x))
}

stopCluster(clust)



for(i in nrow(X)){
  t=parApply(MARGIN = 1,X = X[,-nrow(X)],FUN = euclidian,a=knn_data[i,])
}



t=apply(MARGIN = 1,X = X[,-nrow(X)],FUN = euclidian,a=knn_data[1,-c(8,9)])



n=ncol(knn_data)
m=nrow(X)
x=matrix(nrow = nrow(knn_data),ncol = m)
for(i in 1:m){
  x[,i]=apply(X =knn_data,MARGIN = 1,FUN = euclidian,b=X[i,])
}
for(i in 1:nrow(knn_data)){
  knn_data$error[i]<-min(x[i,])
  knn_data$cluster[i]<-which(x[i,]==min(x[i,]))
}
x=NULL



##########################################
##########################################

##############################3
# R parallel THREADS script BIN
##############################3
no_cores=detectCores()
clust=makeCluster(no_cores,type = "FORK")
registerDoParallel(clust)

knn_data=as.data.frame(scale(data_wo_factors))
k=2

#Generate random centroids
X=matrix(nrow=k,ncol=ncol(knn_data))
clusters=letters[1:k]
for (j in 1:k) {
  X[j,]=foreach(i=1:(ncol(X)),.combine = cbind) %dopar% generate_random(knn_data[,i])
}
X=as.matrix(cbind(X,1:k))
stopCluster(clust)

#Compute Distances
#registerDoParallel(clust)
x=c()
knn_data$error=NULL
knn_data$cluster=NULL
for (i in 1:nrow(knn_data)) {
  x=foreach(j=1:nrow(X), .combine=c) %dopar% euclidian(X[j,],knn_data[i,-c(8,9)])
  knn_data$error[i]<-min(x)
  knn_data$cluster[i]<-which(x==min(x))
}
stopCluster(clust)





