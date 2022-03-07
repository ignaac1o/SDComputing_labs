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
# R parallel script BIN
##############################3



