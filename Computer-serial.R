library(plyr)
library(dplyr)
library(factoextra)
library(magrittr)


data5k=read.csv(file = "computers5k.csv",header = T)
data5k$id = NULL
data5k$cd %<>% mapvalues(from = c("yes","no"), to = c("1","0"))  %>% as.factor()
data5k$laptop %<>% mapvalues(from = c("yes","no"), to = c("1","0")) %>% as.factor()

summary(data5k)

#kmeans only work with numeric vectors
data_wo_factors = data5k %>% dplyr::select(c(-cd,-laptop))

# 1.- Construct the elbow graph and find the optimal clusters number (k).
set.seed(123)
par(mfrow=c(1,2))
fviz_nbclust(data_wo_factors, kmeans, method = "wss") + geom_vline(xintercept = 2, linetype = 2)
fviz_nbclust(data_wo_factors, kmeans, method = "silhouette") 

#both methods suggest 2 clusters as the optimal number.

# 2.- Implement the k-means algorithm

obtain_k_optimal=function(){
  knn=NULL
  for (i in 1:10) {
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


# 3.- Cluster the data using the optimum value using k-means.

# 4.-Measure time

library(microbenchmark)
time=microbenchmark(kmeans(x = data_wo_factors,centers = 2,iter.max = 10))

# 5.- Plot the results of the elbow graph.

# 6.- Plot the first 2 dimensions of the clusters

fviz_cluster(knn, data = data_wo_factors,
             palette = c("#2E9FDF", "#E7B800","#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

# 7.- Find the cluster with the highest average price and print it.

#8.- Print a heat map using the values of the clusters centroids.






############## K means function

data_wf_scaled=scale(data_wo_factors)

#Used to generate random numbers
generate_random=function(vector){
  return(runif(1,min(vector),max(vector)))
}

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

euclidian=function(a,b){
  sqrt(sum((a-b)^2))
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
  print(head(data))
  assign("data_wf_scaled",data,.GlobalEnv)
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

X=rndm_centroids(3,data_wf_scaled)

error=c(0,sum(new_data$error))

i=2
while(round(error[i],2)!= round(error[i-1],2)){
  compute_distances(X,as.data.frame(data_wf_scaled))
  error=c(error,sum(data_wf_scaled$error))
  X=recode_clusters(data_wf_scaled)
  i=i+1
}


library(ggplot2)

ggplot(data_wf_scaled,aes(x=hd,y=speed,color=as.factor(cluster))) + geom_point()





