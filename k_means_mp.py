##############################################################################################################################################
######################################################  PART 1: SERIAL VERSION. OPTION A  ####################################################
##############################################################################################################################################

# Import libraries 
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import time
import multiprocessing as mp

# Implement k-means algorithm
def k_means(dataset, k):
    '''
    It computes several iterations relocating centroids to achieve the final distribution of clusters. The algorithm stops when the centroids of
    te iteration i and the ones of the iteration i-1 match. This will be the way to convergence. The function returns the input dataset adding up 
    two new variables: the assignment and the errors associated.
    
    PARAMETERS:
    "dataset": dataset
    "k": number of clusters
    '''

    data = dataset.copy()
    it = 0

    # Initialize centroids and error.
    n , p = data.shape[0] , data.shape[1]
    new_centroids = np.array(data.sample(n=k).reset_index(drop=True)) # Select random observations as initial centroids.
    new_centroids = np.c_[ new_centroids, np.ones(new_centroids.shape[0]) ] 
    error = np.array([])
    n_iter = True
    data = data.to_numpy()
    data = np.c_[data, np.ones(data.shape[0])] 
    while(n_iter):
      errors = np.array([n])
      distances = np.zeros((n, k))
      for i in range(k): 
        distances[:,i] = np.sqrt(np.sum(np.square(np.subtract(data[:,0:p], np.atleast_2d(new_centroids[i,:p]))), axis=1))

      min_v , min_index = np.amin(distances, axis=1) , np.argmin(distances, axis=1)
      data[:,p], errors = min_index , min_v
      error = np.append(error, np.sum(errors))
      for i in range(k):
        group = np.where(data[:,p] == i)[0]
        assign = data[group,]
        centroid_i = np.mean(assign, axis=0)
        new_centroids[i,] = centroid_i
      it += 1
      if it > 2:
        if (error[it-1] == error[it-2]):
          n_iter = False
        else:
          n_iter = True
      else:
        n_iter = True
    
    data = np.c_[ data, np.ones(n) ] 
    data[:,p+1] = errors
    data_final = pd.DataFrame(data, 
             columns=['price', 
                      'speed',
                      'hd',
                      'ram', 
                      'screen',
                      'cores',
                      'centroid',
                      'errors'])
    return (data_final)

if __name__ == "__main__":

  # Load the dataset. We discard the index (first column) and categorical variables
  data_read = pd.read_csv("computers500k.csv").drop(['id', 'laptop', 'cd', 'trend'], axis=1) 

  # Standarize data
  data_norm = data_read.apply(lambda x: (x-x.mean())/ x.std())

  #Elbow Graph.
  print("-----------------------------------------------------------------------------------------------------------------------------------")
  print("------------------------------------------------ ELBOW GRAPH ----------------------------------------------------------------------")
  print("-----------------------------------------------------------------------------------------------------------------------------------")

  n = 5
  start = time.time()
  pool = mp.Pool(mp.cpu_count())
  squares_mp = [pool.apply(k_means, args=(data_norm, i+1)) for i in range(n)]
  pool.close() 
  end = time.time()
  list_errors = [sum(squares_mp[i]['errors']) for i in range(n)]
  print("Time elapsed:",end - start,"seconds")
  plt.figure(figsize=(10,5))
  plt.title("Elbow Graph")
  plt.xlabel("Number of clusters (k) ", size = 16)
  plt.ylabel("WSS", size = 16)
  plt.plot(range(1,n+1), list_errors)
  plt.show()

  print("-----------------------------------------------------------------------------------------------------------------------------------")
  print("------------------------------ The optimal value for k is 2. We run the algorith with k = 2 ---------------------------------------")
  print("-----------------------------------------------------------------------------------------------------------------------------------")

  # k=2, optimal clusters number
  # Run the algorithm with k=2
  # We measure time 
  k_final = 2
  start = time.time()
  data_kmeans = k_means(data_norm, k_final)
  end = time.time()
  print("Time elapsed:",end - start,"seconds")

  print("-----------------------------------------------------------------------------------------------------------------------------------")
  print("------------------------------------------------ We plot the first two dimensions -------------------------------------------------")
  print("-----------------------------------------------------------------------------------------------------------------------------------")

  # Plot the first two dimensions of the clusters
  plt.figure(figsize=(10,5))
  plt.title("First two dimensions")
  plt.xlabel("Price ", size = 16,)
  plt.ylabel("Speed", size = 16)
  sns.scatterplot(x="price", y="speed", hue="centroid", data=data_kmeans.drop("errors", axis = 1))
  plt.show()

  print("-----------------------------------------------------------------------------------------------------------------------------------")
  print("-------------------------------------------------------- HEAT MAP -----------------------------------------------------------------")
  print("-----------------------------------------------------------------------------------------------------------------------------------")
  # Print the heat map
  data_kmeans_we = data_kmeans.drop("errors", axis = 1)
  plt.figure(figsize=(10,5))
  plt.title("Heat map")
  plt.xlabel("Variables", size = 16,)
  plt.ylabel("Centroids", size = 16)
  sns.heatmap(data_kmeans_we.groupby('centroid').agg('mean').reset_index(drop = True))
  plt.show() 