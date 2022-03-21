##############################################################################################################################################
##############################################  PART 1:  Parallel implementation, multiprocessing.  ##################################
##############################################################################################################################################

# Import libraries 
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import time
import multiprocessing as mp
from itertools import repeat

# Implement k-means algorithm
def k_means(dataset, k):
    '''
    It computes several iterations relocating centroids to achieve the final distribution of clusters. The algorithm computes the distance in 
    each iteration to carry out the assignment.The error is compared in each iteration with the error obtained in the iteration i-1.The function
    returns the input dataset adding up two new variables: the assignment ("centroid") and the errors associated ("errors").
    
    PARAMETERS:
    "dataset": dataset
    "k": number of clusters
    '''

    data = dataset.copy()
    it = 0

    # Initialize centroids and error. We handle with NumPy vectors and matrices instead of objects of the library Pandas Dataframe
    # because of the difference on the performance. 
    n , p = data.shape[0] , data.shape[1]
    new_centroids = np.array(data.sample(n=k, random_state = 100).reset_index(drop=True)) # Select random observations as initial centroids.
    new_centroids = np.c_[ new_centroids, np.ones(new_centroids.shape[0]) ] 
    error = np.array([])
    n_iter = True
    data = data.to_numpy()
    data = np.c_[data, np.ones(data.shape[0])] #Create the new column to host the new variable "centroid"
    while(n_iter):
      errors = np.array([n])
      distances = np.zeros((n, k))
      for i in range(k): 
        # We compute the euclidean distance and host the results in as many columns as k 
        distances[:,i] = np.sqrt(np.sum(np.square(np.subtract(data[:,0:p], np.atleast_2d(new_centroids[i,:p]))), axis=1))

      # We obtain the minimum distance and the index associated (the cluster associated)
      min_v , min_index = np.amin(distances, axis=1) , np.argmin(distances, axis=1)
      data[:,p], errors = min_index , min_v
      error = np.append(error, np.sum(errors)) #total error
      for i in range(k):
        # We calculate the new centroids
        assign = data[np.where(data[:,p] == i)[0],] 
        new_centroids[i,] = np.mean(assign, axis=0)
      it += 1
      if it > 2:
        # We compare the error i with error i-1
        if (error[it-1] == error[it-2]):
          n_iter = False
        else:
          n_iter = True
      else:
        n_iter = True
    
    # We create the DataFrame which will be returned with the new two columns
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
  
  # We try to use pool.apply() but the performance did not improve at all. We try to use pool.map()
  # redefining the main function. However, the performance did improve considerably using pool.starmap()
  # and maintaining constant the argument data. We use zip() to aggregate the iterables in a tuple.
  n = 5
  start = time.time()
  pool = mp.Pool(int(mp.cpu_count()/2))
  squares_mp = list(pool.starmap(k_means, zip(repeat(data_norm), range(1,6))))
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