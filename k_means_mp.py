##############################################################################################################################################
#############################################  PART 2: Parallel implementation, multiprocessing  #############################################
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
    "data": dataset
    "k": number of clusters
    '''

    data = dataset.copy()

    # Initialize centroids and error.
    n = data.shape[0]
    new_centroids = data.sample(n=k, random_state=100).reset_index(drop=True) # Select random observations as initial centroids.
    error = np.array([])
    it = 0
    n_iter = True
    old_centroids = np.zeros((k, data.shape[1]))

    while(n_iter):
      errors = np.array([n])
      distances = pd.DataFrame()
      for i in range(k): 
        distances.loc[:,i] = np.array(data.apply(lambda a,b: np.sqrt(np.sum((a-b)**2)), args = [new_centroids.iloc[i,:]], axis=1))
        min_v , min_index = np.array(distances.min(axis = 1)) , np.array(distances.idxmin(axis = 1))
      data['centroid'], errors = min_index , min_v
      error = np.append(error, np.sum(errors))
      new_centroids = data.groupby('centroid').agg('mean').reset_index(drop = True)
      it += 1
      if (np.round(new_centroids,1) == np.round(old_centroids,1)).all().all():
        n_iter = False
      else:
        old_centroids = new_centroids
        n_iter = True
      
    data['errors'] = errors
    return (data)

if __name__ == "__main__":

  # Load the dataset. We discard the index (first column) and categorical variables
  data_read = pd.read_csv("computers5k.csv").drop(['id', 'laptop', 'cd', 'trend'], axis=1) 

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
