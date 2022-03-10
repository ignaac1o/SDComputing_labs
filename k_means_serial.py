
# Import libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import time

# Function to standarize dataset
def standarize_data(data_input):
    return data_input.apply(lambda x: (x-x.mean())/ x.std())

# Function to initialize centroids randomly
def initialize_centroids(k, data):
    """
    This function initialize centroids randomly. In particular, this function choose an observation at random 
    and assigns it as a centroid.
    k: number of clusters.
    data: dataset.
    """
    return data.sample(n=k).reset_index(drop=True)

def dist_eucl(a,b):
  return np.sqrt(np.sum((a-b)**2))

def assign_obs(data, centroids):
    """
    It calculates the distance between centroids and each observation of the dataset. Finally, it assigns each
    observation to the nearest cluster. It returns two lists: a list which contains the assignments and other list 
    with the distance associated. 
    data: dataset.
    centroids: the observations which work as centroids.
    """

    n , k = (data.shape[0], centroids.shape[0])

    distances = pd.DataFrame()
    for i in range(k): 
      distances.loc[:,i] = list(data.apply(dist_eucl, args = [centroids.iloc[i,:]], axis=1))
      min_v , min_index = list(distances.min(axis = 1)) , list(distances.idxmin(axis = 1))

    return (min_index, min_v)

def cluster_iterations(dataset, k, n_iterations):
    '''
    It computes several iterations relocating centroids to achieve the final distribution of clusters.
    data: dataset
    k: number of clusters
    '''

    data = dataset.copy()


    # Initialize centroids and error
    centroids = initialize_centroids(k, data)
    error = []
    it = 0

    for i in range(n_iterations):
      data['centroid'], data['errors'] = assign_obs(data, centroids)
      error.append(sum(data['errors']))
      centroids = data.groupby('centroid').agg('mean').reset_index(drop = True)
      it += 1
      if it > 2 and (round(error[i]) == round(error[i-1])):
          break
      else:
          continue

    data['centroid'], data['errors'] = assign_obs(data,centroids)
    return (data)

# Load the dataset
data_read = pd.read_csv("computers.csv").iloc[:,1:7] # We discard the index (first column) and categorical variables

# Standarize data
data_norm = standarize_data(data_read)

#Elbow Graph
start = time.time()
n = 5
n_it = 5
squares = [sum((cluster_iterations(data_norm,i+1, n_it))['errors']) for i in range(n)]
end = time.time()
print("Time elapsed:",end - start,"seconds")
plt.figure(figsize=(10,5))
plt.title("Elbow Graph")
plt.plot(range(1,n+1), squares)
plt.show()

# k=2, optimal clusters number
# Run the algorithm with k=2
# We measure time with time
k_final = 2
n_it = 4
start = time.time()
data_kmeans = cluster_iterations(data_norm, k_final, n_it)
end = time.time()
print("Time elapsed:",end - start,"seconds")

# Plot the first two dimensions of the clusters
sns.scatterplot(x="price", y="speed", hue="centroid", data=data_kmeans)
plt.show()

# We see the cluster with largest average price
mean = []
for i in range(k_final-1):
  mean.append(np.mean(data_kmeans['price'].loc[(data_kmeans.centroid == i)]))
  print("The cluster with the largest average price is", mean.index(max(mean)), ",with mean", min(mean))

# Print a heat map
data_kmeans_we = data_kmeans.drop("errors", axis = 1)
sns.heatmap(data_kmeans_we.groupby('centroid').agg('mean').reset_index(drop = True))
plt.show()

