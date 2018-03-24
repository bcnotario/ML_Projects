# Clustering
Various clustering methods to partition *n* observations (of *d* dimensions) into *k* clusters

### k-Gaussians Mixture Clustering
Mixture of Gaussians using the Expectation-Maximization algorithm
* The initial clusters are assigned randomly and the initial parameters (cluster prior probability, Gaussian center, Gaussian covariance matrix) are respectively computed
* The Expectation-step updates the cluster assignment probabilities
* The Maximization-step updates the cluster and Gaussian parameters
* The Expectation and Maximization steps are recursively repeated until the cluster assignments no longer change
```
kgauss(data,k)
```
### k-Means Clustering
Standard k-means algorithm with the initial clusters assigned randomly; the centroids are computed and each point is reassigned to the closest centroid recursively until the cluster assignments no longer change with each recursion
```
kmeans(data,k)
```
### k-Means++ Clustering
The standard k-means algorithm with a specific cluster initialization algorithm
* The first centroid is chosen uniformly at random 
* The squared Euclidean distance between each point and the closest centroid is computed
* A new centroid is chosen with probability distribution proportional to the squared Euclidean distance
* The last two steps are recursively repeated until all the initial centroids have been determined
```
kmeans.pp(data,k)
```
