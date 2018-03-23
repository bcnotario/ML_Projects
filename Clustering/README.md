# Clustering
Various clustering methods to partition n observations (of d dimensions) into k clusters

### k-Gaussians Mixture Clustering
Mixture of Gaussians using the Expectation-Maximization algorithm; the initial clusters are assigned randomly and the initial parameters (cluster prior probability, Gaussian center, Gaussian covariance matrix) are respectively computed; the cluster assignment probabilities are updated in the Expectation-step and the cluster and Gaussian parameters are updated in the Maximization-step recursively until the cluster assignments no longer change

### k-Means Clustering
Standard k-means algorithm with the initial clusters assigned randomly; the centroids are computed and each point is reassigned to the closest centroid recursively until the cluster assignments no longer change with each recursion

### k-Means++ Clustering
The standard k-means algorithm with a specific cluster initialization algorithm; the first centroid is chosen uniformly at random and the remaining centroids are selected based on a probability proportional to their squared Euclidean distance from the nearest centroid until all the centroids are initially determined; it completes when the cluster assignments no longer change with each recursion
