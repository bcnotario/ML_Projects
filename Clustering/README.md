# Clustering
Various clustering methods to partition n observations (of d dimensions) into k clusters

### k-Gaussians Mixture Clustering
Mixture of Gaussians using the Expectation-Maximization algorithm; the initial clusters are assigned randomly and the initial parameters (cluster prior probability, Gaussian center, Gaussian covariance matrix) are computed; the cluster assignment probabilities are updated in the Expectation-step and the cluster and Gaussian parameters are updated in the Maximization-step until the cluster assignments no longer change

### k-Means Clustering
Standard k-means algorithm with the initial clusters assigned randomly; the centroids are recursively computed and each point is reassigned to the closest centroid and until the cluster assignments no longer change

### k-Means++ Clustering
The first centroid chosen uniformly at random and the remaining centroids selected based on a probability proportional to their squared distance from the nearest centroid until all the centroids are determined; then standard k-means algorithm starts are this initialization and it completes when the cluster assignments no longer change
