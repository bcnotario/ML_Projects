# Digit Classifiers
Various classification methods to solve the full 10-digit MNIST handwritten digit classification task

### Mixture Model Digit Classifier
Mixture of Bernoullis using the Expectation-Maximization algorithm
* The specified total mixtures needs to be large enough to have an accurate model but small enough to avoid overfitting the training data
* The initial mixture models are assigned randomly and the initial parameters (mixture model prior probability, Bernoulli mean) are respectively computed
* The Expectation-step updates the mixture model assignment probabilities
* The Maximization-step updates the mixture model and Bernoulli parameters
* The Expectation and Maximization steps are recursively repeated until the iterative change in the log-likelihood is <1
```
Digit.MM(mixtures)
```
### Neural Network Digit Classifier
Feed-forward neural network trained by backpropagation
* The specified number of hidden layers, the learning rate, and the epochs need to optimized to maximize the model classifier accuracy
* The transfer functions are sigmoid for the hidden layer and soft-max for the output layer
* The input layer has 785 (28x28 + 1 bias) neurons and output layer has 10 neurons
```
Digit.NN(n.hidden,n.eta,n.epoch)
```
