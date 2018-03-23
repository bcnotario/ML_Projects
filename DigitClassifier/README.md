# Digit Classifiers
Various classification methods to solve the full 10-digit MNIST handwritten digit classification task

### Mixture Model Digit Classifier
Mixture of Bernoullis using the Expectation-Maximization algorithm where the specified total mixtures needs to be 
large enough to have an accurate model but small enough to avoid overfitting the training data; the initial mixture models
are assigned randomly and the initial parameters (mixture model prior probability, Bernoulli mean) are respectively computed;
the mixture model assignment probabilities are updated in the Expectation-step and the mixture model and Bernoulli 
parameters are updated in the Maximization-step recursively until the change in the log-likelihood is <1
```
Digit.MM(mixtures)
```
### Neural Network Digit Classifier
Feed-forward neural network trained by backpropagation with a sigmoid transfer function in the hidden layer and a 
soft-max transfer function in the output layer; the input and output layers respectively consist of 785 and 10 neurons;
the specified number of hidden layers, the learning rate, and the epochs need to optimized to maximize the model classifier accuracy
```
Digit.NN(n.hidden,n.eta,n.epoch)
```
