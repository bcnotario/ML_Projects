# Viola-Jones Face Recognition
Face recognition algorithm using the Viola-Jones object detection framework to accurately detect faces in real-time 

### Integral Image
The algorithm converts each face and background training image to grayscale and calculates the integral image of each
```
VJ1 <- iimage()
```
### Haar-Like Feature Value Functions
Four functions to calculate the values of the four Haar-Like features for a given image and specified rectangle vertices
* Haar-Like Feature Value 1 - Two adjacent, congruent vertical rectangles
* Haar-Like Feature Value 2 - Two adjacent, congruent horizontal rectangles
* Haar-Like Feature Value 3 - Three adjacent, congruent vertical rectangles
* Haar-Like Feature Value 4 - Four adjacent, congruent rectangles that bisect one larger rectangle
### Haar-Like Feature Table & Training Feature Values
The algorithm creates the Feature Table and stores Training Feature Values
* Feature Table - Contains the coordinates of all possible feature values within a 64x64 pixel image
* Training Feature Values - Contains all feature values for all training faces and all training backgrounds
```
VJ2 <- haarfeat(VJ1$images.sum,VJ1$backs.sum,h1.scale,h2.scale,h3.scale,h4.scale)
```
### AdaBoost
AdaBoost learning algorithm to accurately determine several strong face classifiers from the training images; each strong classifier is built from a linear combination of several weak face classifiers and a strong classifier threshold to eliminate missed faces (false negatives) in the training data
```
VJ3 <- boost(VJ2$face.mat,VJ2$back.mat,VJ2$feat.n)
```
### Classifier Cascade
Application of strong face classifiers to detect faces in a test image (larger than 64x64 pixels) via successive false positive eliminating filters
```
VJ4 <- classcascade(VJ3$class.list,VJ2$feattbl,VJ2$haar.mat)
```
