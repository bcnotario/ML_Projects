#Sigmoid & Softmax Transfer Functions for Neural Net
sig <- function(a){(1+exp(-a))^-1}
softmax <- function(b){exp(b)/sum(exp(b))}

#Neural Network Digit Classifier
#Inputs: n.hidden (Hidden layer neurons), n.eta (learning rate), n.epochs (Training cycles)
#Data Files: TrainDigitX.csv, TrainDigitY.csv, TestDigitX.csv, TestDigitY.csv
#Functions: sig, softmax

Digit.NN <- function(n.hidden,n.eta,n.epoch){

  #Setup Training Data
  trainX <- read.csv("/Users/Documents/TrainDigitX.csv",header=F) #Digit Data
  trainY <- read.csv("/Users/Documents/TrainDigitY.csv",header=F) #Digit Label
  trainE <- matrix(0,dim(trainY),10); for(e in 1:dim(trainY)[1]){trainE[e,trainY[e,1]+1]<-1}
  trainE <- cbind(trainE[,2:10],trainE[,1]) #Digit Label Standard Unit Basis Vector
  
  #Total Training & Holdout Data
  Ntrain <- nrow(trainX) #Total Training Data + Holdout Data
  ntrain <- floor(Ntrain*.8) #Total Training Data

  #Setup Test Data
  testX <- read.csv("/Users/Documents/TestDigitX.csv",header=F) #Digit Data
  testY <- read.csv("/Users/Documents/TestDigitY.csv",header=F) #Digit Label
  testE <- matrix(0,dim(testY),10); for(e in 1:dim(testY)[1]){testE[e,testY[e,1]+1]<-1}
  testE <- cbind(testE[,2:10],testE[,1]) #Digit Label Standard Unit Basis Vector
  Ntest <- nrow(testX) #Total Testing Data

  #Input Parameters
  n.l1 <- n.hidden #Hidden layer neurons
  eta  <- n.eta #Learning rate
  epoch <- n.epoch #Epochs
  
  #Fixed Parameters
  n.l0 <- 785 #Input Layer + Bias neurons; 28x28+1
  n.l2 <- 10 #Output Layer neurons
  
  #List to Store Error & Weights by Epoch
  epoch.err  <- list()
  
  #Random Weights (Initial) for i=1, 1st epoch
  w.01 <- matrix(rnorm((n.l1)*(n.l0)),n.l1,n.l0) #Weights & Bias (n.hiddenx785)
  w.12 <- matrix(rnorm((n.l2)*(n.l1)),n.l2,n.l1) #Weights & Bias (10xn.hidden)
  
  #Epoch Loop
  for(k in 1:epoch){
    
	#Training Data Loop
    for(i in 1:ntrain){
      #Feedforward
      x.0 <- c(as.numeric(trainX[i,]),1) #Input with bias (785x1)
      x.1 <- sig(w.01%*%x.0) #Output with bias (n.hiddenx1)
      x.2 <- softmax(w.12%*%x.1) #Output; 10x1
      
      #Backpropagation
      #Update Delta
      d.2 <- (x.2-trainE[i,]) #10x1
      d.1 <- x.1*(1-x.1)*t(w.12)%*%d.2 #n.hiddenx1
      #Update Weights
      w.12 <- w.12-eta*d.2%*%t(x.1)
      w.01 <- w.01-eta*d.1%*%t(x.0)
      }
    
	#Test Holdout Data & Compute Holdout Error
    Hold.err <- vector()
    for(i in (ntrain+1):Ntrain){
      x.0 <- c(as.numeric(trainX[i,]),1)
      x.1 <- sig(w.01%*%x.0)
      x.2 <- softmax(w.12%*%x.1)
      Hold.err[i-ntrain] <- sum((x.2-trainE[i,])^2)
    } 
    #Store Holdout Error & Weight Matrices per Epoch
    epoch.err[[k]] <- list(Error=sum(Hold.err)/(Ntrain-ntrain),w.01=w.01,w.12=w.12)
  }
  
  #Best Epoch & Best Weights
  epoch.v <- vector(length=epoch)
  for(j in 1:epoch){epoch.v[j] <- epoch.err[[j]]}
  bestepoch <- which(epoch.v=min(epoch.v))
  bestw.01 <- epoch.err[[bestepoch]]$w.01
  bestw.12 <- epoch.err[[bestepoch]]$w.12
  
  #Test Data & Compute Test Data Error
  Test.err <- matrix(0,Ntest,5); colnames(Test.err) <- c("Y","Temp","Pred","Error","Bin.Error")
  for(i in 1:Ntest){
  x.0 <- c(as.numeric(testX[i,]),1)
  x.1 <- sig(bestw.01%*%x.0)
  x.2 <- softmax(bestw.12%*%x.1)
  Test.err[i,1] <- testY[i,1]
  Test.err[i,2] <- which(x.2==max(x.2))
  Test.err[i,3] <- if(Test.err[i,2]==10) {0} else{Test.err[i,2]}
  Test.err[i,4] <- sum((x.2-testE[i,])^2)
  Test.err[i,5] <- sign(abs(Test.err[i,1]-Test.err[i,3]))
  }

  #Output - Epoch Error, Test Error, Binary Error
  return(list(epoch.err=epoch.err,test.err=sum(Test.err[,4]/Ntest),bin.err=sum(Test.err[,5]/Ntest))
}
