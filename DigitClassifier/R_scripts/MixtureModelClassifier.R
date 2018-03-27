#Mixture Model Digit Classifier
#Inputs: mixtures (Total Mixtures)
#Data Files: digits.rdata (Training & Test Data)

Digit.MM <- function(mixtures){
  
  #Setup Training Data
  trainX <- read.csv("/Users/Documents/TrainDigitX.csv",header=F) #Digit Data
  trainY <- read.csv("/Users/Documents/TrainDigitY.csv",header=F) #Digit Label
  ntrain <- nrow(trainX) #Total Training Data
  nclass <- 0:9dim(training.data)[1] #Total Number Classes
  d <- ncol(trainX) #Total Data Dimensions
    
  #Setup Test Data
  testX <- read.csv("/Users/Documents/TestDigitX.csv",header=F) #Digit Data
  testY <- read.csv("/Users/Documents/TestDigitY.csv",header=F) #Digit Label
  ntest <- nrow(testX) #Total Test Data
  
  #Initial Parameters & Model List
  M <- mixtures; N <- ntrain; Mod.list <- list()
  
  #Training Each Digit
  for(digit in 1:nclass){
    if(digit==10){digit.train <- trainX[which(trainY==0),]} else {digit.train <- trainX[which(trainY==digit),]}
    
    #Initial BMM Parameters, Cluster (mixture model), Log-Likelihood Table
    clu.0 <- sample(M,N,replace=TRUE)
    iter <- matrix(c(0,0),1,2)
    colnames(iter) <- c("Iteration","Log-Lik")
    pi.mat <- matrix(0,M,1)
    mu.mat <- matrix(0,M,d)
    for(m in 1:M){
      pi.mat[m,1] <- (length(clu.0[which(clu.0==m)])+1)/(N+M)
      mu.mat[m,] <- (colSums(digit.train[which(clu.0==m),])+1)/(dim(digit.train[which(clu.0==m),])[1]+2)
    }
    
    #E-Step Initial
    gam.mat <- matrix(0,N,M)
    gam.tmp <- matrix(0,N,M)
    gam.exp <- matrix(0,N,M)
    for(i in 1:N){
      #Calculate Bernoulli P(X_i|mu_m)
      for(m in 1:M){
        pX.im <- vector()
        for(j in 1:d){pX.im[j] <- (mu.mat[m,j])^(digit.train[i,j]*1)*(1-mu.mat[m,j])^(1-digit.train[i,j]*1)}
        gam.tmp[i,m] <- log(pi.mat[m]*prod(pX.im))
      }
      
      #Find the max pi_m*P(X_i|mu_m) and calculate the new gamma probability
      l.max <- max(gam.tmp[i,])
      for(m in 1:M){gam.exp[i,m] <- exp(gam.tmp[i,m]-l.max)}
      for(m in 1:M){gam.mat[i,] <- gam.exp[i,]/sum(gam.exp[i,])}
    }
    
    #Log-Likelihood (Initial)
    Q.mat <- matrix(0,N,M)
    for(i in 1:N){
      for(m in 1:M){
        Q.mat[i,m] <- gam.mat[i,m]*(log(pi.mat[m])+t(digit.train[i,])%*%log(mu.mat[m,])+t(1-digit.train[i,])%*%log(1-mu.mat[m,]))
      }
    }
    iter[1,2]<-sum(Q.mat)+sum(log(pi.mat))+log(factorial(2*M-1))+sum(log(mu.mat[m,]))+sum(log(1-mu.mat[m,]))+400*log(6)
    
    #Loop until Log-Likelihood Difference <=1
    converge <- 500
    while(converge>1){
      
      #M-Step Loop
      pi.mat <- matrix(0,M,1)
      mu.mat <- matrix(0,M,d)
      for(m in 1:M){
        pi.mat[m,1] <- (sum(gam.mat[,m])+1)/(N+M)
        mu.mat[m,] <- (t(digit.train)%*%gam.mat[,m]+1)/(sum(gam.mat[,m])+2)
      }
      
      #E-Step Loop
      gam.mat <- matrix(0,N,M)
      gam.tmp <- matrix(0,N,M)
      gam.exp <- matrix(0,N,M)
      for(i in 1:N){
        #Calculate Bernoulli P(X_i|mu_m)
        for(m in 1:M){
          pX.im <- vector()
          for(j in 1:d){pX.im[j] <- (mu.mat[m,j])^(digit.train[i,j]*1)*(1-mu.mat[m,j])^(1-digit.train[i,j]*1)}
          gam.tmp[i,m] <- log(pi.mat[m]*prod(pX.im))
        }
        
        #Find the max pi_m*P(X_i|mu_m) and calculate the new gamma probability
        l.max <- max(gam.tmp[i,])
        for(m in 1:M){gam.exp[i,m] <- exp(gam.tmp[i,m]-l.max)}
        for(m in 1:M){gam.mat[i,] <- gam.exp[i,]/sum(gam.exp[i,])}
      }
      
      #Log-Likelihood
      Q.mat <- matrix(0,N,M)
      for(i in 1:N){
        for(m in 1:M){
          Q.mat[i,m] <- gam.mat[i,m]*(log(pi.mat[m])+t(digit.train[i,])%*%log(mu.mat[m,])+t(1-digit.train[i,])%*%log(1-mu.mat[m,]))
        }
      }
      
      iter <- rbind(iter,c(max(iter[,1]+1),sum(Q.mat)+sum(log(pi.mat))+log(factorial(2*M-1))+sum(log(mu.mat[m,]))+sum(log(1-mu.mat[m,]))+400*log(6)))
      
      #Converges when Log-Likelihood Difference <1
      converge <- iter[max(iter[,1])+1,2]-iter[max(iter[,1]),2]
    }
    #Model Output - Iterations, Pi, Mu, Gamma Parameters
    Mod.list[[digit]] <- list(Iter=iter,Pi=pi.mat,Mu=mu.mat,Gamma=gam.mat)
  }
  
  #Test Data Classification
  Test.mat <- matrix(0,ntest*10,12)
  colnames(Test.mat) <- c("1","2","3","4","5","6","7","8","9","0","True","Test")
  Test.mat[,11] <- testY
  for(i in 1:(ntest*10)){
    Digit.mat <- matrix(0,10,5)
    for(m in 1:5){
      pX.1 <- vector(); pX.2 <- vector(); pX.3 <- vector(); pX.4 <- vector(); pX.5 <- vector()
      pX.6 <- vector(); pX.7 <- vector(); pX.8 <- vector(); pX.9 <- vector(); pX.0 <- vector()
      for(j in 1:d){
        pX.1[j] <- Mod.list[[1]]$Mu[m,j]^(testX[i,j])*(1-Mod.list[[1]]$Mu[m,j])^(1-testX[i,j])
        pX.2[j] <- Mod.list[[2]]$Mu[m,j]^(testX[i,j])*(1-Mod.list[[2]]$Mu[m,j])^(1-testX[i,j])
        pX.3[j] <- Mod.list[[3]]$Mu[m,j]^(testX[i,j])*(1-Mod.list[[3]]$Mu[m,j])^(1-testX[i,j])
        pX.4[j] <- Mod.list[[4]]$Mu[m,j]^(testX[i,j])*(1-Mod.list[[4]]$Mu[m,j])^(1-testX[i,j])
        pX.5[j] <- Mod.list[[5]]$Mu[m,j]^(testX[i,j])*(1-Mod.list[[5]]$Mu[m,j])^(1-testX[i,j])
        pX.6[j] <- Mod.list[[6]]$Mu[m,j]^(testX[i,j])*(1-Mod.list[[6]]$Mu[m,j])^(1-testX[i,j])
        pX.7[j] <- Mod.list[[7]]$Mu[m,j]^(testX[i,j])*(1-Mod.list[[7]]$Mu[m,j])^(1-testX[i,j])
        pX.8[j] <- Mod.list[[8]]$Mu[m,j]^(testX[i,j])*(1-Mod.list[[8]]$Mu[m,j])^(1-testX[i,j])
        pX.9[j] <- Mod.list[[9]]$Mu[m,j]^(testX[i,j])*(1-Mod.list[[9]]$Mu[m,j])^(1-testX[i,j])
        pX.0[j] <- Mod.list[[10]]$Mu[m,j]^(testX[i,j])*(1-Mod.list[[10]]$Mu[m,j])^(1-testX[i,j])
      }
      Digit.mat[1,m] <- Mod.list[[1]]$Pi[m]*prod(pX.1)
      Digit.mat[2,m] <- Mod.list[[2]]$Pi[m]*prod(pX.2)
      Digit.mat[3,m] <- Mod.list[[3]]$Pi[m]*prod(pX.3)
      Digit.mat[4,m] <- Mod.list[[4]]$Pi[m]*prod(pX.4)
      Digit.mat[5,m] <- Mod.list[[5]]$Pi[m]*prod(pX.5)
      Digit.mat[6,m] <- Mod.list[[6]]$Pi[m]*prod(pX.6)
      Digit.mat[7,m] <- Mod.list[[7]]$Pi[m]*prod(pX.7)
      Digit.mat[8,m] <- Mod.list[[8]]$Pi[m]*prod(pX.8)
      Digit.mat[9,m] <- Mod.list[[9]]$Pi[m]*prod(pX.9)
      Digit.mat[10,m] <- Mod.list[[10]]$Pi[m]*prod(pX.0)
    }
    Test.mat[i,1] <- sum(Digit.mat[1,])
    Test.mat[i,2] <- sum(Digit.mat[2,])
    Test.mat[i,3] <- sum(Digit.mat[3,])
    Test.mat[i,4] <- sum(Digit.mat[4,])
    Test.mat[i,5] <- sum(Digit.mat[5,])
    Test.mat[i,6] <- sum(Digit.mat[6,])
    Test.mat[i,7] <- sum(Digit.mat[7,])
    Test.mat[i,8] <- sum(Digit.mat[8,])
    Test.mat[i,9] <- sum(Digit.mat[9,])
    Test.mat[i,10] <- sum(Digit.mat[10,])
    Test.mat[i,12] <- which(Test.mat[i,1:10]==max(Test.mat[i,1:10]))
  }
  
  #Classification Error Rate
  Test.mat <- cbind(Test.mat,abs(sign(Test.mat[,11]-Test.mat[,12])))
  Error.Rate <- sum(Test.mat[,13])/(ntest*10)
  
  #Output - Model List, Overall Error Rate
  return(list(Mod.list=Mod.list,Error.Rate=Error.Rate))
}
