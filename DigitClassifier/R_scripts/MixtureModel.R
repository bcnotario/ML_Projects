#####README
#####SECTION1 - Training Data Set-Up
#####SECTION2 - Digit Mixture Model Function
#####SECTION3 - Digit 0-9; Mixture Models = {5}
#####SECTION4 - Digit 0-9 Classification Test

#####SECTION1 - Training & Test Data Set-Up
load("/Users/Chilipino/Documents/UChicago/2017 Q3 Spring/STAT 37500 - Pattern Recognition/Project/digits.rdata")
num.class <- dim(training.data)[1]  # Number of classes; 0-9 (true labels)
num.training <- dim(training.data)[2]  # Number of training data per class; 500 total
d <- prod(dim(training.data)[3:4])
dim(training.data) <- c(num.class * num.training, d) # Reshape training data to vector per i
training.label <- rep(0:9, num.training) # Labels of training data.
num.test <- dim(test.data)[2] # Number of test data; 1000 total
dim(test.data) <- c(num.class * num.test, d) # Reshape training data to vector per i
test.label <- rep(0:9, num.test) # Labels of test data

#####SECTION2 - Digit Mixture Model Function
#Input total mixture models and digit; output final pi_m, mu_mj, log-likelihood table
Digit.MM <- function(mixtures,number){
  #Initial Parameters
  M <- mixtures; digit <- number; N <- num.training
  digit.train <- training.data[which(training.label==digit),]
  
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
  return(list(Iter=iter,Pi=pi.mat,Mu=mu.mat,Gamma=gam.mat))
}

#####SECTION3 - Digit 0-9; Mixture Models = {5}; Training Time 984 seconds of 10K training images
Sys.time()
mod1.5 <- Digit.MM(5,1)
mod2.5 <- Digit.MM(5,2)
mod3.5 <- Digit.MM(5,3)
mod4.5 <- Digit.MM(5,4)
mod5.5 <- Digit.MM(5,5)
mod6.5 <- Digit.MM(5,6)
mod7.5 <- Digit.MM(5,7)
mod8.5 <- Digit.MM(5,8)
mod9.5 <- Digit.MM(5,9)
mod0.5 <- Digit.MM(5,0)
Sys.time()

#Digit 0-9; M = 5 Images
par(mfrow=c(2,3))
image(t(1 - matrix(mod1.5$Mu[1,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod1.5$Mu[2,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod1.5$Mu[3,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod1.5$Mu[4,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod1.5$Mu[5,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)

image(t(1 - matrix(mod2.5$Mu[1,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod2.5$Mu[2,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod2.5$Mu[3,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod2.5$Mu[4,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod2.5$Mu[5,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)

image(t(1 - matrix(mod3.5$Mu[1,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod3.5$Mu[2,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod3.5$Mu[3,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod3.5$Mu[4,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod3.5$Mu[5,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)

image(t(1 - matrix(mod4.5$Mu[1,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod4.5$Mu[2,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod4.5$Mu[3,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod4.5$Mu[4,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod4.5$Mu[5,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)

image(t(1 - matrix(mod5.5$Mu[1,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod5.5$Mu[2,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod5.5$Mu[3,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod5.5$Mu[4,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod5.5$Mu[5,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)

image(t(1 - matrix(mod6.5$Mu[1,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod6.5$Mu[2,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod6.5$Mu[3,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod6.5$Mu[4,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod6.5$Mu[5,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)

image(t(1 - matrix(mod7.5$Mu[1,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod7.5$Mu[2,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod7.5$Mu[3,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod7.5$Mu[4,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod7.5$Mu[5,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)

image(t(1 - matrix(mod8.5$Mu[1,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod8.5$Mu[2,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod8.5$Mu[3,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod8.5$Mu[4,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod8.5$Mu[5,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)

image(t(1 - matrix(mod9.5$Mu[1,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod9.5$Mu[2,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod9.5$Mu[3,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod9.5$Mu[4,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod9.5$Mu[5,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)

image(t(1 - matrix(mod0.5$Mu[1,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod0.5$Mu[2,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod0.5$Mu[3,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod0.5$Mu[4,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)
image(t(1 - matrix(mod0.5$Mu[5,],20,20))[,20:1], col=gray(seq(0, 1, length.out=256)),axes=FALSE, asp=1)

#####SECTION4 - Digit 3 & 5 Classification
#Testing Data
Test.mat <- matrix(0,num.test*10,12)
colnames(Test.mat) <- c("1","2","3","4","5","6","7","8","9","0","True","Test")
Test.mat[,11] <- rep(c(10,1:9),1000)

Sys.time()
for(i in 1:(num.test*10)){
  Digit.mat <- matrix(0,10,5)
  for(m in 1:5){
    pX.1 <- vector(); pX.2 <- vector(); pX.3 <- vector(); pX.4 <- vector(); pX.5 <- vector()
    pX.6 <- vector(); pX.7 <- vector(); pX.8 <- vector(); pX.9 <- vector(); pX.0 <- vector()
    for(j in 1:d){
      pX.1[j] <- mod1.5$Mu[m,j]^(test.data[i,j])*(1-mod1.5$Mu[m,j])^(1-test.data[i,j])
      pX.2[j] <- mod2.5$Mu[m,j]^(test.data[i,j])*(1-mod2.5$Mu[m,j])^(1-test.data[i,j])
      pX.3[j] <- mod3.5$Mu[m,j]^(test.data[i,j])*(1-mod3.5$Mu[m,j])^(1-test.data[i,j])
      pX.4[j] <- mod4.5$Mu[m,j]^(test.data[i,j])*(1-mod4.5$Mu[m,j])^(1-test.data[i,j])
      pX.5[j] <- mod5.5$Mu[m,j]^(test.data[i,j])*(1-mod5.5$Mu[m,j])^(1-test.data[i,j])
      pX.6[j] <- mod6.5$Mu[m,j]^(test.data[i,j])*(1-mod6.5$Mu[m,j])^(1-test.data[i,j])
      pX.7[j] <- mod7.5$Mu[m,j]^(test.data[i,j])*(1-mod7.5$Mu[m,j])^(1-test.data[i,j])
      pX.8[j] <- mod8.5$Mu[m,j]^(test.data[i,j])*(1-mod8.5$Mu[m,j])^(1-test.data[i,j])
      pX.9[j] <- mod9.5$Mu[m,j]^(test.data[i,j])*(1-mod9.5$Mu[m,j])^(1-test.data[i,j])
      pX.0[j] <- mod0.5$Mu[m,j]^(test.data[i,j])*(1-mod0.5$Mu[m,j])^(1-test.data[i,j])
      }
    Digit.mat[1,m] <- mod1.5$Pi[m]*prod(pX.1)
    Digit.mat[2,m] <- mod2.5$Pi[m]*prod(pX.2)
    Digit.mat[3,m] <- mod3.5$Pi[m]*prod(pX.3)
    Digit.mat[4,m] <- mod4.5$Pi[m]*prod(pX.4)
    Digit.mat[5,m] <- mod5.5$Pi[m]*prod(pX.5)
    Digit.mat[6,m] <- mod6.5$Pi[m]*prod(pX.6)
    Digit.mat[7,m] <- mod7.5$Pi[m]*prod(pX.7)
    Digit.mat[8,m] <- mod8.5$Pi[m]*prod(pX.8)
    Digit.mat[9,m] <- mod9.5$Pi[m]*prod(pX.9)
    Digit.mat[10,m] <- mod0.5$Pi[m]*prod(pX.0)
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
Sys.time()

#Classification Error Rate; 11.26% Error Rate
Test.mat <- cbind(Test.mat,abs(sign(Test.mat[,11]-Test.mat[,12])))
Error.Rate <- sum(Test.mat[,13])/(num.test*10)
