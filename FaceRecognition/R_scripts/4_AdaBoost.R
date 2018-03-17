#Viola-Jones Face Recognition - AdaBoost Strong Classifiers
#Inputs: face.mat (Faces Feature Values), back.mat (Backgrounds Feature Values), feat.n (Total Features)

boost <- function(face.mat,back.mat,feat.n){
  
  #Initialize Face & Background Classification per Feature
  para.list <- list()
  for(l in 1:feat.n){
    para.mat <- matrix(0,4000,4)
    para.mat[,1] <- c(face.mat[l,],back.mat[l,])
    para.mat[,2] <- c(rep(1,2000),rep(-1,2000))
    for(i in 1:4000){
      err <- sum(sign(abs(sign(1*(para.mat[,1]-para.mat[i,1]-.001))-para.mat[,2])))/4000
      para.mat[i,3] <- min(err,1-err)
      para.mat[i,4] <- sign(1-2*err)
    }
    para.list[[l]] <- para.mat
  }

  #Initialize Error Subset
  err.subset <- c(2001:4000)
  err.n <- length(err.subset)
  
  #Boosting Classifier List
  class.list <- list()
  
  #Initialize Boosting Loop
  b <- 1; str.err <- .5
  while(str.err>.005){

    #Intialize Feature Weights
    wait.mat <- matrix(0,2000+err.n,15)
    wait.mat[,1] <- rep(1/(2000+err.n),(2000+err.n))
    
    #Initialize Best Weak Classifier
    feat.all <- matrix(0,feat.n,4); feat.all[,1]<-c(1:feat.n); colnames(feat.all) <- c("feat#","thresh","error","parity")
    for(l in 1:feat.n){
      para.mat <- para.list[[l]][c(1:2000,err.subset),]
      best.l <- as.vector(t(para.mat[which(para.mat[,3]==min(para.mat[,3])),]))
      pred <- sign(abs(sign(best.l[4]*(para.mat[,1]-best.l[1] -.001))-para.mat[,2]))
      feat.all[l,2] <- best.l[1] + .001
      feat.all[l,3] <- t(wait.mat[,1])%*%pred
      feat.all[l,4] <- best.l[4]
    }
    h.t<- as.vector(t(feat.all[which(feat.all[,3]==min(feat.all[,3])),]))[1:4]
    class.best <- matrix(0,15,8); class.best[,1] <- c(1:15); colnames(class.best) <- c("t","h_t","alpha_t","Z_t","h_thresh","h_pol","h_err","Haar")
    class.best[1,2:7] <- c(h.t[1],.5*log((1-h.t[3])/h.t[3]),2*sqrt(h.t[3]*(1-h.t[3])),h.t[2],h.t[4],h.t[3])
    
    #Initialize Strong Classifier Loop
    t <- 2; nclass.err <- .5
    while(nclass.err<0.01){
      
      #Update Classifier Weights
      y.val <- c(rep(1,2000),rep(-1,err.n))
      f.val <- para.list[[class.best[t-1,2]]][c(1:2000,err.subset),1]
      wait.mat[,t] <- wait.mat[,(t-1)]*exp(-class.best[t-1,3]*y.val*sign(class.best[t-1,6]*(f.val-class.best[t-1,5])))/class.best[t-1,4]
      
      #AdaBoost Best Features
      feat.all <- matrix(0,feat.n,4); feat.all[,1]<-c(1:feat.n)
      for(l in 1:feat.n){
        para.mat <- para.list[[l]][c(1:2000,err.subset),]
        best.l <- as.vector(t(para.mat[which(para.mat[,3]==min(para.mat[,3])),]))
        pred <- sign(abs(sign(best.l[4]*(para.mat[,1]-best.l[1] -.001))-para.mat[,2]))
        feat.all[l,2] <- best.l[1] + .001
        feat.all[l,3] <- t(wait.mat[,t])%*%pred
        feat.all[l,4] <- best.l[4]
      }
      h.t<- as.vector(t(feat.all[which(feat.all[,3]==min(feat.all[,3])),]))[1:4]
      class.best <- matrix(0,15,8); class.best[,1] <- c(1:15)
      class.best[t,2:7] <- c(h.t[1],.5*log((1-h.t[3])/h.t[3]),2*sqrt(h.t[3]*(1-h.t[3])),h.t[2],h.t[4],h.t[3])
      
      #Converge until feature error improvement <1%
      nclass.err <- class.best[t,7] - class.best[t-1,7]
      t <- t+1
    }
    
    #Strong Classifier False-Positive Error
    t <- t-1 #Total Strong Classifier Features
    fval.mat <- matrix(0,2000+err.n,t); polar.mat <- matrix(0,2000+err.n,t); thresh.mat <- matrix(0,2000+err.n,t)
    for(i in 1:t){
      fval.mat[,i] <- para.list[[class.best[i,2]]][c(1:2000,err.subset),1]
      polar.mat[,i] <- class.best[i,6]
      thresh.mat[,i] <- class.best[i,5]
    }
    stump.val <- sign(polar.mat*(fval.mat-thresh.mat))
    f.best <- stump.val%*%class.best[1:t,3]
    y.samp <- c(rep(1,2000),rep(-1,err.n))
    f.temp <- cbind(y.samp,f.best,sign(f.best))
    f.thresh <- min(f.temp[which(f.temp[,1]==1&f.temp[,3]==-1),2])-.001
    f.temp2 <- cbind(f.temp,sign(abs(sign(f.temp[,2]-f.thresh)-f.temp[,1])))
    
    #Store Strong Classifier
    class.list[[b]] <- list(class.best=class.best[class.best$h_t>0,],f.thresh=f.thresh)
       
    #Converge until strong classifier error <.005
    err.subset <- which(f.temp2[,4]==1,arr.ind = TRUE)
    err.n <- length(err.subset)
    str.err <- length(err.subset)/(2000+err.n)
    b <- b+1
  }
  
  #Output - Strong Classifier List
  return(class.list)
}
