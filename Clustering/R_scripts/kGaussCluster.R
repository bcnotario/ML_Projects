#k-Gaussians Mixture Clustering
#Inputs: data (n-by-d Dimensional Data), k (Total Clusters)

kgauss <- function(data,k){
  
  #Total Points & Dimension  
  n <- length(data[,1])
  d <- length(data[1,])
  
  #Inital k-Gaussians Parameters (randomly determined)
  clu <- cbind(data,sample(k,n,replace=TRUE))
  pie <- vector("list",k)
  mu <- vector("list",k)
  cov <- vector("list",k)
  for(i in 1:k){
    pie[[i]]<-length(clu[which(clu[,d+1]==i),1])/n
    mu[[i]]<-colMeans(clu[which(clu[,d+1]==i),])[1:d]
    cov[[i]]<-cov(clu[which(clu[,3]==i),1:d])
  }
  
  #E-Step Initial (p_i,j)
  pij.sum <- matrix(0,n,k+1)
  for(i in 1:n){
    for(j in 1:k){
      pij.sum[i,j] <- pie[[j]]*(((2*pi)^(-d/2)*det(cov[[j]])^-.5)*exp(-.5*as.matrix(data[i,]-mu[[j]])%*%
                      solve(cov[[j]])%*%t(as.matrix(data[i,]-mu[[j]]))))
    }
    pij.sum[i,4] <- sum(pij.sum[i,1:3])
  }
  pij.mat <- matrix(0,n,k)
  for(i in 1:n){
    for(j in 1:k){
      pij.mat[i,j] <- pij.sum[i,j]/pij.sum[i,4]
    }
  }
  
  converge <- 500
  iter.mat <- matrix(0,1,1)
  clu.prior <- matrix(0,n,1)
  while(converge>1){
    
    #M-Step Loop (pi_j, mu_j, sigma_j updates)
    pie.mat <- matrix(0,k,1)
    for(j in 1:k){
      pie.mat[j,1] <- sum(pij.mat[,j])/n
    }
    
    mu.mat <- matrix(0,k,d)
    for(j in 1:k){
      for(h in 1:d){
        mu.mat[j,h] <- t(pij.mat[,j]%*%data[,d])/sum(pij.mat[,j])
      } 
    }
    
    cov.list <- vector("list",k)      
    cov.temp <- vector("list",n)
    for(j in 1:k){
      for(i in 1:n){
        cov.temp[[i]] <- pij.mat[i,j]*t(as.matrix(data[i,]-mu.mat[j,]))%*%as.matrix(data[i,]-mu.mat[j,]) 
      }
      cov.list[[j]] <- Reduce('+',cov.temp)/sum(pij.mat[,j])
    }
    
    #E-Step Loop (p_i,j updates)
    pij.sum <- matrix(0,n,k+1)
    for(i in 1:n){
      for(j in 1:k){
        pij.sum[i,j] <- pie.mat[j,1]*(((2*pi)^(-d/2)*det(cov.list[[j]])^-.5)*exp(-.5*as.matrix(data[i,]-mu.mat[j,])%*%
                        solve(cov.list[[j]])%*%t(as.matrix(data[i,]-mu.mat[j,]))))
      }
      pij.sum[i,4] <- sum(pij.sum[i,1:3])
    }
    pij.mat <- matrix(0,n,k)
    for(i in 1:n){
      for(j in 1:k){
        pij.mat[i,j] <- pij.sum[i,j]/pij.sum[i,4]
      }
    }
    
    Cluster <- cbind(data,matrix(0,n,1))
    for(i in 1:n){
      if(max(pij.mat[i,])==pij.mat[i,1]){Cluster[i,3]<-1}
      if(max(pij.mat[i,])==pij.mat[i,2]){Cluster[i,3]<-2}
      if(max(pij.mat[i,])==pij.mat[i,3]){Cluster[i,3]<-3}
    } 
    
    #Converges when cluster assignments same as previous iteration
    iter.mat <- rbind(iter.mat,c(max(iter.mat[,1]+1)))
    converge <- sum(abs(Cluster[,3]-clu.prior))
    clu.prior <- Cluster[,3]
  }
  
  #Output of Cluster Assignments, Iterations
  return(list(mat.final=Cluster,iter.mat=max(iter.mat))) 
}
