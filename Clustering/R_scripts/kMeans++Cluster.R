#k-Means++ Clustering
#Inputs: data (n-by-d Dimensional Data), k (Total Clusters)

kmeans.pp <- function(data,k){
  
  #Total Points & Dimension 
  n <- length(data[,1])
  d <- length(data[1,])
  
  #Inital Centroids (++ Randomly Determined)
  cent.mat <- matrix(0,n,3); cent.mat[,1] <- 1:n; colnames(cent.mat) <- c("Point","Dist","Prob")
  cent <- x[sample(n,1),]
  while(length(cent[,1])<k){
    for(h in 1:n){
      min.dist <- numeric()
      for(l in 1:length(cent[,1])){
        min.dist<-rbind(min.dist,sum((x[1,]-cent[l,])^2))
      }
      cent.mat[h,2] <- min(min.dist)
      cent.mat[h,3] <- cent.mat[h,2]/sum(cent.mat[,2])
    }
    cent <- rbind(cent,x[sample(cent.mat[,1],size=1,replace=FALSE,prob=cent.mat[,3]),])
  }
  
  #Calculation, Cluster, Iteration Matrices
  calc.mat <- matrix(0,nrow=n,ncol=3); calc.mat[,1] <- 1:n; colnames(calc.mat) <- c("Point","Cluster","Min.Dist")
  calc.prior <- matrix(1,nrow=n,ncol=1)
  clu.mat <- matrix(0,nrow=k,ncol=2); clu.mat[,1] <- 1:k; colnames(clu.mat) <- c("Cluster","EDistance")
  iter.mat <- matrix(c(0,0),1,2); colnames(iter.mat) <- c("Iteration","Distortion")
  
  converge <- 500  
  while(converge>1){
    
    #Cluster Assignments Loop
    for(i in 1:n){
      for(j in 1:k){
        clu.mat[j,2] <- sum((data[i,]-cent[j,])^2)
      }
      calc.mat[i,2]<-clu.mat[which(clu.mat[,2]==min(clu.mat[,2])),][1]
      calc.mat[i,3]<-clu.mat[which(clu.mat[,2]==min(clu.mat[,2])),][2]
    }
    
    #Distortion Update
    iter.mat <- rbind(iter.mat,c(max(iter.mat[,1]+1),sum(calc.mat[,3])))
    
    #Centroid Position Loop
    for(j in 1:k){
      index.j <- calc.mat[which(calc.mat[,2]==j),1]
      cent[j,]<-colMeans(data[index.j,])
    }
    
    #Converges when cluster assignments same as previous iteration
    converge <- sum(abs((calc.prior - calc.mat[,2])))
    calc.prior <- calc.mat[,2]
  }
  
  #Output of Cluster Assignments, Iterations, and Distortion
  return(list(mat.final=calc.mat,iter.mat=iter.mat[2:length(iter.mat[,1]),])) 
}
