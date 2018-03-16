#Viola-Jones Face Recognition - Haar-Like Feature Table
#Inputs: images.sum, backs.sum, h1.scale (Haar 1 size), h2.scale (Haar 2 size), h3.scale (Haar 3 size), h4.scale (Haar 4 size)

haarfeat <- function(images.sum,backs.sum,h1.scale,h2.scale,h3.scale,h4.scale){
  
  #Haar 1 Feature Table (Rectangles & Coordinate Grid)
  #Rectangles
  h1.w <- as.vector(seq(h1.scale,32,h1.scale))
  h1.h <- as.vector(seq(h1.scale,32,h1.scale))
  h1.rec <- expand.grid(h1.w,h1.h)
  #Coordinate Grid
  h1xy.rect <- expand.grid(seq(1,64,2),seq(1,64,2),1:dim(h1.rec)[1])
  h1.N <- dim(h1xy.rect)[1]
  #Haar 1 Table
  h1.mat <- matrix(0,nrow=h1.N,ncol=8)
  colnames(h1.mat) <- c("x1a","y1a","x1b","y1b","x2a","y2a","x2b","y2b")
  for(i in 1:h1.N){
    h1.mat[i,1] <- h1xy.rect[i,1]
    h1.mat[i,2] <- h1xy.rect[i,2]
    h1.mat[i,3] <- h1xy.rect[i,1] + h1.rec[h1xy.rect[i,3],1]-1
    h1.mat[i,4] <- h1xy.rect[i,2] + h1.rec[h1xy.rect[i,3],2]-1
    h1.mat[i,5] <- h1xy.rect[i,1] + h1.rec[h1xy.rect[i,3],1]
    h1.mat[i,6] <- h1xy.rect[i,2]
    h1.mat[i,7] <- h1xy.rect[i,1] + 2*h1.rec[h1xy.rect[i,3],1]-1
    h1.mat[i,8] <- h1xy.rect[i,2] + h1.rec[h1xy.rect[i,3],2]-1
  }
  feattbl1 <- h1.mat[which(h1.mat[,1]!=1&h1.mat[,2]!=1&h1.mat[,7]<65&h1.mat[,8]<65),]
  
  #Haar 2 Feature Table (Rectangles & Coordinate Grid)
  #Rectangles
  h2.w <- as.vector(seq(h2.scale,32,h2.scale))
  h2.h <- as.vector(seq(h2.scale,32,h2.scale))
  h2.rec <- expand.grid(h2.w,h2.h)
  #Coordinate Grid
  h2xy.rect <- expand.grid(seq(1,64,2),seq(1,64,2),1:dim(h2.rec)[1])
  h2.N <- dim(h2xy.rect)[1]
  #Haar 2 Table
  h2.mat <- matrix(0,nrow=h2.N,ncol=8)
  colnames(h2.mat) <- c("x1a","y1a","x1b","y1b","x2a","y2a","x2b","y2b")
  for(i in 1:h2.N){
    h2.mat[i,1] <- h2xy.rect[i,1]
    h2.mat[i,2] <- h2xy.rect[i,2]
    h2.mat[i,3] <- h2xy.rect[i,1] + h2.rec[h2xy.rect[i,3],1]-1
    h2.mat[i,4] <- h2xy.rect[i,2] + h2.rec[h2xy.rect[i,3],2]-1
    h2.mat[i,5] <- h2xy.rect[i,1]
    h2.mat[i,6] <- h2xy.rect[i,2] + h2.rec[h2xy.rect[i,3],2]
    h2.mat[i,7] <- h2xy.rect[i,1] + h2.rec[h2xy.rect[i,3],1]-1
    h2.mat[i,8] <- h2xy.rect[i,2] + 2*h2.rec[h2xy.rect[i,3],2]-1
  }
  feattbl2 <- h2.mat[which(h2.mat[,1]!=1&h2.mat[,2]!=1&h2.mat[,7]<65&h2.mat[,8]<65),]
  
  #Haar 3 Feature Table (Rectangles & Coordinate Grid)
  #Rectangles
  h3.w <- as.vector(seq(h3.scale,22,h3.scale))
  h3.h <- as.vector(seq(h3.scale,32,h3.scale))
  h3.rec <- expand.grid(h3.w,h3.h)
  #Coordinate Grid
  h3xy.rect <- expand.grid(seq(1,64,2),seq(1,64,2),1:dim(h3.rec)[1])
  h3.N <- dim(h3xy.rect)[1]
  #Haar 3 Table
  h3.mat <- matrix(0,nrow=h3.N,ncol=12)
  colnames(h3.mat) <- c("x1a","y1a","x1b","y1b","x2a","y2a","x2b","y2b","x3a","y3a","x3b","y3b")
  for(i in 1:h3.N){
    h3.mat[i,1] <- h3xy.rect[i,1]
    h3.mat[i,2] <- h3xy.rect[i,2]
    h3.mat[i,3] <- h3xy.rect[i,1] + h3.rec[h3xy.rect[i,3],1]-1
    h3.mat[i,4] <- h3xy.rect[i,2] + h3.rec[h3xy.rect[i,3],2]-1
    h3.mat[i,5] <- h3xy.rect[i,1] + h3.rec[h3xy.rect[i,3],1]
    h3.mat[i,6] <- h3xy.rect[i,2]
    h3.mat[i,7] <- h3xy.rect[i,1] + 2*h3.rec[h3xy.rect[i,3],1]-1
    h3.mat[i,8] <- h3xy.rect[i,2] + h3.rec[h3xy.rect[i,3],2]-1
    h3.mat[i,9] <- h3xy.rect[i,1] + 2*h3.rec[h3xy.rect[i,3],1]
    h3.mat[i,10] <- h3xy.rect[i,2]
    h3.mat[i,11] <- h3xy.rect[i,1] + 3*h3.rec[h3xy.rect[i,3],1]-1
    h3.mat[i,12] <- h3xy.rect[i,2] + h3.rec[h3xy.rect[i,3],2]-1
  }
  feattbl3 <- h3.mat[which(h3.mat[,1]!=1&h3.mat[,2]!=1&h3.mat[,11]<65&h3.mat[,12]<65),]
  
  #Haar 4 Feature Table (Rectangles & Coordinate Grid)
  #Rectangles
  h4.w <- as.vector(seq(h4.scale,32,h4.scale))
  h4.h <- as.vector(seq(h4.scale,32,h4.scale))
  h4.rec <- expand.grid(h4.w,h4.h)
  #Coordinate Grid
  h4xy.rect <- expand.grid(seq(1,64,2),seq(1,64,2),1:dim(h4.rec)[1])
  h4.N <- dim(h4xy.rect)[1]
  #Haar 3 Table
  h4.mat <- matrix(0,nrow=h4.N,ncol=16)
  colnames(h4.mat) <- c("x1a","y1a","x1b","y1b","x2a","y2a","x2b","y2b","x3a","y3a","x3b","y3b","x4a","y4a","x4b","y4b")
  for(i in 1:h4.N){
    h4.mat[i,1] <- h4xy.rect[i,1]
    h4.mat[i,2] <- h4xy.rect[i,2]
    h4.mat[i,3] <- h4xy.rect[i,1] + h4.rec[h4xy.rect[i,3],1]-1
    h4.mat[i,4] <- h4xy.rect[i,2] + h4.rec[h4xy.rect[i,3],2]-1
    h4.mat[i,5] <- h4xy.rect[i,1] + h4.rec[h4xy.rect[i,3],1]
    h4.mat[i,6] <- h4xy.rect[i,2]
    h4.mat[i,7] <- h4xy.rect[i,1] + 2*h4.rec[h4xy.rect[i,3],1]-1
    h4.mat[i,8] <- h4xy.rect[i,2] + h4.rec[h4xy.rect[i,3],2]-1
    h4.mat[i,9] <- h4xy.rect[i,1]
    h4.mat[i,10] <- h4xy.rect[i,2] + h4.rec[h4xy.rect[i,3],2]
    h4.mat[i,11] <- h4xy.rect[i,1] + h4.rec[h4xy.rect[i,3],1]-1
    h4.mat[i,12] <- h4xy.rect[i,2] + 2*h4.rec[h4xy.rect[i,3],2]-1
    h4.mat[i,13] <- h4xy.rect[i,1] + h4.rec[h4xy.rect[i,3],1]
    h4.mat[i,14] <- h4xy.rect[i,2] + h4.rec[h4xy.rect[i,3],2]
    h4.mat[i,15] <- h4xy.rect[i,1] + 2*h4.rec[h4xy.rect[i,3],1]-1
    h4.mat[i,16] <- h4xy.rect[i,2] + 2*h4.rec[h4xy.rect[i,3],2]-1
  }
  feattbl4 <- h4.mat[which(h4.mat[,1]!=1&h4.mat[,2]!=1&h4.mat[,15]<65&h4.mat[,16]<65),]
    
  #Feature Table Full List
  feattbl <- list()
  n.1 <- dim(feattbl1)[1]; n.2 <- dim(feattbl2)[1]+n.1; n.3 <- dim(feattbl3)[1]+n.2; feat.n <- dim(feattbl4)[1]+n.3
  for(i in 1:n.1){feattbl[[i]] <- feattbl1[i,]}
  for(i in (n.1+1):(n.2)){feattbl[[i]] <- feattbl2[i-n.1,]}
  for(i in (n.2+1):(n.3)){feattbl[[i]] <- feattbl3[i-n.2,]}
  for(i in (n.3+1):feat.n){feattbl[[i]] <- feattbl4[i-n.3,]}
  
  #Feature Table Haar Assignments
  haar.mat <- c(rep(1,dim(feattbl1)[1]),rep(2,dim(feattbl2)[1]),rep(3,dim(feattbl3)[1]),rep(4,dim(feattbl4)[1]))
  
  #Stored Training Feature Values for Faces & Backgrounds
  face.mat <- matrix(0,feat.n,2000)
  back.mat <- matrix(0,feat.n,2000)
  for(l in 1:n.1){
    for(k in 1:2000){
      face.mat[l,k] <- feat.val1(images.sum[[k]],feattbl[[l]][1],feattbl[[l]][2],feattbl[[l]][3],feattbl[[l]][4],
                                 feattbl[[l]][5],feattbl[[l]][6],feattbl[[l]][7],feattbl[[l]][8])
      back.mat[l,k] <- feat.val1(backs.sum[[k]],feattbl[[l]][1],feattbl[[l]][2],feattbl[[l]][3],feattbl[[l]][4],
                                 feattbl[[l]][5],feattbl[[l]][6],feattbl[[l]][7],feattbl[[l]][8])
    }}
  for(l in (n.1+1):n.2){
    for(k in 1:2000){
      face.mat[l,k] <- feat.val2(images.sum[[k]],feattbl[[l]][1],feattbl[[l]][2],feattbl[[l]][3],feattbl[[l]][4],
                                 feattbl[[l]][5],feattbl[[l]][6],feattbl[[l]][7],feattbl[[l]][8])
      back.mat[l,k] <- feat.val2(backs.sum[[k]],feattbl[[l]][1],feattbl[[l]][2],feattbl[[l]][3],feattbl[[l]][4],
                                 feattbl[[l]][5],feattbl[[l]][6],feattbl[[l]][7],feattbl[[l]][8])
    }}
  for(l in (n.2+1):n.3){
    for(k in 1:2000){
      face.mat[l,k] <- feat.val3(images.sum[[k]],feattbl[[l]][1],feattbl[[l]][2],feattbl[[l]][3],feattbl[[l]][4],
                                 feattbl[[l]][5],feattbl[[l]][6],feattbl[[l]][7],feattbl[[l]][8],
                                 feattbl[[l]][9],feattbl[[l]][10],feattbl[[l]][11],feattbl[[l]][12])
      back.mat[l,k] <- feat.val3(backs.sum[[k]],feattbl[[l]][1],feattbl[[l]][2],feattbl[[l]][3],feattbl[[l]][4],
                                 feattbl[[l]][5],feattbl[[l]][6],feattbl[[l]][7],feattbl[[l]][8],
                                 feattbl[[l]][9],feattbl[[l]][10],feattbl[[l]][11],feattbl[[l]][12])
    }}
  for(l in (n.3+1):feat.n){
    for(k in 1:2000){
      face.mat[l,k] <- feat.val4(images.sum[[k]],feattbl[[l]][1],feattbl[[l]][2],feattbl[[l]][3],feattbl[[l]][4],
                                 feattbl[[l]][5],feattbl[[l]][6],feattbl[[l]][7],feattbl[[l]][8],
                                 feattbl[[l]][9],feattbl[[l]][10],feattbl[[l]][11],feattbl[[l]][12],
                                 feattbl[[l]][13],feattbl[[l]][14],feattbl[[l]][15],feattbl[[l]][16])
      back.mat[l,k] <- feat.val4(backs.sum[[k]],feattbl[[l]][1],feattbl[[l]][2],feattbl[[l]][3],feattbl[[l]][4],
                                 feattbl[[l]][5],feattbl[[l]][6],feattbl[[l]][7],feattbl[[l]][8],
                                 feattbl[[l]][9],feattbl[[l]][10],feattbl[[l]][11],feattbl[[l]][12],
                                 feattbl[[l]][13],feattbl[[l]][14],feattbl[[l]][15],feattbl[[l]][16])
    }}
  
  #Output - Feature Table, Training Feature Values Faces & Backgrounds, Haar Assignments
  return(list(feattbl=feattbl,face.mat=face.mat,back.mat=back.mat,haar.mat=haar.mat))
}
