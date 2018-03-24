#Viola-Jones Face Recognition - Classifier Cascade
#Inputs: class.list (Strong Classifier List), feattbl (Feature Table), haar.mat (Haar Assignments), scale (scan size)
#Data Files: portrait.jpg (Test Image)
#Functions: feat.val1, feat.val2, feat.val3, feat.val4

classcascade <- function(class.list,feattbl,haar.mat,scale){
  
  #Test Integral Image
  test.bw <- readJPEG('~/portrait.jpg',native=FALSE)
  test.sum <- matrix(0,1280,1600) #1280x1600 image dimensions
  for(k in 1:1600){
    test.sum[1,k] <- sum(test.bw[1:1,1:k]) 
    for(j in 2:1280){
      test.sum[j,k] <- test.sum[j-1,k] + sum(test.bw[j:j,1:k])
    }}
  
  #Test Image Coordinates Table
  seq.x <- seq(3,(1600-63),scale)
  seq.y <- seq(3,(1280-63),scale)
  xy.faces <- expand.grid(seq.x,seq.y); xy.faces <- cbind(xy.faces,0); colnames(xy.faces) <- c("x_port","y_port","+-1")
  
  #Classifier Cascade
  for(b in 1:length(class.list)){
    
    #Strong Classifier Temp
    class.best <- class.list[[b]]$class.best
    
    #Test for Faces in Successive Slide Subsets
    for(xy in 1:nrow(xy.faces)){
      
      #Test Slide
      slide.xy <- test.sum[xy.faces[xy,2]:(xy.faces[xy,2]+63),xy.faces[xy,1]:(xy.faces[xy,1]+63)]
      
      #Slide Feature Calculations
      slide.ft <- matrix(0,nrow(class.best),4); colnames(slide.ft) <- c("Feat#","Haar","Feature Value","Class Value")
      for(t in 1:nrow(class.best)){
        slide.ft[t,1] <- class.best[t,2] #Feature Number
        slide.ft[t,2] <- haar.mat[slide.ft[t,1]] #Haar Assignment
        slide.ft[t,3] <- do.call(paste0("feat.val",slide.ft[t,2]),feattbl[[slide.ft[t,1]]]) #Feature Value
        slide.ft[t,4] <- class.best[t,3]*sign(class.best[t,6]*(slide.ft[t,3]-class.best[t,5])) #Classifier Value
      }
      
      #Test Slide Classification
      xy.faces[xy,3] <- sign(sum(slide.ft[,4]) - class.list[[b]]$f.thresh)
    }
    
    #Test Image Filtered Faces
    xy.faces <- xy.faces[which(xy.faces[,3]==1),]
  }
  
  #Export Test Image with Faces
  jpeg("portrait_faces.jpg")
  plot(0:1600,0:1600, type='n')
  rasterImage(test.bw, 0, 0, 1600, 1280)
  for(i in 1:dim(xy.faces)[1]){
    rect(xy.faces[i,1],1280-xy.faces[i,2],xy.faces[i,1]+63,1280-xy.faces[i,2]-63,col="NA",border="green")}
  dev.off()
}
