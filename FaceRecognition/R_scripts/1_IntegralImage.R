#Viola-Jones Face Recognition - Integral Image
#Data Files: faces, backgrounds (Training Data JPG), portrait.jpg (Test Data)
#Packages: jpeg

iimage <- function(){
  
  #Training Image Paths
  face.dir <- '~/faces/'
  back.dir <- '~/background/'
  
  #Store Grayscale Converted Training Images (64x64 matrices)
  n <- c(0:1999); images.tr <- list(); backs.tr  <- list()
  for(i in n){
    image.path <- paste0(face.dir,'face',n[i+1],'.jpg')
    image.col <- readJPEG(image.path, native=FALSE)
    images.tr[[i+1]]   <- 0.2126*image.col[,,1] + 0.7152*image.col[,,2] + 0.0722*image.col[,,3]
    back.path <- paste0(back.dir,n[i+1],'.jpg')
    back.col <- readJPEG(back.path, native=FALSE)
    backs.tr[[i+1]]   <- 0.2126*back.col[,,1] + 0.7152*back.col[,,2] + 0.0722*back.col[,,3]
  }
  
  #Training Integral Images
  images.sum <- list(); backs.sum  <- list()
  for(i in n){
    im.temp <- matrix(0,64,64); bk.temp <- matrix(0,64,64)
    for(j in 1:64){
      for(k in 1:64){
        im.temp[j,k] <- sum(images.tr[[i+1]][1:j,1:k])
        bk.temp[j,k] <- sum(backs.tr[[i+1]][1:j,1:k])
      }
    }
    images.sum[[i+1]] <- im.temp
    backs.sum[[i+1]]  <- bk.temp
  }
  
  #Output - Integral Image Faces, Backgrounds, Test Image; Test Image Coordinates
  return(list(images.sum=images.sum,backs.sum=backs.sum))
}
