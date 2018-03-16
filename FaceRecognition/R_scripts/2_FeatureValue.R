#Feature Value Functions
#Inputs: image, rectangle coordinates of feature (x1 to x8, y1 to y8)

#Haar-Like Feature Value 1
feat.val1 <- function(image,x1,y1,x2,y2,x3,y3,x4,y4){
  black <- image[x4,y4]+image[x3-1,y3-1]-image[x4,y3-1]-image[x3-1,y4]
  white <- image[x2,y2]+image[x1-1,y1-1]-image[x2,y1-1]-image[x1-1,y2]
  return(black-white)
}

#Haar-Like Feature Value 2
feat.val2 <- function(image,x1,y1,x2,y2,x3,y3,x4,y4){
  black <- image[x2,y2]+image[x1-1,y1-1]-image[x2,y1-1]-image[x1-1,y2]
  white <- image[x4,y4]+image[x3-1,y3-1]-image[x4,y3-1]-image[x3-1,y4]
  return(black-white)
}

#Haar-Like Feature Value 3
feat.val3 <- function(image,x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6){
  white1 <- image[x2,y2]+image[x1-1,y1-1]-image[x2,y1-1]-image[x1-1,y2]
  black1 <- image[x4,y4]+image[x3-1,y3-1]-image[x4,y3-1]-image[x3-1,y4]
  white2 <- image[x6,y6]+image[x5-1,y5-1]-image[x6,y5-1]-image[x5-1,y6]
  return(black1-white1-white2)
}

#Haar-Like Feature Value 4
feat.val4 <- function(image,x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7,x8,y8){
  white1 <- image[x2,y2]+image[x1-1,y1-1]-image[x2,y1-1]-image[x1-1,y2]
  black1 <- image[x4,y4]+image[x3-1,y3-1]-image[x4,y3-1]-image[x3-1,y4]
  white2 <- image[x6,y6]+image[x5-1,y5-1]-image[x6,y5-1]-image[x5-1,y6]
  black2 <- image[x8,y8]+image[x7-1,y7-1]-image[x8,y7-1]-image[x7-1,y8]
  return(black1+black2-white1-white2)
}
