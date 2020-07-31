border <- function(Mat){
  newmat <- matrix(0,nrow(Mat)+2,ncol(Mat)+2)
  newmat[2:(nrow(newmat)-1),2:(ncol(newmat)-1)] <- Mat
  return(newmat)
}

transform <- function(Mat){
  Asums <- matrix(0,nrow(Mat),ncol(Mat))
  newMat <- Mat
  for(r in 1:(nrow(border(Mat))-2)){
    for(c in 1:(ncol(border(Mat))-2)){
      Asums[r,c] <- sum(c(border(Mat)[(r):(r+2),(c):(c+2)])[-5])
      if (Mat[r,c]==0 & Asums[r,c] %in% c(3,6))
        newMat[r,c] <- 1
      if (Mat[r,c]==1 & Asums[r,c] %in% c(0,1,4,5,7,8))
        newMat[r,c] <- 0
    }
  }
  return(newMat)
}


foo <- function(x,y,x1,y1,N){
  if (y >= 1000 || x > y){
    stop("y or x out of range")
    
  }
  yrows <- character()
  grid_initial <- matrix(0,y,x)
  for (i in 1:y){
    cat(c("fill",i,"line"))
    yrows[i] <- scan(what = character(),n=1)
    grid_initial[i,] <- as.integer(unlist(strsplit(yrows[i], "")))
  }
  if (!all(grid_initial %in% c(0,1))) stop("not 0 or 1")
  GenerationZero <- grid_initial
  Atrans <- list()
  Atrans[[1]] <- GenerationZero
  Atransx1y1 <- numeric()
  Atransx1y1[1] <- GenerationZero[y1+1,x1+1]
  for(i in 2:(N+1)){
    Atrans[[i]] <- transform(Atrans[[i-1]])
    Atransx1y1[i] <- Atrans[[i]][y1+1,x1+1]
  }
  return(cat("expected result: ",sum(Atransx1y1)))
}

foo(3,3,1,0,10)
foo(4,4,2,2,15)
