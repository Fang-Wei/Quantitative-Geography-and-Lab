#1
#(i)
gcd <- function(x){
  
  A <- c()
  
  for(i in 1:x) {
  divisor <- min(i,60)
  dividend <- max(i,60)
  repeat
    {remainder <- dividend %% divisor
     dividend <- divisor
     divisor <- remainder
     if (remainder == 0) break
    }
  A[i] <- dividend
  }
  return(A)
}

gcd(10)

#(ii)
gcd2 <- function(x,y){
  
  A <- matrix(ncol = y, nrow = x)
  
  for(i in 1:x) {
    for(j in 1:y) {
      divisor <- min(i,j)
      dividend <- max(i,j)
      repeat
      {remainder <- dividend %% divisor
      dividend <- divisor
      divisor <- remainder
      if (remainder == 0) break
      }
      A[i,j] <- dividend
    }
  }
  return(A)
}

gcd2(2,5)

#2
cube.root.2 <- function(n) {
  
  B <- c()
  k <- 1
  
  if (n>0) {
    for(i in seq(0.5,n,0.5)){
      B[k] <- sign(i)*abs(i)^(1/3)
      k <- k+1
    }
    return(B)
  }
  
  else {
    for(i in seq(n,0.5,0.5)){
      B[k] <- sign(i)*abs(i)^(1/3)
      k <- k+1
    }
    return(B)
  }
}

cube.root.2(10)
cube.root.2(-10)