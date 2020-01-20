# Quantitative Geography - 3 # 
# Instrucutor: Tzai-Hung Wen (NTU Geography) #
# source: Brunsdon and Comber(2015), Chapter 4 #

## 1. Conditional statement:If-then-else

tree.heights <- c(4.3,7.1,6.3,5.2,3.2)
tree.heights

if (tree.heights[1] < 6) { cat('Tree is small\n') } else { cat('Tree is large\n')}

for (i in 1:3) {
  if (tree.heights[i] < 6) { cat('Tree',i,'is small\n') }
  else { cat('Tree',i, 'is large\n')} }

assess.tree.height <- function(tree.list, thresh)
{ for (i in 1:length(tree.list))
{ if(tree.list[i] < thresh) {cat('Tree',i, ' is small\n')} else { 
  cat('Tree',i, ' is large\n')}
}
} 

assess.tree.height(tree.heights, 6)


## 2. Loops and Repetition

for (i in 1:5) {
  i.cubed <- i * i * i
  cat("The cube of",i, "is",i.cubed, "\n")}

for (val in seq(0,1,by=0.25)) {
  val.squared <- val * val
  cat("The square of",val, "is",val.squared, "\n")}

i <- 1; n <- 654 
repeat{
  i.squared <- i * i
  if (i.squared > n) break 
  i <- i + 1 }
cat("The first square number exceeding",n, "is ",i.squared, "\n")


## 3. Writing Functions

mean.rainfall <- function(rf)  
{ if (all(rf> 0))                 #open Function
{ mean.value <- mean(rf)          #open Consequent
cat("The mean is ",mean.value) 
} else                            #close Consequent
{ cat("Warning: Not all values are positive\n")  #open Alternative
}                                 #close Alternative
}                                 #close Function

mean.rainfall(c(8.5,9.3,6.5,9.3,9.4))  


mean.rainfall2 <- function(rf) { 
  if (all(rf> 0)) {
    return( mean(rf))} else { 
      return(NA)}
}

rainfall.data<-c(8.5,9.3,6.5,9.3,9.4)
mr <- mean.rainfall2(rainfall.data) 
mr

mean.rainfall3 <- function(rf) { 
  if (all(rf> 0)) {
    return(list(mr=mean(rf), rsd=sd(rf)))} else { 
      return(NA)}
}

rainfall.data<-c(8.5,9.3,6.5,9.3,9.4)
results<-mean.rainfall3(rainfall.data) 

results$mr
results$rsd


#Example--1
cube.root <- function(x) { 
  if (is.numeric(x)) {
    if (x >= 0) { result <- x^(1/3) } 
    else { result <- -(-x)^(1/3) } 
    return(result) }
  else {
    cat("WARNING: Input must be numerical, not character\n")
    return(NA)}
}

#Example--2 Greatest Common Divisor (GCD)

gcd <- function(a,b)
{ 
  divisor <- min(a,b) 
  dividend <- max(a,b) 
  repeat
  { remainder <- dividend %% divisor 
  dividend <- divisor
  divisor <- remainder
  if (remainder == 0) break
  }
  return(dividend)
}

gcd(6,15)


debug(gcd)
undebug(gcd)

## 4. Writing Functions for Spatial Data

rm(list = ls())
library(GISTools)
data(georgia)
ls()

georgia.polys[1]

plot(c(939200,1419420),c(905510,1405900),asp=1,type='n')
lapply(georgia.polys,polygon)
?lapply


## 4.1 automatically Choosing the Bounding Box

poly1 <- georgia.polys[[1]] 
min(poly1[,1])

most.eastern <- function(poly) {return(min(poly[,1]))}

most.eastern.list <- lapply(georgia.polys,most.eastern)

most.eastern.value <-min(unlist(most.eastern.list))


# Function definition
most.eastern.point <- function(polys) { 
  # Most eastern points
  most.eastern.list <- lapply(polys,
                              function(poly) return(min(poly[,1])))
  # Return the smallest
  return(min(unlist(most.eastern.list)))}

most.eastern.point(georgia.polys)

### 4.2 shaped maps

data(georgia)
names(georgia)
georgia$PctRural

classifier <- factor(ifelse(georgia$PctRural > 50, "rural","urban"))

fill.cols <- vector(mode="character", length=length(classifier))

fill.cols[classifier=="urban"] <- "yellow" 
fill.cols[classifier=="rural"] <- "darkgreen"

# NB. ew is east/west, ns is north/south
# apply functions to determine bounding coordinates 
ew <- c(most.eastern.point(georgia.polys),
        most.western.point(georgia.polys)) 
ns <- c(most.southern.point(georgia.polys),
        most.northern.point(georgia.polys)) 
# set the plot parameters
par(mar = c(0,0,0,0)) 
plot(ew,ns,asp=1, type='n',xlab='',ylab='',xaxt='n',yaxt='n',bty='n') 
mapply(polygon,georgia.polys,col=fill.cols)

## 5. ANSWERS TO SELF-TEST QUESTIONS

## Q1. 
cube.root.2 <- function(x) 
{ if (is.numeric(x)) 
{ result <- sign(x)*abs(x)^(1/3)
return(result)  
} else
{ cat("WARNING: Input must be numerical, not character\n") 
  return(NA) }
}

## Q2.
gcd <- function(a,b)
{
  divisor <- min(a,b) # line 1
  dividend <- max(a,b) # line 1
  repeat #line 5
  { remainder <- dividend %% divisor #line 2
  dividend <- divisor # line 3
  divisor <- remainder # line 4
  if (remainder == 0) break #line 6 
  }
  return(dividend)
}

## Q3 and Q4: In-class Task #1 and #2

## Q5.
draw.polys <- function(poly.list) 
{ plot(c(939200,1419420),c(905510,1405900),asp=1, type='n',xlab='',ylab='',xaxt='n',yaxt='n',bty='n')
  invisible(lapply(poly.list,polygon)) 
}

?invisible
# Test it
draw.polys(georgia.polys)   

## Q6.
# The function definitions 
most.western.point <- function(polys) {
  most.western.list <- lapply(georgia.polys, 
                              function(poly) return(max(poly[,1]))) 
  return(max(unlist(most.western.list)))} 
#
most.southern.point <- function(polys) {
  most.southern.list <- lapply(georgia.polys, 
                               function(poly) return(min(poly[,2]))) 
  return(min(unlist(most.southern.list)))} 
#
most.northern.point <- function(polys) {
  most.northern.list <- lapply(georgia.polys, 
                               function(poly) return(max(poly[,2]))) 
  return(max(unlist(most.northern.list)))} 
# Test the functions
c(most.eastern.point(georgia.polys), 
  most.western.point(georgia.polys))
c(most.southern.point(georgia.polys), 
  most.northern.point(georgia.polys))

## Q7.
# NB. ew = east/west ns=north/south
draw.polys <- function(poly.list) {  
  ew <- c(most.eastern.point(poly.list), 
          most.western.point(poly.list))
  ns <- c(most.southern.point(poly.list), 
          most.northern.point(poly.list))        
  plot(ew,ns,asp=1,
       type='n',xlab='',ylab='',xaxt='n',yaxt='n',bty='n')
  invisible(lapply(poly.list,polygon)) }
#
# Test it - it should look the same as before!
#
draw.polys(georgia.polys) 

# Q8.
hatch.densities <- vector(mode="numeric",length=length(georgia.polys))
hatch.densities[classifier=="urban"] <- 40
hatch.densities[classifier=="rural"] <- 0
# This assumes ew and ns were defined earlier
plot(ew,ns,asp=1, type='n',xlab='',ylab='',xaxt='n',yaxt='n',bty='n')
invisible(mapply(polygon,georgia.polys,density=hatch.densities))