# Quantitative Geography - 1 # 
# Instrucutor: Tzai-Hung Wen (NTU Geography) #
# source: Brunsdon and Comber(2015), Chapter 2 #

########## 1. Data Types ###########

character(8)
# conversion
as.character("8")
# tests
is.character(8) 
is.character("8")

numeric(8)
# conversions
as.numeric(c("1980","-8","Geography")) 
as.numeric(c(FALSE,TRUE))
# tests
is.numeric(c(8, 8))
is.numeric(c(8, 8, 8, "8"))

logical(7)
# conversion
as.logical(c(7, 5, 0, -4,5))
# TRUE and FALSE can be converted to 1 and 0
as.logical(c(7,5,0,-4,5)) * 1 

data <- c(3, 6, 9, 99, 54, 32, -102) 
# a logical test
index <- (data > 10)
index
# used to subset data
data[index]
sum(data)
sum(data[index])

########## 2. Data Classes ###########

# Vector and Matrix
# defining vectors
vector(mode = "numeric", length = 8)
vector(length = 8)
# testing and conversion
tmp <- data.frame(a=10:15, b=15:20) 
is.vector(tmp)
as.vector(tmp)
tmp[,1]

# defining matrices
matrix(ncol = 2, nrow = 0) 
matrix(1:6)
matrix(1:6, ncol = 2)
as.matrix(6:3)
is.matrix(as.matrix(6:3))

flow <- matrix(c(2000, 1243, 543, 1243, 212, 545, 654, 168, 109), c(3,3), byrow=TRUE)
# Rows and columns can have names, not just 1,2,3,...
colnames(flow) <- c("Leicester", "Liverpool"," Elsewhere") 
rownames(flow) <- c("Leicester", "Liverpool", "Elsewhere") 
# examine the matrix
flow
# and functions exist to summarise
outflows <- rowSums(flow) 
outflows

# a vector assignment
income <-factor(c("High", "High", "Low", "Low", "Low", "Medium", "Low", "Medium"), levels=c("Low", "Medium", "High"))
income > "Low"

income <-ordered (c("High", "High", "Low", "Low", "Low", "Medium", "Low", "Medium"), levels=c("Low", "Medium", "High"))
income > "Low"

# list and user-defined class <existing function>.<class>
employee <- list(name="Lex Comber", start.year = 2005, position="Professor")
class(employee) <- "staff"
print.staff <- function(x) {
  cat("Name: ",x$name, "\n")
  cat("Start Year: ",x$start.year, "\n") 
  cat("Job Title: ",x$position, "\n")}
print(employee)
print(unclass(employee))

########## 2. Data Classes (Self-test questions) ###########

colours <- factor(c("red","blue","red","white", "silver","red","white","silver", "red","red","white","silver","silver"), levels=c("red","blue","white","silver","black"))
colours2 <- factor(c("red","blue","red","white", "silver","red","white","silver", "red","red","white","silver"))
car.type <- factor(c("saloon","saloon","hatchback", "saloon","convertible","hatchback","convertible", "saloon", "hatchback","saloon", "saloon", "saloon", "hatchback"), levels=c("saloon","hatchback","convertible"))
engine <- ordered(c("1.1litre","1.3litre","1.1litre", "1.3litre","1.6litre","1.3litre","1.6litre", "1.1litre","1.3litre","1.1litre", "1.1litre", "1.3litre","1.3litre"), levels=c("1.1litre","1.3litre","1.6litre"))

cars=data.frame(col=colours,type=car.type,eng=engine)

# Self-test question.1
colours[4] <- "orange"

# Self-test question.2
table(colours); table(colours2)

# Self-test question.3
table(car.type, colours); table(colours, car.type)

# Self-test question.4 *important*
colours[engine > "1.1litre"]
table(car.type[engine < "1.6litre"])
table(colours[(engine >= "1.3litre") & (car.type == "hatchback")])

# Self-test question.5
example <- c(1.4,2.6,1.1,1.5,2.6) 
which.max(example)

# Self-test question.6
crosstab <- table(car.type,colours)
?apply
apply(crosstab,1,which.max)

# Self-test question.7 *important*
# Defines the function
which.max.name <- function(x) { 
  return(names(x)[which.max(x)])}

names(example) <- c("Leicester","Nottingham", "Loughborough","Birmingham","Coventry")
example
which.max.name(example)

apply(crosstab,1,which.max.name) # rows
apply(crosstab,2,which.max.name) # columns

# Self-test question.8 *important*
colour=apply(crosstab,1,which.max.name)
type=apply(crosstab,2,which.max.name)
most.popular <- list(colour, type) 
most.popular

# Self-test question.9
print.sales.data <- function(x) { 
  cat("Weekly Sales Data:\n") 
  cat("Most popular colour:\n") 
  for (i in 1:length(x$colour)) {
    cat(sprintf("%12s:%12s\n",names(x$colour)[i], 
                x$colour[i]))}
  cat("Most popular type:\n") 
  for (i in 1:length(x$type)) {
    cat(sprintf("%12s:%12s\n",names(x$type)[i],
                x$type[i]))}
  cat("Total Sold = ",x$total)
}

most.popular <- list(colour=colour, type=type, total=sum(crosstab)) 

print.sales.data(most.popular) 

########## 3. Plotting ###########

x <- seq(0,2*pi,len=100)
y_sin <- sin(x)
plot(x,y_sin,type='l')
plot(x,y_sin,type='l', lwd=3, col='darkgreen')
y2 <- y_sin + rnorm(100,0,0.1)
points(x,y2, pch=16, col='darkred')

y_cos <- cos(x)
par(mfrow = c(1,2))
plot(y_sin,y_cos)
polygon(y_sin,y_cos,col='lightgreen')

plot(y_sin,y_cos, asp=1, type='n') 
polygon(y_sin,y_cos,col='lightblue')

########## 4. Spatial Mapping ###########

library(GISTools)
data(georgia)
class(georgia)
plot(georgia, col=rgb(0,0.9,0.3))

appling <- georgia.polys[[1]]
par(mfrow = c(1,1))
plot(appling, asp=1, type='n', xlab="X_coor", ylab="Y_coor")
polygon(appling, col=rgb(0,0.5,0.7))
polygon(appling, col=rgb(0,0.5,0.7,0.4))

# set the plot extent
plot(appling, asp=1, type='n', xlab="X_coor", ylab="Y_coor")
# plot the points
points(x = runif(500,126,132)*10000,
       y = runif(500,103,108)*10000, pch=16, col='red') 
# plot the polygon with a transparency factor 
polygon(appling, col=rgb(0,0.5,0.7,0.4))

plot(appling, asp=1, type='n', xlab="X_coor", ylab="Y_coor")
polygon(appling, col=rgb(0,0.5,0.7,0.3))
locator()
# add text, sepcifying its placement, colour and size 
text(1287000,1053000, "Appling County",cex=1.5) 
text(1287000,1049000, "Georgia",col='darkred')

########## 5. Data Export and Import ###########

head(appling)
dim(appling)
colnames(appling) <- c("X", "Y")
setwd("D:/R_Labs/_QG2017")
write.csv(appling, file = "test.csv")
tmp.appling <- read.csv(file = "test.csv")

save(list = c("appling", "georgia.polys"), file = "MyData.RData")
load("MyData.RData")

writePolyShape(georgia, "georgia.shp")
new.georgia <- readShapePoly("georgia.shp")