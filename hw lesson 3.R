?rnorm
?runif
?sapply
?c
?round

#1
#amount of samples for the distribution
samples <- 1:10000

#function to round up normal distribution
rounded.up <- function(x,s){
  v <- rnorm(1,x,s)
  return(ceiling(v))
}

#checking function
rounded.up(10,3)

#2
project.length <- function(x){
  a.time <- rounded.up(10,3)
  b.time <- rounded.up(10,3)
  c.time <- 5
  d.time <- 10
  e.time <- rounded.up(5,2)
  f.time <- 4
  length.path.1 <- a.time+b.time+c.time+f.time
  length.path.2 <- a.time+d.time+e.time+f.time
  if(length.path.1 > length.path.2){
    project.time = length.path.1
    cp = 'a-b-c-f'
  } else{
    project.time = length.path.2
    cp = 'a-d-e-f'
  }
  return(c(project.time,cp))
}

#test run
project.length(1)


#3
project.time.vec <- sapply(samples,project.length)

results <- sapply(samples, project.length)
class(results)

results [1,1]
results [2,1]

plength <- function(column){
  return(as.numeric(column[1]))
}

ppath <- function(column){
  return(column[2])
}

project.length.vector <- apply(results,2,plength)
project.path.vector <- apply(results,2,ppath)


?hist
hist(project.time.vec)

#4


##################################
x <- rnorm(10,11,2)
x
y <- runif(10,2,9)
y

#sapply
v <- c(1,11,13,4,5)

cut5 <- function(x){
 if(x<5){
   return(x)
 } else{
  return(5)
 }
}

vmax5 <- sapply(v, cut5)
vmax5

##########################################
#monte carlo simulation

project.length <- function(x){
  a.time <- rnorm(1,10,2)
  b.time <- runif(1,5,13)
  c.time <- rnorm(1,16,3)
  if (a.time+b.time > c.time) {
    project.time = a.time+b.time
  } else {
    project.time = c.time
  }
  return(project.time)
}

project.length()

samples <- 1:10000

project.time.vec <- sapply(samples, project.length)

?hist
hist(project.time.vec)

###################################
#matrix

v1 <- c(1,2,3,4,5)
v2 <- c(5,6,7,8,9)

mat1 <- cbind(v1,v2)
mat1
mat2 <- rbind(v1,v2)
mat2

class(mat1)
typeof(mat1)

t(mat1)
t(mat2)

#apply function

mat <- matrix(rnorm(24,5,2),8,3)
#calculate mean of column
columns.means <- apply(mat, 2, mean)
rows.means <- apply(mat,1,mean)

cut5.all <- apply(mat, c(1,2),cut5)
