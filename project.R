# -------------------------------------------------------------------------
#might be helpful
rm(list = ls())
# -------------------------------------------------------------------------
#Cached Inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- 1
  inverse = NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- 1
    inverse = NULL
  }
  getMatrix <- function(){
    return(x)
  }
  setInverse <- function(inv){
    m <<- 0
    inverse <<- inv
  }
  getInverse <- function(){
    return(inverse)
  }
  setM <- function(m){
    m=m
  }
  getM <- function(){
    return(m)
  }
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse,
       getM = getM,
       setM = setM)
}
  

cacheSolve <- function(x) {
  m <- x$getM()
  if (m==0) {
    message("getting cached data")
    inverse = x$getInverse()
    return(inverse)
  }else{
    data <- x$getMatrix()
    inv <- solve(data)
    x$setInverse(inv)
    return(inv)
  }
}

#Test Samples
y = matrix(c(1,1,3,5,2,1,-1,0,-2),nrow = 3,byrow = T)
z <- makeCacheMatrix(y)
cacheSolve(z)
cacheSolve(z)
f = matrix(c(5,4,9,2,18,5,334,7,5),nrow=3,byrow = T)
z$setMatrix(f)
cacheSolve(z)
cacheSolve(z)
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
#Mode Calculation

my_mode <- function(x = numeric()){
  z=t(matrix(unique(c(x))))
  n=length(z)
  z=rbind(z,c(rep(0,n)))
  for(i in 1:n) {
    repeatition = sum(which(x == z[1,i])/which(x == z[1,i]))
    z[2,i]=repeatition
  }
  max = max(z[2,])
  m=sum(which(z[2,]==max)/which(z[2,]==max))
  if(m==1){
    mode=z[1,which(z[2,]==max)]
    cat("[1]",mode,"unimodal","\n")
  }else if(m==2){
    mode=z[1,which(z[2,]==max)]
    cat("[1]",mode,"bimodal","\n")
  }else{
    print("no mode")
  }
}

#Test Samples
g = c(13, 18, 13, 14, 13, 16, 14, 21, 13)
my_mode(g)
h = c(8, 18, 13, 14, 4, 16, 14, 21, 8)
my_mode(h)
i = c(3, 5, 13, 5, 13, 3)
my_mode(i)


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
#Tests on mtcars data-frame
#1) Manual vs auto

mpg_am_lm = lm(mtcars$mpg~mtcars$am,data = mtcars)
summary(mpg_am_lm)
plot(mpg_am_lm)

# mpg = 17.147+7.245am
# since that the coef of am is a positive number then manual car(am=1)
# has a higher mpg than auto(am=0)
# -------------------------------------------------------------------------
#2) stepwise regression on mtcars

library(MASS)
mpg_._lm = lm(mtcars$mpg~.,data = mtcars)
stepWiseModel = stepAIC(mpg_._lm,direction = "both",trace = F)
summary(stepWiseModel)
plot(mpg_._lm)

# mpg = 9.6178-3.9165wt+1.2259qsec+2.9358am
# as result of the stepwise regression model we can say that
# mpg depends on car weight(wt), time per 0.25 mile(qsec)
# and transmission(am) and independent from the other (cyl,disp,hp,...)
