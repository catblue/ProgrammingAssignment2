## function makeCacheMatrix() creates a special "matrix" object that can cache its inverse.
#  function cacheSolve() computes the inverse of the "matrix" created with makeCacheMatrix(). 
#  If the inverse has already been calculated, then cacheSolve() should retrieve it from the cache.

## makeCacheMatrix() returns a list of four functions; two for setting and two for geting 
#  matrix and its invertion
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  setmtrx <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  getmtrx <- function(){
    return(x)
  }
  
  setinv <- function(y){
    inv <<- y
    return(y)
  }
  
  getinv <- function(){
    return(inv)
  }
  
  return(list(getmtrx=getmtrx, setinv=setinv, getinv=getinv))#, setmtrx=setmtrx)) 
}


## cacheSolve() retrieves inverted matrix froim the cashe if necessary
# using functions passed through an argument

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv <- x$getinv()
  if(!is.null(inv)) {
    return(inv)
  }
  mtrx <- x$getmtrx()
  inv <- solve(mtrx)
  x$setinv(inv)
  return(inv)
}

## crude testing with generated array y1
# by comparing inverting with cashe

test <- function(z = matrix()) {
  zcpy <- matrix(z, dim(z))
  #print (zcpy)
  zc <- makeCacheMatrix(z)
  for (i in 1:1000) {
    cacheSolve(zc)
    #solve(zcpy)
  }
  all(((cacheSolve(zc)) == solve(zcpy))==TRUE)
}
y1 <- matrix(sample(seq(-.02,1.0,length=125^2)),125,125)
test(y1)