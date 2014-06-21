## This file provides two functions designed to reduce time consumption during computations.
## They provide a way to store and read inverse matrix for given matrix, allowing to avoid
## repeated time-consuming computations of inverse matrix.
## First function provides a cache mechanism for storing and reading matrix and its inverse.
## Second function provides implementation of inverse matrix calculation using cache.


## This function provides a wrapper for matrix and its inverse. It creates a list of functions 
## for setting/getting matrix and for setting/getting inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <- inverse
  getinv <- function() inv
  
  list(get = get,
       set = set,
       setinv = setinv,
       getinv = getinv)
}


## This function returns inverse matrix for given matrix 'x', performing calculations only 
## when necessary i.e. when matrix has no previously computed cached inverse and returning 
## pre-calculated inverse otherwise

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)){
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  mat$setinv(inv)
  inv
}
