## The makeCacheMatrix and cacheSolve functions work together to
## create a matrix whose inverse can be cached and to calculate
## and cache that matrix's inverse. This eliminates the
## time-consuming process of calculating the same matrix's inverse
## multiple times.


## makeCacheMatrix creates a special "matrix" by using a list of four
## functions to set the value of the input matrix, get that value, set
## the value of the input matrix's inverse, and get that value.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve calculates the inverse of the "matrix" created with
## makeCacheMatrix if it has not already been calculated. If it has,
## cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}



