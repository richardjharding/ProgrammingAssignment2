## This script contains two functions which when combined allow the
## inverse of a matrix to be calculated, the calculated inverse is cached
## so that subsequent calls simply return the previously calculated value

## Returns a vector of functions used to store a matrix and its
## inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverseResult) i <<- inverseResult
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Takes the vector created by makeCacheMatrix, calculates the inverse using the
## solve function and stores that in the vector by calling setSolve
## then returns the inverse, when called a second time for the same value
## the previously calculated value is returned

cacheSolve <- function(x, ...) {
    ## see if we have already calculated the inverse
    cachedInverse <- x$getInverse()
    if(!is.null(cachedInverse)){
      message("getting cached data")
      return(cachedInverse)
    }
    ## no cached result exists so calculate the inverse, store the result
    ## before returning it
    inputMatrix <- x$get()
    cachedInverse <- solve(inputMatrix,...)
    x$setInverse(cachedInverse)
    cachedInverse
}
