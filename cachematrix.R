## Put comments here that give an overall description of what your
## functions do

## Function that, given a initial matrix, will cache its inverse when calculated with cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(newInverse) inverse <<- newInverse
  getInverse <- function() inverse
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function that calculates the inverse of the "special" matrix created by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("Getting cached inverse")
    return(inverse)
  }
  message("No cached inverse found, calculating...")
  data <- x$getMatrix()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
