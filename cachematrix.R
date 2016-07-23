## This file has two functions makeCacheMatrix and cacheSolve

## makeCacheMatrix: returns a list of functions that:
## (1) & (2) gets and sets the value of the matrix
## (3) & (4) gets and sets the value of its inverse.

## cacheSolve: computes and caches the inverse of the matrix.

## makeCacheMatrix takes a matrix as parameter and returns a list with four functions.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      
      get <- function() x
      
      set <- function(y) {
        x <<- y
        i <<- NULL
      }
      
      setInverse <- function(inverse) i <<- inverse
      
      getInverse <- function() i
      
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns a cached inverse matrix if it exists, else computes the inverse matrix and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  if (!is.null(inverse)) {
    print("Getting cached inverse")
    return(inverse)
  }
  
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  inverse
}
