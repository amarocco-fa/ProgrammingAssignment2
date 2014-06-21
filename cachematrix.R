## These functions will allows to cache the Inverse of a Matrix

## Create an R object used to cache inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  #Init
  inversedMatrix <- NULL
  #Set the original Matrix
  set <- function(y) {
    x <<- y
    inversedMatrix <<- NULL
  }
  #Get back the original Matrix
  get <- function() x
  #Set the Inverse Matrix (already computed)
  setInverse <- function(m) inversedMatrix <<- m
  #Get back the 'cached' Inverse Matrix
  getInverse <- function() inversedMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Look in the cache for the Inverse of 'x'
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    ## value is defined display a message and return the value
    message("getting cached data")
    return(inverse)
  }
  
  ## Result is not defined (not yet computed)
  computeAndCache(x,...)
}

## Process and cache the computation of The Inverse of the special "matrix
computeAndCache  <- function(x, ...) {
  # Get back the original Matrix
  data <- x$get()
  #Compute the Inverse of 'x'
  inverse <- solve(data, ...)
  #Cache the result of the computation
  x$setInverse(inverse)
  #Return the Inverse of 'x'
  inverse
}