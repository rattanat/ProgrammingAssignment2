## The following two functions make it possible to cache 
## the inverse of a Matrix for later use so that we could
## save time since the value need not be computed repeatedly.

## The makeCacheMatrix function creates a special matrix object, 
## essentially a list containing the following functions.
## set: to set the value of the matrix
## get: to get the value of the matrix
## setinv: to set the value of the matrix's inverse
## getinv: to get the value of the matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
  invVal <- NULL
  set <- function(y) {
    x <<- y
    invVal <<- NULL
  }
  get <- function() x
  setinv <- function(inverseVal) invVal <<- inverseVal
  getinv <- function() invVal
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The cacheSolve function calculates the inverse of the special "matrix" object,
## created with the makeCacheMatrix function. 
## If the inverse has already been calculated, 
## it gets the inverse from the cache instead of recalculating the value.
## Otherwise, it calculates the inverse and sets that value in the cache 
## via the setinv function of the special matrix object.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invVal <- x$getinv()
  if(!is.null(invVal)) {
    message("Getting cached data")
    return(invVal)
  }
  data <- x$get()
  invVal <- solve(data, ...)
  x$setinv(invVal)
  invVal
}