## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  library(matlib)
  setI <- function(inv) I <<- inv
  getI <- function() I
  list(set = set, get = get,
       setI = setI,
       getI = getI)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated

cacheSolve <- function(x, ...) {
  i <- x$getI()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- inv(data, ...)
  x$setI(i)
  i
}
