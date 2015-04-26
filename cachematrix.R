## The following functions allow calculating, caching and retreiving of inverse of matrix.
## This is usally a costly computational operation. 
## Therefore caching of the inversed matrix can speed up code, requiring such operation.

## makeCacheMatrix creates a list of functions to set and get matrix and it's inverse. 

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
      }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve returns the inverse of a matrix.
## It checks if the inversed matrix is already calculated and stored.
## If it is cached it is returned. If it is not calculated, the reversal is computed, stored and returned. 

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  matrix_data <- x$get()
  inv <- solve(matrix_data)
  x$setinverse(inv)
  inv
  
      
}
