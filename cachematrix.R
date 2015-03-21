## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      get <- function() x
      setinv <- function(inverse_matrix) inv <<- inverse_matrix
      getinv <- function() inv
      list(get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if (!is.null(inv)){
              message("getting cached inverse")  
              return(inv) 
      }
      
      mx <- x$get()
      inv <- solve(mx, ...)
      x$setinv(inv)    
      inv
}
