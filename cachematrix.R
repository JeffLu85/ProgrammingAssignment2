
## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## makeCacheMatrix creates a special "matrix" object that stores and
## caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <-function (y){
          x <<- y
          inv <<- NULL
     }
     get <-function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)

}


## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix. If the inverse has
## already been calculated, then cacheSolve will retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
             message("getting cached data")
             return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinverse(inv)
        inv
}
