## Programming Assignment 2 - Grace Nabholz
## Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly (there are also alternatives 
## to matrix inversion that we will not discuss here). Your assignment 
## is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setmean <- function(inv) inverse <<- inv
    getmean <- function() inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The following function calculates the mean of the special "vector" 
## created with the above function. However, it first checks to see 
## if the mean has already been calculated. If so, it gets the mean 
## from the cache and skips the computation. Otherwise, it calculates 
## the mean of the data and sets the value of the mean in the cache 
## via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinv(inverse)
    inverse
}
