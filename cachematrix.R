## Matrix inversion is usually a costly computation and their may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(d){
        inv <<- NULL
        mtx <<- d
    }
    get <- function() mtx
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, then the cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)){
        return(i)
    }
    i <- solve(x$get(), ...)
    x$setinv(i)
    i        
}
