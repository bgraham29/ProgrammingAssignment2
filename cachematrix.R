## makeCacheMatrix creates an inverse and caches it into variable M
## cacheSolve retrieves the inverse from the cache (if it exits) or
## computes the inverse if it is not yet cached.
## This code does not capture errors for non-square or singular matrices.

## Create a 'matrix' object that caches the inverse

makeCacheMatrix <- function(x = matrix()) {
        M <- NULL
        set <- function(y) {
                x <<- y
                M <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) M <<- solve
        getInverse <- function() M
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## Compute the inverse of the matrix returned by makeCacheMatrix() or 
## retrieve the inverse from the cache if it already exists.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        M <- x$getInverse()
        if(!is.null(M)) {
                message("getting cached data")
                return(M)
        }
        data <- x$get()
        M <- solve(data, ...)
        x$setInverse(M)
        M
}
