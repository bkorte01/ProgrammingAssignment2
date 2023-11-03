## This function pair caches the inverse of a matrix so that it does not have to
## be computated more than once.

## The makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse. The input is a square invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The cacheSolve function uses the solve function to compute the inverse of a 
## square matrix, which is provided by the makeCacheMatrix function. It will
## return the inverse matrix from the cache if it has already been solved.

cacheSolve <- function(x, ...) {
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}
