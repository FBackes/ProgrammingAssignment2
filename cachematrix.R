## The functions makeCacheMatrix() and cacheSolve() implement a method to cache the inverse of a matrix. 
## A new matrix is created and initialized with makeCacheMatrix(). The function provides setter
## and getter methods for the matrix and its inverse respectively.
##
## The function cacheSolve() calculates and caches the inverse of a matrix created with makeCacheMatrix().
## On consecutive calls of cacheSolve() with the same matrix variable the inverse is taken from the cache,
## which can save time compared to a re-calculation.

## Function: makeCacheMatrix()
## Arguments: A matrix. If the inverse should be calculated, a numeric, inversible matrix
##
## Creates a new matrix with a cache for its inverse. Provides setter and getter methods for the matrix and the inverse.
## The inverse in not calculated. This must be done explicitly with the function cacheSolve.
## Caution: To guarantee consistency, the methods $set() and $setsolve() should not be called directly.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(slv) s <<- slv
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## Function: makeSolve()
## Arguments: A matrix created with makeCacheMatrix(), optional arguments for the solve function
##
## Provides the inverse of a matrix created with makeCacheMatrix().
## If the inverse has already been calculated for the matrix, it is read from the cache.
## Otherwise the inverse is calculated by calling solve() and cached afterwards.
		
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
