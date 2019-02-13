## Two functions for caching a matrix inverse calculation
##
## Usage example:
## > amatrix <- makeCacheMatrix(matrix(rnorm(16),4,4))
## > cacheSolve(amatrix)
## Second call retrieves "amatrix" inverse from cache
## > cacheSolve(amatrix)   

## This function takes a matrix and returns a set of
## functions to cache the inverse calculation 

makeCacheMatrix <- function(x = matrix()) {
    # Variable to hold cached inverse
    inverse <- NULL
    # Set the underlying matrix value
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    # Get the underlying matrix value
    get <- function() x
    # Set the cached inverse value 
    setInverse <- function(inv) inverse <<- inv
    # Get the cached inverse value
    getInverse <- function() inverse
    # Return the vector of functions for manipulating our special matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function takes the matrix created by makeCacheMatrix()
## and returns the inverse from cache if already calculated 
## and if not it calculates and stores in cache before returning 

cacheSolve <- function(x, ...) {
    # Check the cache for the inverse
    inv <- x$getInverse()
    # If there, return it to caller
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Otherwise, retrieve the raw matrix and find the inverse
    data <- x$get()
    inv <- solve(data, ...)
    # Store in the cache
    x$setInverse(inv)
    # Return inverse to caller
    inv 
}