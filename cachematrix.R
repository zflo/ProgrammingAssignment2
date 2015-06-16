## Two functions to create, store, and manipulate a square matrix and its
## inverse.

## makeCacheMatrix(x) takes an invertible matrix x and returns a list of
## functions to set and get x itself and an associated 'inverse' object, storing
## both in the main function's environment when called.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve(s) takes a 'special object' s of the type of makeCacheMatrix's
## output and returns the associated matrix's inverse, either by getting it
## from s if available or else computing it itself.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}
